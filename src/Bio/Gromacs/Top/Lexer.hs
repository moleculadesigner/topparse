{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

{-|
Module      : Bio.Gromacs.Top.Lexer
Description : Gromacs topology lexer
Copyright   : (c) Danila Iakovlev, 2023
License     : LGPL3
Maintainer  : moleculadesigner@gmail.com
Stability   : experimental
Portability : POSIX

Module contains simple Gromacs topology lexer with
no preprocessing on `grompp` directives
-}
module Bio.Gromacs.Top.Lexer
    ( lexTop
    )
where

import Prelude hiding (length)

import Data.Text.Lazy as T (pack, Text, length)
import Data.Maybe (fromJust)
import Data.Void
import Data.Foldable (Foldable(toList))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.Extra ((|:))

import Text.Megaparsec hiding (State, Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad (void)


-- | Basic Parsec type alias
type Parser = Parsec Void Text

{-  # Lexer

Splits the input stream to the tokens
-}

-- | A `Token` has an offset position and a type. 
data Token
    = Token
    { tokenOffset :: !Int
    , tokenType  :: TokenType
    } deriving (Eq, Ord)

instance Show Token where
    show :: Token -> String
    show (Token offset ttype) = show ttype ++ " -> " ++ show offset ++ "\n"

-- | `Token` types. End of file has its own token because we will need
-- to express when a valid program is allowed to end.
data TokenType
    = Comment Int       -- ^ "; Line comment with it`s length"
    | Eol               -- ^ '\n'
    | SectionOpening    -- ^ '['
    | SectionClosing    -- ^ ']'
    | Identifier Text   -- ^ Snake case alphanumeric
    | Numeral Text      -- ^ -123.45 | -123.45e03
    | Directive Text    -- ^ #include, #define, #ifdef, #endif...
                        -- https://manual.gromacs.org/current/onlinehelp/gmx-grompp.html#gmx-grompp
    | QuotedText Text   -- ^ "Quoted text" 
    | AngledText Text   -- ^ <Quoted text>
    | Eof               -- ^ End of input
    deriving (Eq, Ord, Show)

-- | `Token` length
tokenTypeSize :: TokenType -> Pos
tokenTypeSize t = mkPos $
    case t of
        Comment span   -> span + 1
        Eol            -> 1
        SectionOpening -> 1
        SectionClosing -> 1
        Identifier txt -> fromIntegral . T.length $ txt
        Numeral txt    -> fromIntegral . T.length $ txt
        Directive txt  -> (+ 1) . fromIntegral . T.length $ txt
        QuotedText txt -> (+ 2) . fromIntegral . T.length $ txt
        AngledText txt -> (+ 2) . fromIntegral . T.length $ txt
        Eof            -> 1

-- | Space Consumer
sc :: Parser ()
sc = L.space
    hspace1
    empty
    empty

-- | Combinator to easily add the current source offset to a `Token`
withOffset
    :: MonadParsec e s m
    => (Int -> m a)
    -> m a
withOffset f = getOffset >>= f


-- | Within-line spanned lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | A symbol helper
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- ## Parsers for each `Token` constructor
commentP :: Parser Token
commentP = withOffset $
    \pos -> Token pos <$> lexeme cmt
    where
        cmt = Comment 
            . fromIntegral 
            . length 
            . pack 
            <$>
                ( char ';' 
                *> many printChar 
                <* lookAhead (void eol <|> eof)
                <?> ";-Commented Text"
                )

eolP :: Parser Token
eolP = withOffset $
    \pos -> Token pos Eol <$ lexeme (eol <?> "End of line")

sectionOP :: Parser Token
sectionOP = withOffset $
    \pos -> Token pos SectionOpening <$ symbol "["

sectionCP :: Parser Token
sectionCP = withOffset $
    \pos -> Token pos SectionClosing <$ symbol "]"

identifierP :: Parser Token
identifierP = withOffset $
    \pos -> Token pos <$> lexeme idp
    where
        idp = Identifier . pack
            <$> some (alphaNumChar <|> char '_')
            <?> "Underscored Alphanum"

numeralP :: Parser Token
numeralP = withOffset $
    \pos -> Token pos <$> lexeme nmp
    where
        nmp = do
            leading <- optional (choice (map char "+-."))
                       <* lookAhead digitChar
                       <?> "Numeral leading symbol"
            rest <- some (digitChar <|> choice (map char "+-.eE"))
                    <?> "Numeral body symbols"
            return . Numeral . pack $ toList leading ++ rest

directiveP :: Parser Token
directiveP = withOffset $
    \pos -> Token pos <$> lexeme drp
    where
        drp = Directive . pack <$>
            ( char '#'
            *> some alphaNumChar
            <?> "#-direcive"
            )

quotedTextP :: Parser Token
quotedTextP = withOffset $
    \pos -> Token pos <$> lexeme qtx
    where
        qtx = QuotedText . pack <$>
            ( char '"'
            *> manyTill printChar (char '"')
            <?> "Quoted Text"
            )

angledTextP :: Parser Token
angledTextP = withOffset $
    \pos -> Token pos <$> lexeme atx
    where
        atx = AngledText . pack <$>
            ( char '<'
            *> manyTill printChar (char '>')
            <?> "Angled Text"
            )

eofP :: Parser Token
eofP = withOffset $
    \pos -> Token pos Eof <$ eof

tokenP :: Parser Token
tokenP = choice
  [ eolP
  , commentP
  , sectionOP
  , sectionCP
  , directiveP
  , quotedTextP
  , angledTextP
  , numeralP
  , identifierP
  ]

lexerP :: Parser (NonEmpty Token)
lexerP = do
    sc
    tokens <- many tokenP
    end <- eofP
    pure $ tokens |: end

lexTop :: Text -> Either (ParseErrorBundle Text Void) (NonEmpty Token)
lexTop = runParser lexerP ""