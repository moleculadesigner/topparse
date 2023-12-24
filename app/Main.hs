{-# LANGUAGE OverloadedStrings #-}
module Main where

import Bio.Gromacs.Top.Lexer (lexTop)
import Data.Text.Lazy as T (Text, pack)

main :: IO ()
main = do
    contents <- readFile "/Users/iakovlevda/work/top-hs/topparse/test/sections.top"
    putStrLn "\n\nRun Program\n\n"
    print $ lexTop $ pack contents