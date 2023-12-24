{-# LANGUAGE OverloadedStrings #-}
module Bio.Section
    ( sectionHeaderFromName
    , SectionHeader
    , headless
    , sectionStub
    , Section
    )
where

import qualified Data.Map as M
import qualified Data.Text as T

{- The reference on *.top file format is in Gromacs documentation
   https://manual.gromacs.org/current/reference-manual/topologies/topology-file-formats.html
-}

data Section = Section
    { header :: SectionHeader
    , entries :: [T.Text]
    } deriving (Show)

data SectionHeader =
    -- | Parameters
    Headless           -- Headless part of the file
    | Unknown          -- section with empty or unknown header
    | Defaults         -- defaults
    | Atomtypes        -- atomtypes
    | Bondtypes        -- bondtypes
    | Pairtypes        -- pairtypes
    | Angletypes       -- angletypes
    | Dihedraltypes    -- dihedraltypes
    | Constrainttypes  -- constrainttypes
    | NonbondParams    -- nonbond_params
    -- | Molecule Definition(s)
    | Moleculetype     -- moleculetype
    | Atoms            -- atoms
    -- | System
    | System           -- system
    | Molecules        -- molecules
    -- | Inter-molecular interactions
    | IntermolecularInteractions  -- intermolecular_interactions
    -- | Molecule Type directives
    | Bonds            -- bonds
    | Pairs            -- pairs
    | PairsNB          -- pairs_nb
    | Angles           -- angles
    | Dihedrals        -- dihedrals
    | Exclusions       -- exclusions
    | Constraints      -- constraints
    | Settles          -- settles
    | VirtualSites1    -- virtual_sites1
    | VirtualSites2    -- virtual_sites2
    | VirtualSites3    -- virtual_sites3
    | VirtualSites4    -- virtual_sites4
    | VirtualSitesN    -- virtual_sitesn
    -- | Restraints
    | PositionRestraints     -- position_restraints
    | DistanceRestraints     -- distance_restraints
    | DihedralRestraints     -- dihedral_restraints
    | OrientationRestraints  -- orientation_restraints
    | AngleRestraints        -- angle_restraints
    | AngleRestraintsZ       -- angle_restraints_z
    deriving (Show, Eq)


sectionHeaderMap :: M.Map T.Text SectionHeader
sectionHeaderMap = M.fromList
    [ ("defaults"                    , Defaults)
    , ("atomtypes"                   , Atomtypes)
    , ("bondtypes"                   , Bondtypes)
    , ("pairtypes"                   , Pairtypes)
    , ("angletypes"                  , Angletypes)
    , ("dihedraltypes"               , Dihedraltypes)
    , ("constrainttypes"             , Constrainttypes)
    , ("nonbond_params"              , NonbondParams)
    , ("moleculetype"                , Moleculetype)
    , ("atoms"                       , Atoms)
    , ("system"                      , System)
    , ("molecules"                   , Molecules)
    , ("intermolecular_interactions" , IntermolecularInteractions)
    , ("bonds"                       , Bonds)
    , ("pairs"                       , Pairs)
    , ("pairs_nb"                    , PairsNB)
    , ("angles"                      , Angles)
    , ("dihedrals"                   , Dihedrals)
    , ("exclusions"                  , Exclusions)
    , ("constraints"                 , Constraints)
    , ("settles"                     , Settles)
    , ("virtual_sites1"              , VirtualSites1)
    , ("virtual_sites2"              , VirtualSites2)
    , ("virtual_sites3"              , VirtualSites3)
    , ("virtual_sites4"              , VirtualSites4)
    , ("virtual_sitesn"              , VirtualSitesN)
    , ("position_restraints"         , PositionRestraints)
    , ("distance_restraints"         , DistanceRestraints)
    , ("dihedral_restraints"         , DihedralRestraints)
    , ("orientation_restraints"      , OrientationRestraints)
    , ("angle_restraints"            , AngleRestraints)
    , ("angle_restraints_z"          , AngleRestraintsZ)
    ]

sectionHeaderFromName :: T.Text -> SectionHeader
sectionHeaderFromName t = 
    let sh = M.lookup t sectionHeaderMap
    in case sh
    of
        (Just header) -> header
        Nothing -> Unknown

headless :: SectionHeader
headless = Headless

section :: T.Text
        -- ^ Section Header
        -> [T.Text]
        -- ^ Entries
        -> Section
section h ents = Section {header = sectionHeaderFromName h, entries = ents}

sectionStub :: T.Text -> Section
sectionStub h = section h []