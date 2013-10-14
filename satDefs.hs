-- | SAT - Tester Definitions
-- | find out if a set of prop. logical sentences is satisfiable (that is: is there a assignment of truth-values such that all sentences are true?)
module SatDefs
    (Variable, Sentence(..), TruthAssignment, System
    ) where

import Data.Map.Strict (Map)
type Variable = String

data Sentence
    = TrueS
    | FalseS
    | VarS  Variable
    | NotS  Sentence
    | AndS  Sentence Sentence
    | OrS   Sentence Sentence
    | ImplS Sentence Sentence
    | EqS   Sentence Sentence
    deriving (Show, Ord, Eq)

type TruthAssignment = Map Variable Bool
type System = [Sentence]