module Types where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

import CIByteString

type VariableName = Int
type Name = CIByteString
type Value = L.ByteString

data Term = A Value -- Atom
          | V VariableName -- Variable
          | P Proposition -- Proposition
          | N Proposition -- Negation
          deriving (Eq, Show, Ord)

data Proposition = Proposition Name [Term] deriving (Eq, Show, Ord)

data RuleProposition = RuleP Proposition -- Proposition
                     | RuleN Proposition -- Negation
                     deriving (Eq, Show)
                              
data Rule = Rule Proposition [RuleProposition] deriving (Eq, Show)

data Fact = FactP Proposition | FactR Rule deriving (Eq, Show)

type Substitution = M.Map VariableName Term
