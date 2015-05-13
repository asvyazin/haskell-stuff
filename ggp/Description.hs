{-# LANGUAGE OverloadedStrings #-}

module Description where

import qualified Data.ByteString.Lazy.Char8 as L
import Language.Sexp

import Types

fromProposition :: Proposition -> Sexp
fromProposition (Proposition name terms) = List (Atom name : map fromTerm terms)

fromTerm :: Term -> Sexp
fromTerm (A a) = Atom a
fromTerm (V v) = Atom $ "?" `L.append` v
fromTerm (P p) = fromProposition p
fromTerm (N p) = List [Atom "NOT", fromProposition p]

toProposition :: Sexp -> Proposition
toProposition (Atom name) = Proposition name []
toProposition (List sexps) = toProposition' sexps

toProposition' :: [Sexp] -> Proposition
toProposition' (Atom name : sexps) = Proposition name $ map toTerm sexps
toProposition' sexps = error $ "Invalid proposition: " ++ show sexps

toTerm :: Sexp -> Term
toTerm (Atom atom) | L.head atom == '?' = V (L.tail atom)
                   | otherwise = A atom
toTerm sexp = P $ toProposition sexp

toFact :: Sexp -> Fact
toFact (List (Atom "<=" : thenPart : ifPart)) = FactR $ Rule (toProposition thenPart) (map toRuleProposition ifPart)
  where toRuleProposition (List [Atom "NOT", List sexps]) = RuleN $ toProposition' sexps
        toRuleProposition sexp = RuleP $ toProposition sexp
toFact sexp = FactP $ toProposition sexp
