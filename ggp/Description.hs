{-# LANGUAGE OverloadedStrings #-}

module Description where

import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as L
import Language.Sexp

import CIByteString
import Types
import UniqueVariables

fromProposition :: Proposition -> Sexp
fromProposition (Proposition name terms) = List (Atom (toBS name) : map fromTerm terms)

fromTerm :: Term -> Sexp
fromTerm (A a) = Atom a
fromTerm (V v) = Atom $ "?v" `L.append` L.pack (show v)
fromTerm (P p) = fromProposition p
fromTerm (N p) = List [Atom "not", fromProposition p]

toProposition :: Monad m => Sexp -> (Name -> m VariableName) -> m Proposition
toProposition (Atom name) _ = return $ Proposition (toCI name) []
toProposition (List (Atom name : sexps)) remapVar =
  liftM (Proposition (toCI name)) (mapM (`toTerm` remapVar) sexps)
toProposition sexps _ = fail $ "Invalid proposition: " ++ show sexps

toTerm :: Monad m => Sexp -> (Name -> m VariableName) -> m Term
toTerm (Atom atom) remapVar
  | L.head atom == '?' = liftM V $ remapVar $ toCI $ L.tail atom
  | otherwise = return $ A atom
toTerm sexp remapVar = liftM P $ toProposition sexp remapVar

toFact :: Monad m => Sexp -> StateT RemapState m Fact
toFact sexp = enterNewFact >> case sexp of
  (List (Atom "<=" : thenPart : ifPart)) ->
    liftM FactR $ liftM2 Rule (toProposition thenPart remapVariable) (mapM toRuleProposition ifPart)
    where toRuleProposition (List [Atom notStr, sexps])
            | toCI notStr == "not" = liftM RuleN $ toProposition sexps remapVariable
          toRuleProposition sexp1 = liftM RuleP $ toProposition sexp1 remapVariable
  _ -> liftM FactP $ toProposition sexp remapVariable
