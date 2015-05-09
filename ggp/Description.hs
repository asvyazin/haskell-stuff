{-# LANGUAGE OverloadedStrings #-}

module Description where

import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import Data.Maybe
import Language.Sexp

type Name = L.ByteString
type Value = L.ByteString

data Term = A Value -- Atom
          | V Name -- Variable
          | P Proposition -- Proposition
          deriving (Eq, Show)

data Proposition = Proposition Name [Term] deriving (Eq, Show)

data RuleProposition = RuleP Proposition -- Proposition
                     | RuleN Proposition -- Negation
                     deriving (Eq, Show)
                              
data Rule = Rule Proposition [RuleProposition] deriving (Eq, Show)

data Fact = FactP Proposition | FactR Rule deriving (Eq, Show)

type Substitution = M.Map Name Term

data Database = Database { facts :: [Fact]
                         , customPropositions :: M.Map Name ([Term] -> Maybe [Substitution]) }

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

addFact :: Fact -> State Database ()
addFact f = modify $ \s -> s { facts = f : facts s }

initDatabase :: Database
initDatabase = Database [] $ M.fromList [("DISTINCT", distinct)]
  where distinct [t1, t2] | t1 /= t2 = Just []
                          | otherwise = Nothing
        distinct _ = error "Invalid arguments"

loadDatabase :: [Sexp] -> State Database ()
loadDatabase = mapM_ (addFact . toFact)

containsVar :: Name -> Term -> Bool
containsVar name (V name') | name' == name = True
                           | otherwise = False
containsVar name (P (Proposition _ terms)) = any (containsVar name) terms
containsVar _ _ = False

unifyVar :: MonadPlus m => Name -> Term -> m Substitution
unifyVar name term | containsVar name term = mzero
                   | otherwise = return $ M.fromList [(name, term)]

apply :: Substitution -> Term -> Term
apply subst v@(V var) = fromMaybe v (M.lookup var subst)
apply subst (P prop) = P $ apply' subst prop
apply _ x = x

apply' :: Substitution -> Proposition -> Proposition
apply' subst (Proposition name terms) = Proposition name $ map (apply subst) terms

unify' :: MonadPlus m => [Term] -> [Term] -> m Substitution
unify' [] [] = return M.empty
unify' [] _ = mzero
unify' _ [] = mzero
unify' (t1 : terms1) (t2 : terms2) = do
  subst <- unify t1 t2
  let newTerms1 = map (apply subst) terms1
  let newTerms2 = map (apply subst) terms2
  subst' <- unify' newTerms1 newTerms2
  return $ M.union subst subst'

unify :: MonadPlus m => Term -> Term -> m Substitution
unify (A a1) (A a2) | a1 == a2 = return M.empty
                    | otherwise = mzero
unify (V name1) v2@(V name2) | name1 == name2 = return M.empty
                             | otherwise = return $ M.fromList [(name1, v2)]
unify (V name) term = unifyVar name term
unify term (V name) = unifyVar name term
unify (P (Proposition name1 terms1)) (P (Proposition name2 terms2)) | name1 /= name2 = mzero
                                                                    | otherwise = unify' terms1 terms2
unify _ _ = mzero

toMonadPlus :: MonadPlus m => [a] -> m a
toMonadPlus = msum . map return

matchQuery :: Term -> Database -> [Substitution]
matchQuery query database = observeAll $ doMatchQuery query database

doMatchQuery :: MonadLogic m => Term -> Database -> m Substitution
doMatchQuery query database = tryMatchCustomProposition query database
                            `mplus`
                            tryMatchFacts query database

tryMatchFacts :: MonadLogic m => Term -> Database -> m Substitution
tryMatchFacts query database = do
  fact <- toMonadPlus $ facts database
  matchFact fact query database  

tryMatchCustomProposition :: MonadPlus m => Term -> Database -> m Substitution
tryMatchCustomProposition (P (Proposition name args)) database = case M.lookup name (customPropositions database) of
  Nothing -> mzero
  Just func -> maybe mzero toMonadPlus (func args)
tryMatchCustomProposition _ _ = mzero

matchFact :: MonadLogic m => Fact -> Term -> Database -> m Substitution
matchFact (FactP prop) query _ = unify (P prop) query
matchFact (FactR (Rule thenPart ifPart)) query database = do
  subst <- unify (P thenPart) query
  subprop <- liftM (applyToRule subst) (toMonadPlus ifPart)
  case subprop of
    RuleP p -> do
      subst' <- doMatchQuery (P p) database
      return $ M.union subst subst'
    RuleN p -> do
      lnot $ once $ doMatchQuery (P p) database
      return subst
    where applyToRule subst (RuleP p) = RuleP $ apply' subst p
          applyToRule subst (RuleN p) = RuleN $ apply' subst p
