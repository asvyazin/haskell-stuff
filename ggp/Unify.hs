module Unify where

import Control.Monad
import qualified Data.Map as M
import Data.Maybe

import Types

apply :: Substitution -> Term -> Term
apply subst v@(V var) = fromMaybe v (M.lookup var subst)
apply subst (P prop) = P $ apply' subst prop
apply _ x = x

apply' :: Substitution -> Proposition -> Proposition
apply' subst (Proposition name terms) = Proposition name $ map (apply subst) terms

combineSubstitutions :: Substitution -> Substitution -> Substitution
combineSubstitutions s = M.union s . applyToSubstitution s

applyToSubstitution :: Substitution -> Substitution -> Substitution
applyToSubstitution s = M.map $ apply s

containsVar :: VariableName -> Term -> Bool
containsVar name (V name')
  | name' == name = True
  | otherwise = False
containsVar name (P (Proposition _ terms)) = any (containsVar name) terms
containsVar _ _ = False

unifyVar :: MonadPlus m => VariableName -> Term -> m Substitution
unifyVar name term
  | containsVar name term = mzero
  | otherwise = return $ M.fromList [(name, term)]

unify' :: MonadPlus m => [Term] -> [Term] -> m Substitution
unify' [] [] = return M.empty
unify' [] _ = mzero
unify' _ [] = mzero
unify' (t1 : terms1) (t2 : terms2) = do
  subst <- unify t1 t2
  let newTerms1 = map (apply subst) terms1
  let newTerms2 = map (apply subst) terms2
  subst' <- unify' newTerms1 newTerms2
  return $ combineSubstitutions subst' subst

unify :: MonadPlus m => Term -> Term -> m Substitution
unify (A a1) (A a2)
  | a1 == a2 = return M.empty
  | otherwise = mzero
unify (V name1) v2@(V name2)
  | name1 == name2 = return M.empty
  | otherwise = return $ M.fromList [(name1, v2)]
unify (V name) term = unifyVar name term
unify term (V name) = unifyVar name term
unify (P (Proposition name1 terms1)) (P (Proposition name2 terms2))
  | name1 /= name2 = mzero
  | otherwise = unify' terms1 terms2
unify _ _ = mzero
