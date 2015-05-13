{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Monad.Logic
import Control.Monad.State
import qualified Data.Map as M

import Types
import Unify

data Database = Database { facts :: [Fact]
                         , dynamicFacts :: [Proposition]
                         , lastMoves :: M.Map Name Term
                         , customPropositions :: M.Map Name (Database -> [Term] -> [Substitution]) }

setFacts :: Monad m => [Fact] -> StateT Database m ()
setFacts fs = modify $ \s -> s { facts = fs }

addDynamicFact :: Monad m => Proposition -> StateT Database m ()
addDynamicFact p = modify $ \s -> s { dynamicFacts = p : dynamicFacts s }

setDynamicFacts :: Monad m => [Proposition] -> StateT Database m ()
setDynamicFacts p = modify $ \s -> s { dynamicFacts = p }

setLastMoves :: Monad m => [(Name, Term)] -> StateT Database m ()
setLastMoves pairs = modify $ \s -> s { lastMoves = M.fromList pairs }

initDatabase :: Database
initDatabase = Database [] [] M.empty $
               M.fromList [("DISTINCT", ggpDistinct), ("TRUE", ggpTrue), ("DOES", ggpDoes)]
  where ggpDistinct _ [t1, t2] | t1 /= t2 = [M.empty]
                               | otherwise = []
        ggpDistinct _ _ = error "Distinct. Invalid arguments"
        ggpTrue d [t] = observeAll $ tryMatchDynamicFacts (toMonadPlus $ dynamicFacts d) t
        ggpTrue _ _ = error "True. Invalid arguments"
        ggpDoes d [A role, termMove] = case M.lookup role (lastMoves d) of
          Nothing -> mzero
          Just lastMove -> unify termMove lastMove
        ggpDoes _ _ = error "Does. Invalid arguments"

matchQuery :: Database -> Term -> [Substitution]
matchQuery database = observeAll . doMatchQuery database

doMatchQuery :: MonadLogic m => Database -> Term -> m Substitution
doMatchQuery database query = tryMatchCustomProposition database query
                            `mplus`
                            tryMatchFacts database query

tryMatchFacts :: MonadLogic m => Database -> Term -> m Substitution
tryMatchFacts database query = do
  fact <- toMonadPlus $ facts database
  matchFact database query fact

tryMatchDynamicFacts :: MonadLogic m => m Proposition -> Term -> m Substitution
tryMatchDynamicFacts fs t@(P _) = fs >>= (unify t . P)
tryMatchDynamicFacts fs (N p) = fs >>=
                                (lnot . once . unify (P p) . P) >>
                                return M.empty
tryMatchDynamicFacts _ _ = mzero

tryMatchCustomProposition :: MonadPlus m => Database -> Term -> m Substitution
tryMatchCustomProposition database (P (Proposition name args)) = case M.lookup name (customPropositions database) of
  Nothing -> mzero
  Just func -> toMonadPlus (func database args)
tryMatchCustomProposition _ _ = mzero

matchFact :: MonadLogic m => Database -> Term -> Fact -> m Substitution
matchFact _ query (FactP prop) = unify (P prop) query
matchFact database query (FactR (Rule thenPart ifPart)) = do
  subst <- unify (P thenPart) query
  foldM (\ s p -> let p1 = applyToRule s p
                  in do
                    s' <- case p1 of
                      RuleP p2 -> doMatchQuery database (P p2)
                      RuleN p2 -> lnot (once $ doMatchQuery database (P p2)) >> return M.empty
                    return $ combineSubstitutions s' s) subst ifPart
    where applyToRule subst (RuleP p) = RuleP $ apply' subst p
          applyToRule subst (RuleN p) = RuleN $ apply' subst p

toMonadPlus :: MonadPlus m => [a] -> m a
toMonadPlus = msum . map return
