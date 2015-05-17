{-# LANGUAGE OverloadedStrings #-}

module Database where

import Control.Monad.Logic
import Control.Monad.State
import qualified Data.Foldable as F
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

import Types
import Unify

data Database = Database { facts :: [Fact]
                         , doesTerms :: [Term]
                         , trueTerms :: [Term] }
                deriving (Eq, Show)

setFacts :: Monad m => [Fact] -> StateT Database m ()
setFacts fs = modify $ \s -> s { facts = fs }

setDoesTerms :: Monad m => [Term] -> StateT Database m ()
setDoesTerms ts = modify $ \s -> s { doesTerms = ts }

setTrueTerms :: Monad m => [Term] -> StateT Database m ()
setTrueTerms ts = modify $ \s -> s { trueTerms = ts }

initDatabase :: Database
initDatabase = Database [] [] []

matchQuery :: Database -> Term -> [Substitution]
matchQuery database = uniqL . observeAll . doMatchQuery database

doMatchQuery :: MonadLogic m => Database -> Term -> m Substitution
doMatchQuery database query = case query of
    (P (Proposition name args))
      | name == "true" -> toMonadPlus (trueTerms database) >>= unify query
      | name == "does" -> toMonadPlus (doesTerms database) >>= unify query
      | name == "distinct" -> case args of
        [t1, t2]
          | t1 /= t2 -> return M.empty
          | otherwise -> mzero
        _ -> mzero
      | name == "not" -> case args of
        [t] -> lnot (once (doMatchQuery database t)) >> return M.empty
        _ -> mzero
    _ -> tryMatchFacts database query

tryMatchFacts :: MonadLogic m => Database -> Term -> m Substitution
tryMatchFacts database query = do
  fact <- toMonadPlus $ facts database
  matchFact database query fact

matchFact :: MonadLogic m => Database -> Term -> Fact -> m Substitution
matchFact _ query (FactP prop) = unify (P prop) query
matchFact database query (FactR (Rule thenPart ifPart)) = do
  subst <- unify (P thenPart) query
  foldM (\ s p ->
          let p1 = applyToRule s p
          in do
            s' <- case p1 of
              RuleP p2 -> doMatchQuery database (P p2)
              RuleN p2 -> lnot (once (doMatchQuery database (P p2))) >> return M.empty
            return $ combineSubstitutions s' s) subst ifPart
    where applyToRule subst (RuleP p) = RuleP $ apply' subst p
          applyToRule subst (RuleN p) = RuleN $ apply' subst p

toMonadPlus :: (F.Foldable t, MonadPlus m) => t a -> m a
toMonadPlus = F.foldl (\x y -> x `mplus` return y) mzero

uniqL :: Ord a => [a] -> [a]
uniqL = reverse . fst . foldl' (\ (prevL, prevS) x ->
                                 let nextL = if S.member x prevS then prevL else x : prevL
                                 in (nextL, S.insert x prevS)) ([], S.empty)
