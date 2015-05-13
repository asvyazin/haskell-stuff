{-# LANGUAGE OverloadedStrings #-}

module UniqueVariables where

import Control.Monad.State
import Data.ByteString.Lazy.Char8
import qualified Data.Map as M

import Types

data RemapState = RemapState { currentVariable :: Int
                             , currentMap :: M.Map Name Name }
                deriving (Eq, Show)

emptyRemapState :: RemapState
emptyRemapState = RemapState 0 M.empty

getNextVariable :: Monad m => StateT RemapState m Name
getNextVariable = do
  s <- get
  let var = currentVariable s
  put $ s { currentVariable = var + 1 }
  return $ pack $ "v" ++ show var

addMapping :: Monad m => Name -> Name -> StateT RemapState m ()
addMapping from to = modify $ \s -> s { currentMap = M.insert from to (currentMap s) }

remapVariable :: Monad m => Name -> StateT RemapState m Name
remapVariable name = do
  s <- get
  case M.lookup name (currentMap s) of
    Just newName -> return newName
    Nothing -> do
      newName <- getNextVariable
      addMapping name newName
      return newName

enterNewFact :: Monad m => StateT RemapState m ()
enterNewFact = modify $ \s -> s { currentMap = M.empty }

remapFacts :: [Fact] -> [Fact]
remapFacts fs = evalState (mapM remapFact fs) emptyRemapState

remapFact :: Monad m => Fact -> StateT RemapState m Fact
remapFact fact = enterNewFact >> remapFact'
  where remapFact' = case fact of
          (FactP prop) -> liftM FactP $ remapProposition prop
          (FactR rule) -> liftM FactR $ remapRule rule

remapRule :: Monad m => Rule -> StateT RemapState m Rule
remapRule (Rule thenPart ifParts) = liftM2 Rule (remapProposition thenPart) (mapM remapRuleProposition ifParts)

remapProposition :: Monad m => Proposition -> StateT RemapState m Proposition
remapProposition (Proposition name terms) = liftM (Proposition name) (mapM remapTerm terms)

remapRuleProposition :: Monad m => RuleProposition -> StateT RemapState m RuleProposition
remapRuleProposition (RuleP prop) = liftM RuleP $ remapProposition prop
remapRuleProposition (RuleN prop) = liftM RuleN $ remapProposition prop

remapTerm :: Monad m => Term -> StateT RemapState m Term
remapTerm t@(A _) = return t
remapTerm (V v) = liftM V $ remapVariable v
remapTerm (P p) = liftM P $ remapProposition p
remapTerm (N p) = liftM N $ remapProposition p
