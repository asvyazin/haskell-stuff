{-# LANGUAGE OverloadedStrings #-}

module UniqueVariables where

import Control.Monad.State
import qualified Data.Map as M

import Types

data RemapState = RemapState { currentVariable :: VariableName
                             , currentMap :: M.Map Name VariableName }
                deriving (Eq, Show)

emptyRemapState :: RemapState
emptyRemapState = RemapState 10 M.empty

getNextVariable :: Monad m => StateT RemapState m VariableName
getNextVariable = do
  s <- get
  let var = currentVariable s
  put $ s { currentVariable = var + 1 }
  return var

addMapping :: Monad m => Name -> VariableName -> StateT RemapState m ()
addMapping from to = modify $ \s -> s { currentMap = M.insert from to (currentMap s) }

remapVariable :: Monad m => Name -> StateT RemapState m VariableName
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
