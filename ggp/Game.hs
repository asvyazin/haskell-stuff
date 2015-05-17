{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import Control.Monad.Logger
import Control.Monad.Random
import Control.Monad.State
import Data.ByteString.Lazy
import Data.Map
import Data.Text.Format
import qualified Data.Text.Lazy as L
import Language.Sexp

import CIByteString
import Database
import Description
import Types
import UniqueVariables

data Game = Game { gameId :: ByteString
                 , gameRole :: ByteString
                 , gameStartclock :: Int
                 , gamePlayclock :: Int
                 , gameDatabase :: Database }

fromAtom :: Term -> Value
fromAtom (A val) = val
fromAtom x = error $ "Not atom: " ++ show x

unwrapProposition :: Term -> Proposition
unwrapProposition (P p) = p
unwrapProposition x = error $ "Not proposition: " ++ show x

simpleProposition :: Name -> Database -> [Term]
simpleProposition name database =
  uniqL $ Prelude.map (! 0) $ matchQuery database (P (Proposition name [V 0]))

getRoles :: Database -> [ByteString]
getRoles = Prelude.map fromAtom . simpleProposition "role"

getInits :: Database -> [Proposition]
getInits = Prelude.map unwrapProposition . simpleProposition "init"

getNext :: Database -> [Proposition]
getNext = Prelude.map unwrapProposition . simpleProposition "next"

getLegalMoves :: Database -> ByteString -> [Proposition]
getLegalMoves database role =
  Prelude.map (unwrapMove . (! 0)) $ matchQuery database (P (Proposition "legal" [A role, V 0]))
  where unwrapMove (P p) = p
        unwrapMove (A a) = Proposition (toCI a) []
        unwrapMove x = error $ "Invalid move: " ++ show x

computeInits :: Monad m => StateT Database m ()
computeInits = get >>= (setDynamicFacts . getInits)

loadDatabase :: Monad m => [Sexp] -> StateT Database m ()
loadDatabase sexps = evalStateT (mapM toFact sexps) emptyRemapState >>= setFacts >> computeInits

modifyDatabase :: Monad m => (Database -> Database) -> StateT Game m ()
modifyDatabase func =
  modify $ \g -> let newD = func $ gameDatabase g
                 in g { gameDatabase = newD }

toGameState :: Monad m => StateT Database m r -> StateT Game m r
toGameState s = do
  g <- get
  (r, newD) <- lift $ runStateT s $ gameDatabase g
  put $ g { gameDatabase = newD }
  return r

initGame :: ByteString -> ByteString -> [Sexp] -> Int -> Int -> Game
initGame id_ role description startclock playclock =
  let database = execState (loadDatabase description) initDatabase
  in Game id_ role startclock playclock database

doPlay :: (MonadLogger m, MonadRandom m) => [Proposition] -> StateT Game m Proposition
doPlay moves = do
  roles <- liftM (getRoles . gameDatabase) get
  $(logDebug) $ L.toStrict $ "Moves: {}" `format` (Only $ Shown moves)
  case moves of
    [] -> return ()
    _ -> do
      toGameState $ setLastMoves $ Prelude.zip roles $ Prelude.map P moves
      nextFacts <- liftM (getNext . gameDatabase) get
      $(logDebug) $ L.toStrict $ "Next facts: {}" `format` (Only $ Shown nextFacts)
      modifyDatabase $ \d -> d { dynamicFacts = nextFacts }
  game <- get
  let legalMoves = getLegalMoves (gameDatabase game) (gameRole game)
  $(logDebug) $ L.toStrict $ "Legal moves: {}" `format` (Only $ Shown legalMoves)
  uniform legalMoves
