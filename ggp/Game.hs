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
fromAtom _ = error "Not atom"

unwrapProposition :: Term -> Proposition
unwrapProposition (P p) = p
unwrapProposition _ = error "Not proposition"

simpleProposition :: Name -> Database -> [Term]
simpleProposition name database =
  Prelude.map (! "x") $ matchQuery database (P (Proposition name [V "x"]))

getRoles :: Database -> [ByteString]
getRoles = Prelude.map fromAtom . simpleProposition "ROLE"

getInits :: Database -> [Proposition]
getInits = Prelude.map unwrapProposition . simpleProposition "INIT"

getNext :: Database -> [Proposition]
getNext = Prelude.map unwrapProposition . simpleProposition "NEXT"

getLegalMoves :: Database -> ByteString -> [Proposition]
getLegalMoves database role =
  Prelude.map (unwrapProposition . (! "x")) $ matchQuery database (P (Proposition "LEGAL" [A role, V "x"]))

computeInits :: Monad m => StateT Database m ()
computeInits = get >>= (setDynamicFacts . getInits)

loadDatabase :: Monad m => [Sexp] -> StateT Database m ()
loadDatabase sexps =
  let fs = remapFacts $ Prelude.map toFact sexps
  in setFacts fs >> computeInits

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
