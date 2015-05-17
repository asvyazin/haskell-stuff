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

getLegalMoves :: Database -> ByteString -> [Move]
getLegalMoves database role =
  Prelude.map (unwrapMove . (! 0)) $ matchQuery database (P (Proposition "legal" [A role, V 0]))
  where unwrapMove (P p) = MoveP p
        unwrapMove (A a) = MoveA a
        unwrapMove x = error $ "Invalid move: " ++ show x

computeInits :: Monad m => StateT Database m ()
computeInits = gets (Prelude.map toTrueTerm . getInits) >>= setTrueTerms

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

doPlay :: (MonadLogger m, MonadRandom m) => [Move] -> StateT Game m Move
doPlay moves = do
  roles <- gets (getRoles . gameDatabase)
  $(logDebug) $ L.toStrict $ "Moves: {}" `format` (Only $ Shown moves)
  case moves of
    [] -> return ()
    _ -> do
      let does = Prelude.zipWith toDoesTerm roles moves
      $(logDebug) $ L.toStrict $ "Does: {}" `format` (Only $ Shown does)
      toGameState $ setDoesTerms does
      true <- gets (Prelude.map toTrueTerm . getNext . gameDatabase)
      $(logDebug) $ L.toStrict $ "True: {}" `format` (Only $ Shown true)
      toGameState $ setTrueTerms true
  game <- get
  let legalMoves = getLegalMoves (gameDatabase game) (gameRole game)
  $(logDebug) $ L.toStrict $ "Legal moves: {}" `format` (Only $ Shown legalMoves)
  uniform legalMoves

toDoesTerm :: ByteString -> Move -> Term
toDoesTerm role move =
  let moveTerm = case move of
        MoveP p -> P p
        MoveA a -> A a
  in P (Proposition "does" [A role, moveTerm])

toTrueTerm :: Proposition -> Term
toTrueTerm p = P (Proposition "true" [P p])
