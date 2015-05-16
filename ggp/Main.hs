{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.ByteString.Lazy as B
import qualified Data.Conduit.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.Sexp
import Language.Sexp.Parser
import Language.Sexp.Printer
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Conduit
import Network.Wai.Handler.Warp (run)
import System.Environment

import Description
import Game
import Protocol

type Games = M.Map B.ByteString Game

setGame :: TMVar Games -> B.ByteString -> Game -> STM ()
setGame var id_ game = takeTMVar var >>= (putTMVar var . M.insert id_ game)

getGame :: TMVar Games -> B.ByteString -> STM Game
getGame var id_ = do
  games <- readTMVar var
  return $ games M.! id_

deleteGame :: TMVar Games -> B.ByteString -> STM ()
deleteGame var id_ = takeTMVar var >>= (putTMVar var . M.delete id_)

logConduit :: (MonadIO m, Show a) => Conduit a m a
logConduit = awaitForever $ \x -> do
  liftIO $ print x
  yield x

readMessage :: (MonadIO m, MonadThrow m) => Request -> Source m Message
readMessage req = sourceRequestBody req =$= logConduit =$= conduitParser sexpParser =$= L.map (toMessage . snd)

processMessage :: (MonadIO m, MonadLogger m, MonadRandom m) => Message -> TMVar Games -> m Sexp
processMessage Info _ = return $ Atom "available"
processMessage (Preview _ _) _ = undefined
processMessage (Start id_ role description startclock playclock) var =
  let game = initGame id_ role description startclock playclock
  in do liftIO $ atomically $ setGame var id_ game
        return $ Atom "ready"
processMessage (Play id_ moves) var = do
  game <- liftIO $ atomically $ getGame var id_
  (move, newGame) <- runStateT (doPlay (map toProposition moves)) game
  liftIO $ atomically $ setGame var id_ newGame
  return $ fromProposition move
processMessage (Stop id_ _) var = do
  liftIO $ atomically $ deleteGame var id_
  return $ Atom "done"
processMessage (Abort id_) var = do
  liftIO $ atomically $ deleteGame var id_
  return $ Atom "done"

instance MonadRandom m => MonadRandom (LoggingT m) where
  getRandom = lift getRandom
  getRandomR = lift . getRandomR
  getRandoms = lift getRandoms
  getRandomRs = lift . getRandomRs
                  
app :: TMVar Games -> Application
app var req respond = do
  msg <- fromJust <$> runConduit (readMessage req =$= L.head)
  response <- runStderrLoggingT $ processMessage msg var
  respond $ responseLBS
    status200
    [("Content-Type", "text/acl"),
     ("Access-Control-Allow-Origin", "*")]
    (printMach response)

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        [portStr] -> read portStr
        _ -> 9147
  games <- atomically $ newTMVar M.empty
  run port $ app games
