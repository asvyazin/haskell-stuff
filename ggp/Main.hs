{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.Attoparsec
import qualified Data.Conduit.List as L
import Data.Maybe
import Data.Sexp
import Language.Sexp.Parser
import Language.Sexp.Printer
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Conduit
import Network.Wai.Handler.Warp (run)

import Description
import Game
import Protocol

logConduit :: (MonadIO m, Show a) => Conduit a m a
logConduit = awaitForever $ \x -> do
  liftIO $ print x
  yield x

readMessage :: (MonadIO m, MonadThrow m) => Request -> Source m Message
readMessage req = sourceRequestBody req =$= conduitParser sexpParser =$= L.map (toMessage . snd)

processMessage :: (MonadIO m, MonadLogger m) => Message -> TMVar Game -> m Sexp
processMessage Info _ = return $ Atom "available"
processMessage (Preview _ _) _ = undefined
processMessage (Start id_ role description startclock playclock) var = let game = initGame id_ role description startclock playclock
                                                                       in do liftIO $ atomically $ putTMVar var game
                                                                             return $ Atom "ready"
processMessage (Play _ moves) var = do
  game <- liftIO $ atomically $ takeTMVar var
  (move, newGame) <- runStateT (doPlay (map toProposition moves)) game
  liftIO $ atomically $ putTMVar var newGame
  return $ fromProposition move
processMessage (Stop _ _) _ = return $ Atom "done"
processMessage (Abort _) _ = return $ Atom "done"

app :: TMVar Game -> Application
app var req respond = do
  msg <- fromJust <$> runConduit (readMessage req =$= L.head)
  response <- runStderrLoggingT $ processMessage msg var
  respond $ responseLBS
    status200
    [("Content-Type", "text/acl")]
    (printMach response)

main :: IO ()
main = do
  game <- atomically newEmptyTMVar
  run 9147 $ app game
