{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.IO.Class
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

logConduit :: Show a => Conduit a IO a
logConduit = awaitForever $ \x -> do
  liftIO $ print x
  yield x

readMessage :: (MonadIO m, MonadThrow m) => Request -> Source m Message
readMessage req = sourceRequestBody req =$= conduitParser sexpParser =$= L.map (toMessage . snd)

processMessage :: Message -> TMVar Game -> STM Sexp
processMessage Info _ = return $ Atom "available"
processMessage (Preview _ _) _ = undefined
processMessage (Start id_ role description startclock playclock) var = let database = execState (loadDatabase description) initDatabase
                                                                           game = Game id_ role startclock playclock database
                                                                       in do putTMVar var game
                                                                             return $ Atom "ready"
processMessage (Play _ _) _ = undefined
processMessage (Stop _ _) _ = undefined
processMessage (Abort _) _ = undefined

app :: TMVar Game -> Application
app var req respond = do
  msg <- fromJust <$> runConduit (readMessage req =$= L.head)
  Prelude.putStrLn $ "received " ++ show msg
  response <- atomically $ processMessage msg var
  game <- atomically $ readTMVar var
  Prelude.putStrLn $ "roles: " ++ show (roles game)
  Prelude.putStrLn $ "init: " ++ show (inits game)
  respond $ responseLBS
    status200
    [("Content-Type", "text/acl")]
    (printMach response)

main :: IO ()
main = do
  game <- atomically newEmptyTMVar
  run 9147 $ app game
