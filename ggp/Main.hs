{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad.IO.Class
import Data.ByteString.Lazy
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

import Protocol

logConduit :: Show a => Conduit a IO a
logConduit = awaitForever $ \x -> do
  liftIO $ print x
  yield x

readMessage :: Request -> Source IO Message
readMessage req = sourceRequestBody req =$= conduitParser sexpParser =$= L.map (toMessage . snd)

processInfo :: Sexp
processInfo = Atom "available"

processPreview :: [Sexp] -> Int -> Sexp
processPreview = undefined

processStart :: ByteString -> ByteString -> [Sexp] -> Int -> Int -> Sexp
processStart = undefined

processPlay :: ByteString -> [Sexp] -> Sexp
processPlay = undefined

processStop :: ByteString -> [Sexp] -> Sexp
processStop = undefined

processAbort :: ByteString -> Sexp
processAbort = undefined

app :: Application
app req respond = do
  msg <- fromJust <$> runConduit (readMessage req =$= L.head)
  Prelude.putStrLn $ "received " ++ show msg
  let response = case msg of
        Info -> processInfo
        Preview description previewclock -> processPreview description previewclock
        Start id_ role description startclock playclock -> processStart id_ role description startclock playclock
        Play id_ move -> processPlay id_ move
        Stop id_ move -> processStop id_ move
        Abort id_ -> processAbort id_
  respond $ responseLBS
    status200
    [("Content-Type", "text/acl")]
    (printMach response)

main :: IO ()
main = run 9147 app
