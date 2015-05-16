{-# LANGUAGE OverloadedStrings #-}

module Protocol where

import Data.ByteString.Lazy.Char8
import Data.Sexp

import CIByteString

data Message = Info
             | Preview [Sexp] Int
             | Start ByteString ByteString [Sexp] Int Int
             | Play ByteString [Sexp]
             | Stop ByteString [Sexp]
             | Abort ByteString
             deriving (Eq, Show)

toInt :: ByteString -> Int
toInt = read . unpack

toMessage :: Sexp -> Message
toMessage (List (Atom name : args)) = toMessage' (toCI name) args
  where toMessage' "info" [] = Info
        toMessage' "preview" [List description, Atom previewclock] =
          Preview description $ toInt previewclock
        toMessage' "start" [Atom id_, Atom role, List description, Atom startclock, Atom playclock] =
          Start id_ role description (toInt startclock) (toInt playclock)
        toMessage' "play" [Atom id_, Atom nil] | toCI nil == "nil" = Play id_ []
        toMessage' "play" [Atom id_, List move] = Play id_ move
        toMessage' "stop" [Atom id_, List move] = Stop id_ move
        toMessage' "abort" [Atom id_] = Abort id_
        toMessage' _ _ = undefined
toMessage _ = undefined
