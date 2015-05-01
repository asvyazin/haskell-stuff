{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Protocol where

import Data.ByteString.Lazy.Char8
import Data.Sexp

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
toMessage (List [Atom "INFO"]) = Info
toMessage (List [Atom "PREVIEW", List description, Atom previewclock]) =
  Preview description $ toInt previewclock
toMessage (List [Atom "START", Atom id_, Atom role, List description, Atom startclock, Atom playclock]) = Start id_ role description (toInt startclock) (toInt playclock)
toMessage (List [Atom "PLAY", Atom id_, List move]) = Play id_ move
toMessage (List [Atom "STOP", Atom id_, List move]) = Stop id_ move
toMessage (List [Atom "ABORT", Atom id_]) = Abort id_
toMessage _ = undefined
