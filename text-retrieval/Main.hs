{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import Data.Text.ICU
import NLP.Snowball

defaultLocale :: LocaleName
defaultLocale = Locale "en-US"

runBreaker :: Breaker a -> Text -> [Text]
runBreaker brk = map brkBreak . breaks brk

sentences :: Text -> [Text]
sentences = runBreaker $ breakSentence defaultLocale

myWords :: Text -> [Text]
myWords = runBreaker $ breakWord defaultLocale

lowercase :: Text -> Text
lowercase = toLower defaultLocale

phrase :: Text
phrase = "A General Game Player is a system that can play previously unknown games given nothing but their rules. The Game Description Language (GDL) has been developed as a highlevel knowledge representation formalism for axiomatising the rules of any game, and a basic requirement of a General Game Player is the ability to reason logically about a given game description. In this paper, we address the fundamental limitation of existing GDL to be conﬁned to deterministic games with complete information about the game state. To this end, we develop an extension of GDL that is both simple and elegant yet expressive enough to allow to formalise the rules of arbitrary (discrete and ﬁnite) n-player games with randomness and incomplete state knowledge. We also show that this extension sufﬁces to provide players with all information they need to reason about their own knowledge as well as that of the other players up front and during game play."

main :: IO ()
main = print $ map (stems English . map lowercase . myWords) $ sentences phrase
