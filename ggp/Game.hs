{-# LANGUAGE OverloadedStrings #-}

module Game where

import Data.ByteString.Lazy
import Data.Map

import Description

data Game = Game { gameId :: ByteString
                 , gameRole :: ByteString
                 , gameStartclock :: Int
                 , gamePlayclock :: Int
                 , gameDatabase :: Database }

fromAtom :: Term -> Value
fromAtom (A val) = val
fromAtom _ = error "Not atom"

simpleProposition :: Name -> Game -> [Term]
simpleProposition name = Prelude.map (! "x") . matchQuery (P (Proposition name [V "x"])) . gameDatabase

roles :: Game -> [ByteString]
roles = Prelude.map fromAtom . simpleProposition "ROLE"

inits :: Game -> [Term]
inits = simpleProposition "INIT"
