module Main where

import Prelude

import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.List (List, length, reverse, (:), take)
import Data.List as List
import Effect (Effect)
import Effect.Console (log)
import Effect.Random (randomBool)

data Dodosuko = Dodo | Suko

derive instance Eq Dodosuko
instance showDodosuko :: Show Dodosuko where
  show Dodo = "ドド"
  show Suko = "スコ"

ranDodosuko :: Effect Dodosuko
ranDodosuko = randomBool <#> if _ then Dodo else Suko

endodosukoList :: List Dodosuko
endodosukoList = reverse $ List.fromFoldable $ [ Dodo, Suko, Suko, Suko, Dodo, Suko, Suko, Suko, Dodo, Suko, Suko, Suko ]

endodosukoListLength :: Int
endodosukoListLength = length endodosukoList

dodoskoRec :: List Dodosuko -> Effect (Step (List Dodosuko) Unit)
dodoskoRec prevDodosukoList =
  if take endodosukoListLength prevDodosukoList == endodosukoList then do
    log "ラブ注入♡"
    pure $ Done unit
  else do
    newDodosuko <- ranDodosuko
    log $ show newDodosuko
    pure $ Loop (newDodosuko : prevDodosukoList)

main :: Effect Unit
main = tailRecM dodoskoRec $ List.fromFoldable []
