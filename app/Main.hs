module Main where

import PeekPoke (peekPoke)
import Hello (hello)
import RWebsite (rwebsite,initRWebsite)

import GHCJS.HPlay.View
import Transient.Base

main :: IO ()
main = keep $ do
  initialState <- initRWebsite
  initNode $ onBrowser peekPoke
           <|> rwebsite initialState
