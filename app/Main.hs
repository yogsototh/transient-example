module Main where

-- import PeekPoke (peekPoke)
-- import Hello (hello)
import RWebsite (rwebsite,initRWebsite)

import GHCJS.HPlay.View
import Transient.Base

main :: IO ()
main = keep $ do
  initialState <- initRWebsite
  initNode $ rwebsite initialState
  -- initNode $ onBrowser peekPoke
  --          <|> onBrowser hello
  --          <|> rwebsite initialState
