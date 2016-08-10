{-# LANGUAGE NoMonomorphismRestriction #-}
module Hello
  ( hello )
where

import Prelude hiding(div,id)
import GHCJS.HPlay.View
import Transient.Move

import Data.String

hello :: Cloud ()
hello = local $ do
  render $ rawHtml $ br >> br
  name <- render $ inputString Nothing ! placeholder (fs "your name") `fire` OnKeyUp
  render $ rawHtml $ h2 $ "hello " ++ name

placeholder= atr (fs "placeholder")

fs :: IsString a => String -> a
fs = fromString
