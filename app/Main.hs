module Main where

import PeekPoke (peekPoke)
import Hello (hello)

import GHCJS.HPlay.View
import Transient.Base

main :: IO ()
main = keep . initNode . onBrowser $ peekPoke <|> hello
