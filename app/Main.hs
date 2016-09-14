module Main where

import PeekPoke (peekPoke)
import Hello (hello)
import RWebsite (rwebsite)

import GHCJS.HPlay.View
import Transient.Base

main :: IO ()
main = rwebsite
