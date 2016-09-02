{-# LANGUAGE NoMonomorphismRestriction #-}
module PeekPoke
  ( peekPoke )
where

import Prelude hiding(div,id)
import GHCJS.HPlay.View
import Transient.Base
import Transient.Move

import Data.IORef (newIORef,IORef,atomicModifyIORef)
import Control.Monad.IO.Class
import Data.String
import Data.Typeable

type Model = Int
data Msg = Increase | Decrease deriving (Read, Show, Typeable)

peekPoke :: Cloud ()
peekPoke = do
   model <- onAll . liftIO $ newIORef 0
   poke  <|> peek model

pokeMessages :: IsString a => a
pokeMessages = fs "pokeMessages"

-- send a message to the server each click
poke :: Cloud ()
poke  =  do
    msg <- local $ render $ do
       rawHtml $ div  ! id (fs "pokes") $ h1 "Pokes: 0"
       increaseButton <|> decreaseButton
    local $ render $ wprint msg
    -- update the mailbox in the server
    atRemote . local $ putMailbox pokeMessages msg >> stop
  where
    increaseButton = inputSubmit  "increase" `fire` OnClick >> return Increase
    decreaseButton = inputSubmit  "decrease" `fire` OnClick >> return Decrease

update :: Msg -> Model -> (Model,Model)
update Increase n = (n+1,n+1)
update Decrease n = (n-1,n-1)

-- receive updates from the server
peek :: IORef Model -> Cloud ()
peek model = do
  -- each update send a msg to each client
  msg <- atRemote . local $ getMailbox pokeMessages :: Cloud Msg
  local $ render $ do
    n <- liftIO $ atomicModifyIORef model (update msg)
    at (fs "#pokes") Insert . rawHtml  $ h1 $  "Pokes: " ++  show n

fs :: IsString a => String -> a
fs = fromString
