{-# LANGUAGE NoMonomorphismRestriction #-}
module Lib
  ( startServer )
where

import GHCJS.HPlay.View
import Transient.Base
import Transient.Move
import Data.IORef
import Control.Monad.IO.Class
import Data.String
import Prelude hiding(div,id)

startServer :: IO ()
startServer = keep . initNode $ onBrowser peekPoke

peekPoke = do
   model <- onAll . liftIO $ newIORef (0 :: Int)
   poke  <|> peek model

pokeMessages = fs "pokeMessages"

-- send a message to the server each click
poke :: Cloud ()
poke  =  do
    local $ render $ do
       rawHtml $ div  ! id (fs "pokes") $ p "Pokes: 0"
       inputSubmit  "Poke others" `fire` OnClick

    atRemote . local $ putMailbox pokeMessages "poke" >> stop  -- update the mailbox in the server

-- receive updates from the server
peek :: IORef Int -> Cloud ()
peek model = do
    msg <- atRemote . local $ getMailbox pokeMessages  :: Cloud String  -- each update send a msg to each client
    local $ render $ do
       n <- liftIO $ atomicModifyIORef model $ \n ->  (n +1,n+1)
       at  (fs "#pokes")  Insert . rawHtml  $ p $  "Pokes: " ++  show n

fs= fromString
