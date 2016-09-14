module RWebsite where

-- The IORef play the role of a DB

import Prelude hiding(div,id)
import Transient.Base
import Transient.Move
import Transient.EVars
import Transient.Move.Utils
import GHCJS.HPlay.View hiding (option,input)
import Data.IORef
import Data.String (fromString)
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Data.Typeable (Typeable)

data MyForm = MyForm { i1, i2 :: String } deriving (Show,Read,Typeable)

data InitRWebsite = InitRWebsite (IORef MyForm) (EVar MyForm)

rwebsite :: IO ()
rwebsite = keep $ do
  rdata <- liftIO $ newIORef (MyForm "i1" "i2")
  dataAvailable <- newEVar
  initNode $ formWidget rdata dataAvailable
           <|> syncWidget dataAvailable
           <|> longLivingProcess rdata dataAvailable

formWidget :: IORef MyForm -> EVar MyForm -> Cloud ()
formWidget rdata dataAvailable = onBrowser $ do
  local . render . rawHtml $ do
        h1 "State"
        div ! id (fromString "msg") $ "No message yet."
  st <- atRemote . localIO $ readIORef rdata -- read the state data
  myForm <- local . render $ MyForm <$> getString (Just (i1 st)) `fire` OnKeyUp
                                    <*> getString (Just (i2 st)) `fire` OnKeyUp
  -- notify the long living process
  atRemote $ localIO $ writeEVar dataAvailable myForm

syncWidget :: EVar MyForm ->  Cloud ()
syncWidget dataAvailable = onBrowser $ do
  -- display in the browser
  received <- atRemote $ local $ readEVar dataAvailable
  local . render . at (fromString "#msg") Insert . rawHtml $
      p $ "Check other navigators: " <> i1 received <> ", " <> i2 received

longLivingProcess :: IORef MyForm -> EVar MyForm -> Cloud ()
longLivingProcess rdata dataAvailable =
  onServer $ local $ do
    liftIO $ print "longliving"
    dat <- readEVar dataAvailable
    liftIO $ writeIORef rdata dat
    liftIO $ print $ show dat <> " arrived"
