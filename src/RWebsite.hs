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


-- Events
data Msg = LoginFormUpdated LoginForm
         | MyFormUpdated MyForm
         deriving (Show,Read,Typeable)

data LoginForm = LoginForm { login :: String
                           , pass :: String }
               deriving (Show,Read,Typeable)

data MyForm = MyForm { i1, i2 :: String }
            deriving (Show,Read,Typeable)

-- States
data State = State LoginState MyFormState
           deriving (Show,Read,Typeable)

data LoginState = BeforeLogin
                | FailedLogin
                | Logged UserId
                | AdminLogged UserId
                deriving (Show,Read,Typeable)

newtype UserId = UserId String
               deriving (Show,Read,Typeable)

data MyFormState = MyFormNotSendYet
                 | MyFormSent MyForm
                 deriving (Show,Read,Typeable)

-- Init data structure
data InitRWebsite = InitRWebsite (IORef State) (EVar Msg)

initRWebsite :: TransIO InitRWebsite
initRWebsite = do
  -- generate initial state
  rdata <- liftIO $ newIORef $ State BeforeLogin MyFormNotSendYet
  -- initialize event bus
  dataAvailable <- newEVar
  -- return the init data structure
  return (InitRWebsite rdata dataAvailable)

rwebsite :: InitRWebsite -> Cloud ()
rwebsite (InitRWebsite rdata dataAvailable) =
  frontend rdata dataAvailable
  <|> syncWidget dataAvailable
  <|> longLivingProcess rdata dataAvailable

frontend :: IORef State -> EVar Msg -> Cloud ()
frontend rdata dataAvailable = onBrowser $ do
  loginWidget rdata dataAvailable
  formWidget rdata dataAvailable

loginWidget :: IORef State -> EVar Msg -> Cloud ()
loginWidget _ dataAvailable = do
  local . render . rawHtml $ h1 "Login Widget"
  loginForm <- local . render $
    LoginForm <$> getString Nothing `fire` OnChange
              <*> getString Nothing `fire` OnChange
  atRemote $ localIO $ writeEVar dataAvailable (LoginFormUpdated loginForm)

formWidget :: IORef State -> EVar Msg -> Cloud ()
formWidget rdata dataAvailable = do
  local . render . rawHtml $ h1 "MyForm Widget"
  st <- atRemote $ localIO $ readIORef rdata
  case st of
    (State (Logged _) _) -> do
        myForm <- local . render $ MyForm <$> getString Nothing `fire` OnKeyUp
                                          <*> getString Nothing `fire` OnKeyUp
        atRemote $ localIO $ writeEVar dataAvailable (MyFormUpdated myForm)
    (State (AdminLogged _) MyFormNotSendYet) -> 
        local.render.rawHtml $ h1 "Waiting Formulary"
    (State (AdminLogged _) (MyFormSent myForm)) ->
        local.render.rawHtml $ do
          h1 "Waiting Formulary"
          div (show myForm)
    _ -> local . render .rawHtml $ h1 "FAILED"

syncWidget :: EVar Msg ->  Cloud ()
syncWidget dataAvailable = onBrowser $ do
  -- display in the browser
  received <- atRemote $ local $ readEVar dataAvailable
  local . render . at (fromString "#msg") Insert . rawHtml $
      p $ "Check other navigators: " <> show received

longLivingProcess :: IORef State -> EVar Msg -> Cloud ()
longLivingProcess rdata dataAvailable =
  onServer $ local $ do
    liftIO $ print "longliving"
    msg <- readEVar dataAvailable
    liftIO $ modifyIORef rdata (update msg)
    liftIO $ print $ show msg <> " arrived"
    state <- liftIO $ readIORef rdata
    liftIO $ print $ show state <> " inner state"

update :: Msg -> State -> State  
update (LoginFormUpdated (LoginForm "foo" "bar")) (State _ mf) = State (Logged (UserId "foo")) mf
update (LoginFormUpdated (LoginForm "admin" "admin")) (State _ mf) = State (AdminLogged (UserId "foo")) mf
update (LoginFormUpdated _) (State _ mf) = State FailedLogin mf
update (MyFormUpdated myForm) (State (Logged l) _) = State (Logged l) (MyFormSent myForm)
update _ (State _ mf) = State FailedLogin mf
