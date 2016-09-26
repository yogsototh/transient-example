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
         | MyFormUpdated MyForm deriving (Show,Read,Typeable)
data LoginForm = LoginForm { login :: String
                           , pass :: String } deriving (Show,Read,Typeable)
data MyForm = MyForm { i1, i2 :: String } deriving (Show,Read,Typeable)

-- States
data State = BeforeLogin | FailedLogin | Logged UserId | MyFormSent MyForm deriving (Show,Read,Typeable)
newtype UserId = UserId String deriving (Show,Read,Typeable)

-- Init data structure
data InitRWebsite = InitRWebsite (IORef State) (EVar Msg)

initRWebsite :: TransIO InitRWebsite
initRWebsite = do
  -- generate initial state
  rdata <- liftIO $ newIORef BeforeLogin
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
frontend rdata dataAvailable = onBrowser $
  loginWidget rdata dataAvailable
  -- <|> formWidget rdata dataAvailable

loginWidget :: IORef State -> EVar Msg -> Cloud ()
loginWidget rdata dataAvailable = onBrowser $ do
  local . render . rawHtml $ h1 "Login Widget"
  loginForm <- local . render $
    LoginForm <$> getString Nothing `fire` OnChange
              <*> getString Nothing `fire` OnChange
  atRemote $ localIO $ writeEVar dataAvailable (LoginFormUpdated loginForm)
  local . render . rawHtml $ h1 "MyForm Widget"
  st <- atRemote $ localIO $ readIORef rdata
  case st of
    (Logged _) -> do
        myForm <- local . render $ MyForm <$> getString Nothing `fire` OnKeyUp
                                          <*> getString Nothing `fire` OnKeyUp
        atRemote $ localIO $ writeEVar dataAvailable (MyFormUpdated myForm)
    _ -> local . render .rawHtml $ h1 "FAILED"

-- formWidget :: IORef Msg -> EVar MyForm -> EVar LoginForm -> Cloud ()
-- formWidget rdata dataAvailable loginForm = onBrowser $ do
--   lf <- local $ readEVar loginForm
--   case lf of
--   then do
--        local . render . rawHtml $ do
--             h1 "Form Widget"
--             div ! id (fromString "msg") $ "No message yet."
--        st <- atRemote . localIO $ readIORef rdata -- read the state data
--        myForm <- local . render $ MyForm <$> getString (Just (i1 st)) `fire` OnKeyUp
--                             <*> getString (Just (i2 st)) `fire` OnKeyUp
--        -- notify the long living process
--        atRemote $ localIO $ writeEVar dataAvailable myForm
--   else local . render . rawHtml $ h2 "Wrong Login"

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
update (LoginFormUpdated (LoginForm "foo" "bar")) _ = Logged (UserId "foo")
update (LoginFormUpdated _) _ = FailedLogin
update (MyFormUpdated myForm) (Logged _)= MyFormSent myForm
update (MyFormUpdated myForm) (MyFormSent _)= MyFormSent myForm
update _ _ = FailedLogin
