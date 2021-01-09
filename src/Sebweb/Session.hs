module Sebweb.Session (
    Session(..)
  , SessionStore
  , createSessionStore
  , insertSession
  , removeSessionByUser
  , removeSessionByToken
  , removeAllExpiredSessions

  , querySessionStore
  , isExpiredSession
  , workerSessionClearer
) where

import Data.Time
import Data.List
import qualified Data.Text as T
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent

import Sebweb.Utils
import Sebweb.Log
import Sebweb.LogI


data Session = Session {
  sToken :: T.Text
, sUser :: T.Text
, sExpires :: UTCTime }

type SessionStore = TVar [Session]


-- ---------------------------------------------------------------------------
-- Session Management

createSessionStore :: IO SessionStore
createSessionStore = atomically $ newTVar []

insertSession :: SessionStore -> T.Text -> T.Text -> Int -> IO ()
insertSession ss token user lifetimeSec = do
  now <- getCurrentTime
  let newS = Session token user (addSeconds lifetimeSec now)
  atomically $ modifyTVar' ss (ins newS)
  where ins newSess ss' = newSess : filter (not . matchesUserName user) ss'

removeSessionByUser :: SessionStore -> T.Text -> IO ()
removeSessionByUser ss userName = do
  atomically $ modifyTVar' ss (filter (not . matchesUserName userName))

removeSessionByToken :: SessionStore -> T.Text -> IO ()
removeSessionByToken ss tok = do
  atomically $ modifyTVar' ss (filter (not . matchesToken tok))

removeAllExpiredSessions :: SessionStore -> IO ()
removeAllExpiredSessions ss = do
  now <- getCurrentTime
  atomically $ modifyTVar' ss (filter (isExpiredSession now))

-- Only queries the session store, does not modify even if expired!
querySessionStore :: SessionStore -> T.Text -> IO (Maybe Session)
querySessionStore ss tok = do
  sessions <- readTVarIO ss
  return $ find (matchesToken tok) sessions

isExpiredSession :: UTCTime -> Session -> Bool
isExpiredSession now s = (diffUTCTime (sExpires s) now) < 0

matchesUserName :: T.Text -> Session -> Bool
matchesUserName t s = t == (sUser s)

matchesToken :: T.Text -> Session -> Bool
matchesToken t s = t == (sToken s)

workerSessionClearer :: LogQueue -> SessionStore -> IO ()
workerSessionClearer ilq ss = do
  nowStartUp <- getCurrentTime
  let su2AM = secondsUntilTomorrowAtHour 2 nowStartUp
  logEnqueue ilq $ ILogData nowStartUp ILInfo ITWorker $
    "sessionclearer: scheduling session clear for in " <>
    (T.pack $ show su2AM) <> " seconds"
  threadDelay (su2AM * 1000000)
  _ <- forever $ do
    now <- getCurrentTime
    sessLBefore <- readTVarIO ss >>= return . length
    removeAllExpiredSessions ss
    sessLAfter <- readTVarIO ss >>= return . length
    logEnqueue ilq $ ILogData now ILInfo ITWorker $
      "sessionclearer: from " <> (T.pack $ show sessLBefore) <>
      " to " <> (T.pack $ show sessLAfter) <> " sessions"
    threadDelay ((secondsUntilTomorrowAtHour 2 now) * 1000000)
  return ()

