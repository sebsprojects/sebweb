module Sebweb.Log (
    LogQueue
  , LogData(..)
  , logCreateQueue
  , logDirect
  , logEnqueue
  , workerLogWriter

  , readLogH
  , logListFileDates
) where

import System.IO hiding (hPutStr)
import System.Directory
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad
import Data.Time
import Data.Time.Clock.System
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Sebweb.Utils


-- ------------------------------------------------------------------------
-- Log Enqueue and Write

class LogData a where
  isCritical :: a -> Bool
  assembleLogLine :: a -> T.Text
  getTimestamp :: a -> UTCTime
  setTimestamp :: UTCTime -> a -> a

type LogQueue a = TBQueue a

logCreateQueue :: Int -> IO (LogQueue a)
logCreateQueue = atomically . newTBQueue . fromIntegral

logDirect :: (LogData a) => Handle -> a -> IO ()
logDirect h ld  = do
  TIO.hPutStr h (assembleLogLine ld) >> hFlush h

logEnqueue :: (LogData a) => LogQueue a -> a -> IO ()
logEnqueue lq ld = do
  st <- getSystemTime
  atomically $ writeTBQueue lq $
    setTimestamp (epochSecondsToUTCTime (systemSeconds st)) ld

workerLogWriter :: (LogData a) => T.Text -> T.Text -> Int -> LogQueue a -> IO ()
workerLogWriter logDir fileSuffix logInterval lq = do
  _ <- forever $ do
    threadDelay (logInterval * 1000000)
    now <- getCurrentTime
    let fileName = assembleLogFileName logDir now fileSuffix
    -- TODO: FILE IO
    h <- openFile fileName AppendMode
    logFlush lq h
    hClose h
  return ()

logFlush :: (LogData a) => LogQueue a -> Handle -> IO ()
logFlush lq h = do
  ls <- atomically $ flushTBQueue lq
  mapM_ ((TIO.hPutStr h) . assembleLogLine) ls >> hFlush h


-- ------------------------------------------------------------------------
-- Log Query Utility

assembleLogFileName :: T.Text -> UTCTime -> T.Text -> String
assembleLogFileName logDir tim suff =
  T.unpack logDir <> "/" <> tf tim <> T.unpack suff
  where tf = formatTime defaultTimeLocale "%F"

readLogH :: T.Text -> UTCTime -> T.Text -> IO (Maybe Handle)
readLogH logDir tim suff = do
  let fileName = assembleLogFileName logDir tim suff
  -- TODO: FILE IO
  fe <- doesFileExist fileName
  case fe of
    False -> return Nothing
    True -> do
      -- TODO: FILE IO
      openFileRetryTimeout (1 * 1000000) (50 * 1000) fileName ReadMode

logListFileDates :: T.Text -> T.Text -> IO [UTCTime]
logListFileDates logDir infx = do
  -- TODO: Safer!
  es <- listDirectory (T.unpack logDir) >>= pure . (map T.pack)
  let logs = filter (T.isInfixOf infx) es
  let dateStrings = map (T.unpack . (safeHead "") . (T.splitOn "_")) logs
  return $ catMaybes $ map (parseTimeM True defaultTimeLocale "%F") dateStrings

