module Sebweb.WorkerLogCleaner (
    workerLogCleaner
) where

import System.IO
import System.Directory
import Control.Concurrent
import Control.Monad
import Control.Exception
import Data.Time
import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Sebweb.Utils
import Sebweb.Log
import Sebweb.LogI


-- ---------------------------------------------------------------------------
-- Worker and related high level functions

workerLogCleaner :: LogQueue -> T.Text -> T.Text -> IO ()
workerLogCleaner ilq logDir storDir = do
  -- TODO: FILE IO
  createDirectoryIfMissing False (T.unpack storDir)
  createDirectoryIfMissing False (T.unpack $ logDir <> "/trash")
  nowStartUp <- getCurrentTime
  let su3AM = secondsUntilTomorrowAtHour 3 nowStartUp
  logEnqueue ilq $ ILogData nowStartUp ILInfo ITWorker $
    "logcleaner: scheduling log cleaning for in " <> (T.pack $ show su3AM) <>
    " seconds"
  threadDelay (su3AM * 1000000)
  _ <- forever $ do
    now <- getCurrentTime
    logCleaner ilq logDir storDir "_internallog.txt"
    logCleaner ilq logDir storDir "_httplog.txt"
    threadDelay ((secondsUntilTomorrowAtHour 3 now) * 1000000)
  return ()

-- Attempts to clean log files up to 12 months back
logCleaner :: LogQueue -> T.Text -> T.Text -> T.Text -> IO ()
logCleaner ilq logDir storDir suff = do
  now <- getCurrentTime
  let ms = monthList now 12 10
  mapM_ (\(y,m) -> (cleanLog ilq (mkFil y m) logDir (mkPth y m))) ms
  where mkPth y m = storDir <> "/" <> y <> "-" <> m <> suff
        mkFil y m t = dateFilter y m t &&
                     (typeFilter "internal" suff t || typeFilter "http" suff t)

cleanLog :: LogQueue -> (T.Text -> Bool) -> T.Text -> T.Text -> IO ()
cleanLog ilq fil logDir storFile = do
  now <- getCurrentTime
  -- TODO: FILE IO
  logs <- listDirectory (T.unpack logDir) >>=
          return . sort . (filter fil) . (map T.pack)
  case null logs of
    True -> return ()
    False -> do
      logEnqueue ilq $ ILogData now ILInfo ITWorker $
        "logcleaner: filter matching " <> T.pack (show $ length logs) <>
        " log files, starting cleaning"
      -- TODO: FILE IO
      bracket
        (openFile (T.unpack storFile) WriteMode)
        (hClose)
        (\h -> mapM_ (appendLogFile logDir h) logs)
      let trashDir = logDir <> "/trash"
      mapM_ (\f -> renameFile (prepDir logDir f) (prepDir trashDir f)) logs
      logEnqueue ilq $ ILogData now ILInfo ITWorker
                                      "logcleaner: cleaning successful"
  where prepDir d f = T.unpack $ d <> "/" <> f


-- ---------------------------------------------------------------------------
-- Log file re-writing

appendLogFile :: T.Text -> Handle -> T.Text -> IO ()
appendLogFile logDir h p = do
  case listToMaybe (T.splitOn "_" p) of
    Nothing -> return ()
    Just _ -> do
      -- TODO: FILE IO
      bracket
        (openFile (T.unpack $ logDir <> "/" <> p) ReadMode)
        hClose
        (\hLog -> processLines h hLog)

processLines :: Handle -> Handle -> IO ()
processLines hStor hLog = do
  iseof <- hIsEOF hLog
  if iseof
  then return ()
  else do
    TIO.hGetLine hLog >>= TIO.hPutStrLn hStor
    processLines hStor hLog


-- ---------------------------------------------------------------------------
-- Filters and Utility

dateFilter :: T.Text -> T.Text -> T.Text -> Bool
dateFilter year month t =
          let mtoks = fmap (T.splitOn "-") (listToMaybe (T.splitOn "_" t))
          in case mtoks of
            Just (y : m:_:[]) -> y == year && m == month
            _ -> False

typeFilter :: T.Text -> T.Text -> T.Text -> Bool
typeFilter typ suff t = T.isInfixOf typ suff && T.isInfixOf typ t

-- Enumerates (Y,m) starting at (now - dBuf) backward for numMonths
monthList :: UTCTime -> Integer -> Integer -> [(T.Text, T.Text)]
monthList now numMonths dBuf =
  let (_, currentMonth, _) = toGregorian (utctDay now)
      (_, potStartMonth, _) = toGregorian (addDays (-dBuf) $ utctDay now)
      start = if currentMonth == potStartMonth then 0 else 1
      ms = map (\d -> addGregorianMonthsClip (-d) (utctDay now))
          [start .. (numMonths + start - 1)]
      ss = map (\t -> T.pack $ formatTime defaultTimeLocale "%Y-%m" t) ms
  in map ((\t -> (safeHead "" t, safeLast "" t)) . (T.splitOn "-")) ss

