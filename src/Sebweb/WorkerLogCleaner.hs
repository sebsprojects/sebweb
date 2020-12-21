module Sebweb.WorkerLogCleaner where

import System.IO
import System.Directory
import System.Process
import Control.Concurrent
import Control.Monad
import Control.Exception
import Data.Time
import Data.Maybe
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Sebweb.Utils
import Sebweb.LogHttp
import Sebweb.LogInternal


-- ---------------------------------------------------------------------------
-- Worker and related high level functions

logCleaningWorker :: InternalLogQueue -> T.Text -> T.Text -> IO ()
logCleaningWorker ilq logDir storDir = do
  -- TODO: Safe!
  createDirectoryIfMissing False (T.unpack storDir)
  nowStartUp <- getCurrentTime
  let su3AM = secondsUntilTomorrowAtHour 3 nowStartUp
  enqueueInternalLogLine ilq $ InternalLogData nowStartUp ILInfo ITWorker $
    "logcleaner: scheduling log cleaning for in " <> (T.pack $ show su3AM) <>
    " seconds"
  threadDelay (su3AM * 1000000)
  _ <- forever $ do
    now <- getCurrentTime
    internalLogCleaner ilq logDir storDir
    httpLogCleaner ilq logDir storDir
    threadDelay ((secondsUntilTomorrowAtHour 3 now) * 1000000)
  return ()

-- Attempts to clean all log files that are older than currentMonth-2 months
internalLogCleaner :: InternalLogQueue -> T.Text -> T.Text -> IO ()
internalLogCleaner ilq logDir storDir = do
  now <- getCurrentTime
  let ms = monthList now 10
  -- Delay by 2 seconds between files if the asynch zip call takes some time
  -- to prevent to many zip processes spawning
  --mapM_ (\(y,m) -> uncurry (logCleaner ilq (fil y m) logDir) (afp y m) >>
  --                 threadDelay (2 * 1000000))
  --      ms
  mapM_ (\(y,m) -> uncurry (logCleaner ilq (fil y m) logDir) (afp y m)) ms
  where afp y m = (storDir <> "/" <> y <> "-" <> m <> "_internallog.txt",
                   storDir <> "/" <> y <> "_internallog.zip")
        fil y m t = intFilter t && dateFilter y m t

-- Attempts to clean all log files that are older than currentMonth-2 months
httpLogCleaner :: InternalLogQueue -> T.Text -> T.Text -> IO ()
httpLogCleaner ilq logDir storDir = do
  now <- getCurrentTime
  let ms = monthList now 10 -- deal with the log files of the last 10+2 months
  --mapM_ (\(y,m) -> uncurry (logCleaner ilq (fil y m) logDir) (afp y m) >>
  --                 threadDelay (2 * 1000000))
  --      ms
  mapM_ (\(y,m) -> uncurry (logCleaner ilq (fil y m) logDir) (afp y m)) ms
  where afp y m = (storDir <> "/" <> y <> "-" <> m <> "_httplog.txt",
                   storDir <> "/" <> y <> "_httplog.zip")
        fil y m t = httpFilter t && dateFilter y m t

-- Attempts to gather all log files in logDir matching the filter into one file
-- storFile and then add this file to the zip archive zipFile
logCleaner :: InternalLogQueue -> (T.Text -> Bool) -> T.Text -> T.Text ->
              T.Text -> IO ()
logCleaner ilq fil logDir storFile zipFile = do
  now <- getCurrentTime
  logs <- listDirectory (T.unpack logDir) >>=
          return . sort . (filter fil) . (map T.pack)
  case null logs of
    True -> return ()
    False -> do
      enqueueInternalLogLine ilq $ InternalLogData now ILInfo ITWorker $
        "logcleaner: filter matching " <> T.pack (show $ length logs) <>
        " log files, starting cleaning"
      -- TODO: Safe!
      bracket
        (openFile (T.unpack storFile) WriteMode)
        (hClose)
        (\h -> mapM_ (appendLogFile logDir h) logs)
      let trashDir = logDir <> "/trash"
      -- TODO: Safe!
      createDirectoryIfMissing False (T.unpack trashDir)
      --putStrLn $ "\nFilter matches " <> (show $ length logs) <> "log files"
      mapM_ (\f -> renameFile (prepDir logDir f) (prepDir trashDir f)) logs
      --callProcess "zip" ["--quiet", "--test", "--move", "--junk-paths",
      --                   T.unpack zipFile, T.unpack storFile]
      enqueueInternalLogLine ilq $ InternalLogData now ILInfo ITWorker
                                   "logcleaner: cleaning successful"
  where prepDir d f = T.unpack $ d <> "/" <> f


-- ---------------------------------------------------------------------------
-- Log file re-writing

appendLogFile :: T.Text -> Handle -> T.Text -> IO ()
appendLogFile logDir h p = do
  let mds = listToMaybe $ T.splitOn "_" p
  --putStrLn $ "Processing " <> T.unpack p
  case mds of
    Nothing -> return ()
    Just ds -> do
      -- TODO: Make exception safe
      bracket
        (openFile (T.unpack $ logDir <> "/" <> p) ReadMode)
        hClose
        (\hLog -> processLines ds h hLog)
      --putStrLn "Done processing"

-- TODO: This is hacky and should be dealt with properly
-- Detect if we are parsing http log lines
processLines :: T.Text -> Handle -> Handle -> IO ()
processLines dateString h hl = do
  iseof <- hIsEOF hl
  if iseof
    then return ()
    else do
      l <- TIO.hGetLine hl
      case isDNT (parseCSVLine l) of
        True -> return ()
        False -> TIO.hPutStrLn h (dateString <> " " <> l)
      processLines dateString h hl
  where isDNT csv | length csv `elem` [11, 12] = (csv !! 10) == "1"
                  | otherwise = False


-- ---------------------------------------------------------------------------
-- Filters and Utility

httpFilter :: T.Text -> Bool
httpFilter t = T.isInfixOf "http" t && not (T.isInfixOf "old" t)

intFilter :: T.Text -> Bool
intFilter t = T.isInfixOf "internal" t

dateFilter :: T.Text -> T.Text -> T.Text -> Bool
dateFilter year month t =
          let mtoks = fmap (T.splitOn "-") (listToMaybe (T.splitOn "_" t))
          in case mtoks of
            Just (y:m:_:[]) -> y == year && m == month
            _ -> False

monthList :: UTCTime -> Integer -> [(T.Text, T.Text)]
monthList now numMonths =
  let ms = map (\d -> addGregorianMonthsClip (-d) (utctDay now))
          [2..(numMonths)+2]
      ss = map (\t -> T.pack $ formatTime defaultTimeLocale "%Y-%m" t) ms
  in map ((\t -> (safeHead "" t, safeLast "" t)) . (T.splitOn "-")) ss

