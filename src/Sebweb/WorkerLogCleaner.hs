module Sebweb.WorkerLogCleaner (
    workerLogCleaner
) where

import System.IO
import System.Directory
import Data.Time
import Data.Maybe
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Sebweb.Log
import Sebweb.LogI
import Sebweb.Worker


-- ---------------------------------------------------------------------------
-- Worker and related high level functions

workerLogCleaner :: ILogQueue -> Int ->
                    T.Text -> T.Text -> T.Text -> T.Text -> T.Text ->IO ()
workerLogCleaner ilq startt logDir storDir trashDir iSuff hSuff = do
  dailyWorker ilq "logcleaner" startt $ do
    -- TODO: FILE IO
    createDirectoryIfMissing False (T.unpack storDir)
    createDirectoryIfMissing False (T.unpack trashDir)
    logCleaner ilq logDir storDir trashDir iSuff
    logCleaner ilq logDir storDir trashDir hSuff

-- Attempts to clean log files up to 12 months back
logCleaner :: ILogQueue -> T.Text -> T.Text -> T.Text -> T.Text -> IO ()
logCleaner ilq logDir storDir trashDir suff = do
  now <- getCurrentTime
  let dBuf = 10
  let monthsBack = 12
  let ms = monthList now monthsBack (fromIntegral dBuf)
  mapM_ (\(y,m) -> cleanYM ilq (mkFil y m now dBuf) logDir trashDir (mkPth y m)) ms
  where mkPth y m = storDir <> "/" <> T.pack (show y) <> "-" <> padNum m <> suff
        mkFil y m now dBuf t = dateFilter y m t && T.isSuffixOf suff t &&
                               currentMonthDayFilter now dBuf t


-- Collects (day)logs matching y m and suff
cleanYM :: ILogQueue -> (T.Text -> Bool) -> T.Text -> T.Text -> T.Text -> IO ()
cleanYM ilq fil logDir trashDir storFile = do
  -- TODO: FILE IO
  logs <- listDirectory (T.unpack logDir) >>=
          return . sort . filter fil . map T.pack
  case null logs of
    True -> return ()
    False -> do
      logEnqueue ilq $ mkILogData ILInfo ITWorker $
        "logcleaner: filter matching " <> T.pack (show $ length logs) <>
        " log files, starting cleaning"
      -- TODO: FILE IO
      withFile (T.unpack storFile) WriteMode
        (\h -> mapM_ (appendLogFile logDir h) logs)
      mapM_ (\f -> renameFile (mkFilNam logDir f) (mkFilNam trashDir f)) logs
      logEnqueue ilq $ mkILogData ILInfo ITWorker
        "logcleaner: cleaning successful"
  where mkFilNam d f = T.unpack $ d <> "/" <> f


-- ---------------------------------------------------------------------------
-- Log file re-writing

appendLogFile :: T.Text -> Handle -> T.Text -> IO ()
appendLogFile logDir h p = do
  case listToMaybe (T.splitOn "_" p) of
    Nothing -> return ()
    Just _ -> do
      -- TODO: FILE IO
      withFile (T.unpack $ logDir <> "/" <> p) ReadMode (processLines h)

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

-- Checks if t contains year and month
dateFilter :: Int -> Int -> T.Text -> Bool
dateFilter year month t =
  let mtoks = fmap (T.splitOn "-") (listToMaybe (T.splitOn "_" t))
  in case mtoks of
    Just [y, m, _] -> y == T.pack (show year) && m == padNum month
    _ -> False

-- Given the current date and dayBuffer, checks that if t contains the current
-- month, that day does not violate the day buffer
currentMonthDayFilter :: UTCTime -> Int -> T.Text -> Bool
currentMonthDayFilter now dBuf t =
  let mtoks = fmap (T.splitOn "-") (listToMaybe (T.splitOn "_" t))
      (currentYear, currentMonth, currentDay) = toGregorian (utctDay now)
      daysT = map padNum [1..(max 0 (currentDay - dBuf))]
  in case mtoks of
    Just [y, m, d] | padNum currentMonth == m &&
                     T.pack (show currentYear) == y -> d `elem` daysT
                   | otherwise -> True
    _ -> False

padNum :: Int -> T.Text
padNum m | m < 9 = T.pack $ "0" <> show m
         | otherwise = T.pack $ show m

-- Enumerates (Y,m) starting at (now - dBuf) backward for numMonths
monthList :: UTCTime -> Integer -> Integer -> [(Int, Int)]
monthList now numMonths dBuf =
  let (_, currentMonth, _) = toGregorian (utctDay now)
      (_, potStartMonth, _) = toGregorian (addDays (-dBuf) $ utctDay now)
      start = if currentMonth == potStartMonth then 0 else 1
      ms = map (\d -> addGregorianMonthsClip (-d) (utctDay now))
          [start .. (numMonths + start - 1)]
  in map (\t -> (let (y, m, _) = toGregorian t in (fromIntegral y, m))) ms
