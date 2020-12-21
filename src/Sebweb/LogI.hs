module Sebweb.LogI (
    ILogLevel(..)
  , ILogType(..)
  , ILogData(..)

  , ILogQuery
  , iLogPerformQuery
) where

import System.IO hiding (hPutStr)
import Data.Time
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Sebweb.Utils
import Sebweb.Log


-- ------------------------------------------------------------------------
-- ILog Data Types

data ILogLevel = ILInfo | ILError | ILCrit deriving (Eq)
data ILogType = ITWorker | ITAuth | ITOther | ITWarp deriving (Eq)

instance Show ILogLevel where
  show ILInfo = "Inf"
  show ILError = "Err"
  show ILCrit = "Crt"

instance Show ILogType where
  show ITWorker = "Wrk"
  show ITAuth = "Ath"
  show ITOther = "Oth"
  show ITWarp = "Wrp"

data ILogData = ILogData {
  ildTime :: UTCTime
, ildLevel :: ILogLevel
, ildType :: ILogType
, ildMessage :: T.Text }
  deriving (Show)

instance LogData ILogData where
  isCritical (ILogData _ level _ _) = level == ILCrit
  assembleLogLine = assembleILogLine

assembleILogLine :: ILogData -> T.Text
assembleILogLine ild = T.intercalate "," itms <> "\n"
  where itms = [ T.pack $ formatTime defaultTimeLocale "%F %T" (ildTime ild)
               , T.pack $ show (ildLevel ild)
               , T.pack  $ show (ildType ild)
               , encodeCSV 512 $ ildMessage ild ]


-- ------------------------------------------------------------------------
-- ILog Query

data ILogQuery = ILogQuery {
  ilqrDate :: UTCTime
, ilqrLevel :: [ILogLevel]
, ilqrType :: [ILogType]
} deriving (Show, Eq)

iLogPerformQuery :: T.Text -> ILogQuery -> IO [ILogData]
iLogPerformQuery logDir ilqr = do
  mh <- readLogH logDir (ilqrDate ilqr) "_internallog.txt"
  case mh of
    Nothing -> pure []
    Just h -> gatherInternalReport ilqr h []

gatherInternalReport :: ILogQuery -> Handle -> [ILogData] -> IO [ILogData]
gatherInternalReport ilqr h acc = do
  eof <- hIsEOF h
  case eof of
    True -> return acc
    False -> do
      csv <- TIO.hGetLine h >>= pure . parseCSVLine
      case length csv == 4 of
        False -> gatherInternalReport ilqr h acc
        True -> case matchesInternalQuery ilqr csv of
          False -> gatherInternalReport ilqr h acc
          True -> do
            let repLine = assembleInternalReport csv
            gatherInternalReport ilqr h (acc ++ [repLine])

assembleInternalReport :: [T.Text] -> ILogData
assembleInternalReport csv =
  let mtim = parseTimeM False defaultTimeLocale "%F %T" (T.unpack $ csv !! 0)
  in ILogData { ildTime = fromMaybe errorTime mtim
              , ildLevel = textToILLeniant (csv !! 1)
              , ildType = textToITLeniant (csv !! 2)
              , ildMessage = dropQuotation (csv !! 3)
              }

matchesInternalQuery :: ILogQuery -> [T.Text] -> Bool
matchesInternalQuery ilqr csv =
  (textToILLeniant (csv !! 1) `elem` (ilqrLevel ilqr)) &&
  (textToITLeniant (csv !! 2) `elem` (ilqrType ilqr))

textToITLeniant :: T.Text -> ILogType
textToITLeniant = (fromMaybe ITOther) . textToIT

textToILLeniant :: T.Text -> ILogLevel
textToILLeniant = (fromMaybe ILInfo) . textToIL

textToIT :: T.Text -> Maybe ILogType
textToIT "Wrk" = Just ITWorker
textToIT "Ath" = Just ITAuth
textToIT "Wrp" = Just ITWarp
textToIT "Oth" = Just ITOther
textToIT _ = Nothing

textToIL :: T.Text -> Maybe ILogLevel
textToIL "Crt" = Just ILCrit
textToIL "Err" = Just ILError
textToIL "Inf" = Just ILInfo
textToIL _ = Nothing

