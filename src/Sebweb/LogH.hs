module Sebweb.LogH (
    HLogData(..)

  , HLogQuery(..)
  , hLogPerformQuery
) where

import System.IO hiding (hPutStr)
import Data.Time
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Sebweb.Utils
import Sebweb.Log


-- ------------------------------------------------------------------------
-- HLog Data Types

data HLogData = HLogData {
  hldTime :: UTCTime
, hldIP :: Maybe T.Text
, hldVersion :: T.Text
, hldMethod :: T.Text
, hldPath :: T.Text
, hldStatus :: T.Text
, hldHeaderHost :: Maybe T.Text
, hldReferer :: Maybe T.Text
, hldUserAgent :: Maybe T.Text
, hldCookie :: Bool
, hldDNT :: Bool
, hldCountry :: Maybe T.Text
} deriving (Show)

instance LogData HLogData where
  isCritical _ = False
  assembleLogLine = assembleHttpLogLine


-- ------------------------------------------------------------------------
-- HTTP Log Line Construction

assembleHttpLogLine :: HLogData -> T.Text
assembleHttpLogLine hld = T.intercalate "," xs <> "\n"
  where xs = [ -- basic: timestamp, ip, status, version, method, url
               T.pack $ formatTime defaultTimeLocale "%F %T" (hldTime hld)
               , fromMaybe "" (hldIP hld)
               , hldVersion hld
               , hldMethod hld
               , encodeCSV 64 $ hldPath hld
               , hldStatus hld
               -- header: host, referer, user-agent
               , encodeCSV 64 $ fromMaybe "" (hldHeaderHost hld)
               , encodeCSV 64 $ fromMaybe "" (hldReferer hld)
               -- , encodeCSV 256 $ fromMaybe "" (httpLogUserAgent ld)
               , procUA (hldUserAgent hld)
               -- server info: cookie, DNT
               , if hldCookie hld then "1" else "0"
               , if hldDNT hld then "1" else "0"
               -- ip info: country code
               , fromMaybe "" (hldCountry hld)
               ]
        procUA Nothing = ""
        procUA (Just ua) = fromMaybe "UnknownUA" (extractUserAgent ua)

extractUserAgent :: T.Text -> Maybe T.Text
extractUserAgent = tryUATokens
  [ "Android", "iPad", "iPhone", "Windows Phone", -- mobile
    "Ubuntu", "Linux", "Macintosh", "Windows",    -- PC
    "Googlebot", "bingbot", "Yahoo"               -- bot
  ]

tryUATokens :: [T.Text] -> T.Text -> Maybe T.Text
tryUATokens [] _ = Nothing
tryUATokens (t : ts) uaText | T.isInfixOf t uaText = Just t
                            | otherwise = tryUATokens ts uaText


-- ------------------------------------------------------------------------
-- HTTP Log Query

data HLogQuery = HLogQuery {
  hlqDate :: UTCTime
, hlqMethods :: [T.Text]
, hlqStatus :: [T.Text]
, hlqStatic :: (Bool, Bool)
} deriving (Show, Eq)

hLogPerformQuery :: T.Text -> HLogQuery -> IO [HLogData]
hLogPerformQuery logDir hlq = do
  mh <- readLogH logDir (hlqDate hlq) "_httplog.txt"
  case mh of
    Nothing -> pure []
    Just h -> gatherHttpReport hlq h []

gatherHttpReport :: HLogQuery -> Handle -> [HLogData] -> IO [HLogData]
gatherHttpReport hlq h acc = do
  eof <- hIsEOF h
  case eof of
    True -> return acc
    False -> do
      csv <- TIO.hGetLine h >>= pure . parseCSVLine
      case length csv == 12 of
        False -> gatherHttpReport hlq h acc -- invalid log line, ignore
        True -> case matchesHttpQuery hlq csv of
          False -> gatherHttpReport hlq h acc -- does not match query, ignore
          True ->
            gatherHttpReport hlq h (acc ++ [assembleHttpReport csv])

assembleHttpReport :: [T.Text] -> HLogData
assembleHttpReport csv =
  let mtim = parseTimeM False defaultTimeLocale "%F %T" (T.unpack $ csv !! 0)
  in HLogData {
    hldTime = fromMaybe errorTime mtim
  , hldIP = if dnt then Nothing else safeIndex csv 1
  , hldVersion = csv !! 2
  , hldMethod = csv !! 3
  , hldPath = dropQuotation (csv !! 4)
  , hldStatus = csv !! 5
  , hldHeaderHost = fmap dropQuotation (safeIndex csv 6)
  , hldReferer = fmap dropQuotation (safeIndex csv 7)
  , hldUserAgent = if dnt then Nothing else procMT (csv !! 8)
  , hldCookie = procBool (csv !! 9)
  , hldDNT = dnt
  , hldCountry = procMT (csv !! 11) }
  where procMT "" = Nothing
        procMT t = Just t
        procBool "1" = True
        procBool _ = False
        dnt = procBool (csv !! 10)

matchesHttpQuery :: HLogQuery -> [T.Text] -> Bool
matchesHttpQuery hlq csv =
  let suff = fromMaybe "" $ extractPathSuffix (dropQuotation (csv !! 4))
  in and [(csv !! 5) `elem` (hlqStatus hlq),
          (csv !! 3) `elem` (hlqMethods hlq),
          (evalStatic (isStaticSuffix suff) (hlqStatic hlq))]
  where evalStatic _ (True, True) = True
        evalStatic True (_, True) = True
        evalStatic False (True, _) = True
        evalStatic _ (_, _) = False

