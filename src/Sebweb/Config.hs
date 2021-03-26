{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Sebweb.Config (
    ServerConfig(..)
  , configFromFile
  , configFromText

  , logStartup
  , runStandardRedirecter
  , runStandardApp

  , warpRedirectSettings
  , warpMainSettings
  , warpTLSSettings
) where

import System.Directory
import System.IO
import Control.Exception
import Text.Read
import Data.Maybe
import Data.Time
import Data.List
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS

import Sebweb.Utils
import Sebweb.Log
import Sebweb.LogI
import Sebweb.LogH
import Sebweb.ResponseCommon
import Sebweb.Middleware
import Sebweb.Session


data ServerConfig = ServerConfig {
  cRevision :: T.Text

, cHostName :: T.Text
, cServerName :: T.Text
, cHttpPort :: Int
, cHttpsPort :: Int

, cSslCertPath :: T.Text
, cSslKeyPath :: T.Text
, cUsersDirPath :: T.Text
, cLogDirPath :: T.Text
, cStaticDirPath :: T.Text

, cCookieName :: T.Text
, cMaxSessionAge :: Int
, cMaxContentLength :: Int
, cResponseTimeout :: Int

, cLogQueueLength :: Int
, cLogFlushInterval :: Int

, cWrktSessionClearer :: Int
, cWrktWhoisLooker :: Int
, cWrktLogCleaner :: Int
}


-- ------------------------------------------------------------------------
-- Config Parsing (subset of JSON supporting only int and string)

cfgT :: T.Text -> [(T.Text, T.Text)] -> Maybe T.Text
cfgT key config = lookup key config

cfgI :: T.Text -> [(T.Text, T.Text)] -> Maybe Int
cfgI key config = compMF2 readMaybe (fmap T.unpack) (lookup key config)

-- TODO: FILE IO
configFromFile :: FilePath -> IO (Maybe ServerConfig)
configFromFile fp = do
  fe <- doesFileExist fp
  case fe of
    True -> TIO.readFile fp >>= return . configFromText
    False -> return Nothing

-- Make sure all records of ServerConfig are present here
-- This guarantees that all records are read from JSON
configFromText :: T.Text -> Maybe ServerConfig
configFromText t =
  let dict = configPairsFromText t
      valT key f = fmap f (cfgT key dict) -- f :: T.Text -> a
      valI key f = fmap f (cfgI key dict) -- f :: Int -> a
      fm f x = fromMaybe Nothing $ fmap f x
      updM valType key updf = fm (\sc -> valType key (updf sc))
  in updM valT "revision"           (\c v -> c { cRevision = v }) $
     updM valT "hostName"           (\c v -> c { cHostName = v }) $
     updM valT "serverName"         (\c v -> c { cServerName = v }) $
     updM valI "httpPort"           (\c v -> c { cHttpPort = v }) $
     updM valI "httpsPort"          (\c v -> c { cHttpsPort = v }) $
     updM valT "sslCertPath"        (\c v -> c { cSslCertPath = v }) $
     updM valT "sslKeyPath"         (\c v -> c { cSslKeyPath = v }) $
     updM valT "usersDirPath"       (\c v -> c { cUsersDirPath = v }) $
     updM valT "logDirPath"         (\c v -> c { cLogDirPath= v }) $
     updM valT "staticDirPath"      (\c v -> c { cStaticDirPath = v }) $
     updM valT "cookieName"         (\c v -> c { cCookieName = v }) $
     updM valI "maxSessionAge"      (\c v -> c { cMaxSessionAge = v }) $
     updM valI "maxContentLength"   (\c v -> c { cMaxContentLength= v }) $
     updM valI "responseTimeout"    (\c v -> c { cResponseTimeout = v }) $
     updM valI "logQueueLength"     (\c v -> c { cLogQueueLength = v }) $
     updM valI "logFlushInterval"   (\c v -> c { cLogFlushInterval = v }) $
     updM valI "wrktSessionClearer" (\c v -> c { cWrktSessionClearer = v }) $
     updM valI "wrktWhoisLooker"    (\c v -> c { cWrktWhoisLooker = v }) $
     updM valI "wrktLogCleaner"     (\c v -> c { cWrktLogCleaner = v }) $
     Just $ ServerConfig {}

configPairsFromText :: T.Text -> [(T.Text, T.Text)]
configPairsFromText t =
  let ls = map (T.filter configFilter) $
           dropWhile (/= "{") $
           dropWhileEnd (/= "}" ) $
           map T.strip (T.lines t)
  in catMaybes $ map parseLine ls

configFilter :: Char -> Bool
configFilter c = (c /= '\"') && (c /= ',')

parseLine :: T.Text -> Maybe (T.Text, T.Text)
parseLine l = case T.splitOn ":" l of
  [k, v] -> Just (T.strip k, T.strip v)
  _ -> Nothing


-- --------------------------------------------------------------------------
-- Application Wrappers

runStandardRedirecter :: ServerConfig -> ILogQueue -> IO ()
runStandardRedirecter cfg ilq =
  runSettings (warpRedirectSettings cfg ilq) $
    withCommonHeaders $
    withCertbotACMEHandler ilq (cStaticDirPath cfg) $
      -- Removed all middleware checks, this should be handled by
      -- the main app once redirected
--    withResponseTimeoutCheck (cfgI "responseTimeout" cfg) errorPage $
--    withHeaderHostCheck (cfgT "hostName" cfg) errorPage $
--    withMethodCheck errorPage $
--    withContentLengthCheck (cfgI "maxContentLength" cfg) errorPage $
    \req resp -> (resp $ buildFullRedirectResp (cHostName cfg) req)

runStandardApp :: ServerConfig -> ILogQueue -> HLogQueue ->
                  SessionStore -> ErrorPage -> Application -> IO ()
runStandardApp cfg ilq hlq sessionStore errorPage app = do
  let hostName = cHostName cfg
  runTLS (warpTLSSettings cfg) (warpMainSettings cfg ilq) $
    withRevision (cRevision cfg) $
    withRequestLogging hlq $
    withCommonHeaders $
    withHeadRequestStripping $
    withResponseTimeoutCheck (cResponseTimeout cfg) errorPage $
    withHeaderHostCheck hostName errorPage $
    withMethodCheck errorPage $
    withContentLengthCheck (cMaxContentLength cfg) errorPage $
    withAuth ilq sessionStore (cCookieName cfg) $
    (if hostName == "localhost" then withDev errorPage else id) $
    withStaticFileHandler (cStaticDirPath cfg) $
    app

logStartup :: Bool -> IO ()
logStartup success = do
  now <- getCurrentTime
  let level = if success then ILInfo else ILCrit
  let msg = "startup: " <> if success then "success" else "failure"
  let ld = ILogData now level ITOther msg
  logDirect stdout (ld { ildMessage = msg <> " out=stdout"})
  logDirect stderr (ld { ildMessage = msg <> " out=stderr"})


-- --------------------------------------------------------------------------
-- Warp Settings

warpRedirectSettings :: ServerConfig -> ILogQueue -> Settings
warpRedirectSettings c ilq =
  setPort (cHttpPort c) $
  setServerName (encodeUtf8 $ cServerName c <> "-r") $
  setOnException (handleRedirectException ilq) $
  setOnExceptionResponse serveExceptionResponse $
  defaultSettings

warpMainSettings :: ServerConfig -> ILogQueue -> Settings
warpMainSettings c ilq =
  setPort (cHttpsPort c) $
  setServerName (encodeUtf8 $ cServerName c) $
  setOnException (handleWarpException ilq) $
  setOnExceptionResponse serveExceptionResponse $
  defaultSettings

warpTLSSettings :: ServerConfig -> TLSSettings
warpTLSSettings c = defaultTlsSettings {
  certFile = T.unpack $ cSslCertPath c
, keyFile = T.unpack $ cSslKeyPath c
, onInsecure = DenyInsecure et }
  where et = "sebsmpu accepts only secure connections via HTTPS at this " <>
             "port. Please upgrade!"


-- ------------------------------------------------------------------------
-- Exceptions handling for warp-internal

handleWarpException :: ILogQueue -> Maybe Request -> SomeException -> IO ()
handleWarpException ilq _ e =
  logEnqueue ilq $ mkILogData ILError ITWarp ("warp: " <> (T.pack $ show e))

handleRedirectException :: ILogQueue -> Maybe Request -> SomeException -> IO ()
handleRedirectException ilq _ e =
  logEnqueue ilq $ mkILogData ILError ITWarp ("warpre: " <> (T.pack $ show e))

-- Basically same as Warp's defaultOnExceptionResponse
-- TODO: Handle HTTP2 ConnectionError like in ^
serveExceptionResponse :: SomeException -> Response
serveExceptionResponse e = case fromException e of
  (Just _ :: Maybe InvalidRequest) ->
    buildPlainResp status400 $ es <> "Bad request."
  _ ->
    buildPlainResp status500 $ es <> "Internal Server Error."
  where es = "sebsmpu encountered an exception: "


