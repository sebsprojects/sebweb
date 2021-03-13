{-# LANGUAGE ScopedTypeVariables #-}

module Sebweb.Config (
    ServerConfig
  , cfgT
  , cfgI
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

import Sebweb.Log
import Sebweb.LogI
import Sebweb.Response
import Sebweb.ResponseError
import Sebweb.Middleware
import Sebweb.Session

type ServerConfig = [(T.Text, T.Text)]


-- ------------------------------------------------------------------------
-- Config Parsing (subset of JSON supporting only int and string)

cfgT :: T.Text -> ServerConfig -> T.Text
cfgT key config = case lookup key config of
  Nothing -> error $ "config does not contain key=" <> T.unpack key
  Just val -> val

cfgI :: T.Text -> ServerConfig -> Int
cfgI key config = case fmap T.unpack (lookup key config) of
  Nothing -> error $ "config does not contain key=" <> T.unpack key
  Just val -> case readMaybe val of
    Nothing -> error $ "config no int from val=" <> val <> " for key=" <> 
                       T.unpack key
    Just ival -> ival

-- TODO: FILE IO
configFromFile :: FilePath -> IO (Maybe ServerConfig)
configFromFile fp = do
  fe <- doesFileExist fp
  case fe of
    True -> TIO.readFile fp >>= return . Just . configFromText
    False -> return Nothing

configFromText :: T.Text -> ServerConfig
configFromText t =
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

runStandardRedirecter :: ServerConfig -> LogQueue -> ErrorPage -> IO ()
runStandardRedirecter cfg ilq errorPage =
  runSettings (warpRedirectSettings cfg ilq) $
    withCommonHeaders $
    withCertbotACMEHandler ilq (cfgT "staticDirPath" cfg) $
    withResponseTimeoutCheck (cfgI "responseTimeout" cfg) errorPage $
    withHeaderHostCheck (cfgT "hostName" cfg) errorPage $
    withMethodCheck errorPage $
    withContentLengthCheck (cfgI "maxContentLength" cfg) errorPage $
    \req resp -> (resp $ buildFullRedirectResp (cfgT "hostName" cfg) req)

runStandardApp :: ServerConfig -> LogQueue -> LogQueue ->
                  SessionStore -> ErrorPage -> Application -> IO ()
runStandardApp cfg ilq hlq sessionStore errorPage app = do
  let hostName = cfgT "hostName" cfg
  runTLS (warpTLSSettings cfg) (warpMainSettings cfg ilq) $
    withRevision (cfgT "revision" cfg) $
    withRequestLogging hlq $
    withCommonHeaders $
    withHeadRequestStripping $
    withResponseTimeoutCheck (cfgI "responseTimeout" cfg) errorPage $
    withHeaderHostCheck hostName errorPage $
    withMethodCheck errorPage $
    withContentLengthCheck (cfgI "maxContentLength" cfg) errorPage $
    withAuth ilq sessionStore (cfgT "cookieName" cfg) $
    (if hostName == "localhost" then withDev errorPage else id) $
    withStaticFileHandler $
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

warpRedirectSettings :: ServerConfig -> LogQueue -> Settings
warpRedirectSettings c ilq =
  setPort (cfgI "httpPort" c) $
  setServerName (encodeUtf8 $ cfgT "serverName" c <> "-r") $
  setOnException (handleRedirectException ilq) $
  setOnExceptionResponse serveExceptionResponse $
  defaultSettings

warpMainSettings :: ServerConfig -> LogQueue -> Settings
warpMainSettings c ilq =
  setPort (cfgI "httpsPort" c) $
  setServerName (encodeUtf8 $ cfgT "serverName" c) $
  setOnException (handleWarpException ilq) $
  setOnExceptionResponse serveExceptionResponse $
  defaultSettings

warpTLSSettings :: ServerConfig -> TLSSettings
warpTLSSettings c = defaultTlsSettings {
  certFile = T.unpack $ cfgT "sslCertPath" c
, keyFile = T.unpack $ cfgT "sslKeyPath" c
, onInsecure = DenyInsecure et }
  where et = "sebsmpu accepts only secure connections via HTTPS at this " <>
             "port. Please upgrade!"


-- ------------------------------------------------------------------------
-- Exceptions handling for warp-internal

handleWarpException :: LogQueue -> Maybe Request -> SomeException -> IO ()
handleWarpException ilq _ e = do
  now <- getCurrentTime
  logEnqueue ilq $ ILogData now ILError ITWarp ("warp: " <> (T.pack $ show e))

handleRedirectException :: LogQueue -> Maybe Request -> SomeException -> IO ()
handleRedirectException ilq _ e = do
  now <- getCurrentTime
  logEnqueue ilq $ ILogData now ILError ITWarp ("warpre: " <> (T.pack $ show e))

-- Basically same as Warp's defaultOnExceptionResponse
-- TODO: Handle HTTP2 ConnectionError like in ^
serveExceptionResponse :: SomeException -> Response
serveExceptionResponse e = case fromException e of
  (Just _ :: Maybe InvalidRequest) ->
    buildPlainResp status400 $ es <> "Bad request."
  _ ->
    buildPlainResp status500 $ es <> "Internal Server Error."
  where es = "sebsmpu encountered an exception: "


