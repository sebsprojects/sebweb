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
import Data.Default.Class (def)
import Data.Maybe
import Data.Time
import Data.List (dropWhileEnd)
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
import Network.TLS


data ServerConfig = ServerConfig {
  cRevision :: T.Text

, cHostName :: T.Text
, cServerName :: T.Text
, cHttpPort :: Int
, cHttpsPort :: Int

, cSslCertFile :: T.Text
, cSslKeyFile :: T.Text

, cSslSniPath :: T.Text
, cSslSniHosts :: [T.Text]

, cUsersDirPath :: T.Text
, cLogDirPath :: T.Text
, cStaticDirPaths :: [T.Text]

, cCookieName :: T.Text
, cMaxSessionAge :: Int
, cMaxContentLength :: Int
, cResponseTimeout :: Int

, cLogQueueLength :: Int
, cLogFlushInterval :: Int

, cWrktSessionClearer :: Int
, cWrktWhoisLooker :: Int
, cWrktLogCleaner :: Int
} deriving (Show)


-- ------------------------------------------------------------------------
-- Config Parsing (subset of JSON supporting only int and string)

cfgT :: T.Text -> [(T.Text, T.Text)] -> Maybe T.Text
cfgT key config = fmap (T.strip . T.filter (/= '\"')) (lookup key config)

cfgI :: T.Text -> [(T.Text, T.Text)] -> Maybe Int
cfgI key config = compMF2 readMaybe (fmap T.unpack) (lookup key config)

cfgL :: T.Text -> [(T.Text, T.Text)] -> Maybe [T.Text]
cfgL key = compMF2 parseList (lookup key)

parseList :: T.Text -> Maybe [T.Text]
parseList t = case T.uncons (T.strip t) of
  Just ('[', t1) -> case T.unsnoc t1 of
    Just (t2, ']') ->
      let toks = map (T.strip . T.filter (/= '\"')) (T.split (== ',') t2)
      in Just $ filter (not . T.null) toks
    _ -> Nothing
  _ -> Nothing

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
      valL key f = fmap f (cfgL key dict)
      fm = maybe Nothing
      updM valType key updf = fm (valType key . updf)
  in updM valT "revision"           (\c v -> c { cRevision = v }) $
     updM valT "hostName"           (\c v -> c { cHostName = v }) $
     updM valT "serverName"         (\c v -> c { cServerName = v }) $
     updM valI "httpPort"           (\c v -> c { cHttpPort = v }) $
     updM valI "httpsPort"          (\c v -> c { cHttpsPort = v }) $
     updM valT "sslCertFile"        (\c v -> c { cSslCertFile = v }) $
     updM valT "sslKeyFile"         (\c v -> c { cSslKeyFile = v }) $
     updM valT "sslSniPath"         (\c v -> c { cSslSniPath = v }) $
     updM valL "sslSniHosts"        (\c v -> c { cSslSniHosts = v }) $
     updM valT "usersDirPath"       (\c v -> c { cUsersDirPath = v }) $
     updM valT "logDirPath"         (\c v -> c { cLogDirPath = v }) $
     updM valL "staticDirPaths"     (\c v -> c { cStaticDirPaths = v }) $
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
  let ls = dropWhile (/= "{") $
           dropWhileEnd (/= "}" ) $
           map T.strip (T.lines t)
  in mapMaybe parseLine ls

parseLine :: T.Text -> Maybe (T.Text, T.Text)
parseLine l = case T.splitOn ":" l of
  [k, v] -> Just (T.filter (/= '\"') (T.strip k),
                  T.dropWhileEnd (== ',') (T.strip v))
  _ -> Nothing


-- --------------------------------------------------------------------------
-- Application Wrappers

runStandardRedirecter :: ServerConfig -> ILogQueue -> IO ()
runStandardRedirecter cfg ilq =
  runSettings (warpRedirectSettings cfg ilq) $
    withCommonHeaders $
    withCertbotACMEHandler ilq (cStaticDirPaths cfg) $
      -- Removed all middleware checks, this should be handled by
      -- the main app once redirected
      -- withResponseTimeoutCheck (cfgI "responseTimeout" cfg) errorPage $
      -- withHeaderHostCheck (cfgT "hostName" cfg) errorPage $
      -- withMethodCheck errorPage $
      -- withContentLengthCheck (cfgI "maxContentLength" cfg) errorPage $
    \req resp -> resp $ buildFullRedirectResp (cHostName cfg) req

runStandardApp :: ServerConfig -> ILogQueue -> HLogQueue ->
                  SessionStore -> ErrorPage -> Application -> IO ()
runStandardApp cfg ilq hlq sessionStore errorPage app = do
  let hostName = cHostName cfg
  sniLookup <- readSniFiles cfg
  runTLS (warpTLSSettings cfg sniLookup) (warpMainSettings cfg ilq) $
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
    withStaticFileHandler (cStaticDirPaths cfg) app

logStartup :: Bool -> IO ()
logStartup success = do
  now <- getCurrentTime
  let level = if success then ILInfo else ILCrit
  let msg = "startup: " <> if success then "success" else "failure"
  let ld = ILogData now level ITOther msg
  logDirect stdout (ld { ildMessage = msg <> " out=stdout"})
  logDirect stderr (ld { ildMessage = msg <> " out=stderr"})

readSniFiles :: ServerConfig -> IO [(Maybe HostName, Credentials)]
readSniFiles cfg = do
  -- TODO: Should come from config
  let sniHosts = cSslSniHosts cfg
  let p = cSslSniPath cfg
  eCreds <- mapM (\h -> credentialLoadX509 (T.unpack $ p <> "/" <> h <> ".pem") (T.unpack $ p <> "/" <> h <> ".key")) sniHosts
  let mCreds = map (either (const Nothing) (\c -> Just $ Credentials [c])) eCreds
  let sniMap = zip [Just $ T.unpack h | h <- sniHosts] mCreds
  return $ map (\(a, b) -> (a, fromJust b)) $ filter (\(_, b) -> isJust b) sniMap
  

-- --------------------------------------------------------------------------
-- Warp Settings

warpRedirectSettings :: ServerConfig -> ILogQueue -> Settings
warpRedirectSettings c ilq =
  setPort (cHttpPort c) $
  setServerName (encodeUtf8 $ cServerName c <> "-r") $
  setOnException (handleRedirectException ilq) $
  setOnExceptionResponse serveExceptionResponse defaultSettings

warpMainSettings :: ServerConfig -> ILogQueue -> Settings
warpMainSettings c ilq =
  setPort (cHttpsPort c) $
  setServerName (encodeUtf8 $ cServerName c) $
  setOnException (handleWarpException ilq) $
  setOnExceptionResponse serveExceptionResponse defaultSettings

warpTLSSettings :: ServerConfig -> [(Maybe HostName, Credentials)] -> TLSSettings
warpTLSSettings cfg sniLookup = 
  (tlsSettings (T.unpack $ cSslCertFile cfg) (T.unpack $ cSslKeyFile cfg)) {
      onInsecure = DenyInsecure m
    , tlsServerHooks = def { onServerNameIndication = \h -> return $ fromMaybe mempty $ lookup h sniLookup }
  }
  where m = "sebsmpu accepts only secure connections via HTTPS at this " <>
            "port. Please upgrade!"


-- ------------------------------------------------------------------------
-- Exceptions handling for warp-internal

handleWarpException :: ILogQueue -> Maybe Request -> SomeException -> IO ()
handleWarpException ilq _ e =
  logEnqueue ilq $ mkILogData ILError ITWarp ("warp: " <> T.pack (show e))

handleRedirectException :: ILogQueue -> Maybe Request -> SomeException -> IO ()
handleRedirectException ilq _ e =
  logEnqueue ilq $ mkILogData ILError ITWarp ("warpre: " <> T.pack (show e))

-- Basically same as Warp's defaultOnExceptionResponse
-- TODO: Handle HTTP2 ConnectionError like in ^
serveExceptionResponse :: SomeException -> Response
serveExceptionResponse e = case fromException e of
  (Just _ :: Maybe InvalidRequest) ->
    buildPlainResp status400 $ es <> "Bad request."
  _ ->
    buildPlainResp status500 $ es <> "Internal Server Error."
  where es = "sebsmpu encountered an exception: "