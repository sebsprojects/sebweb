{-# LANGUAGE ScopedTypeVariables #-}

module Sebweb.Config (
    ServerConfig
  , configTextOrErr
  , configIntOrErr
  , configFromFile
  , configFromText

  , warpRedirectSettings
  , warpMainSettings
  , warpTLSSettings
) where

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

type ServerConfig = [(T.Text, T.Text)]


-- ------------------------------------------------------------------------
-- Config Parsing (subset of JSON supporting only int and string)

configTextOrErr :: T.Text -> ServerConfig -> T.Text
configTextOrErr key config = case lookup key config of
  Nothing -> error $ "config does not contain key=" <> T.unpack key
  Just val -> val

configIntOrErr :: T.Text -> ServerConfig -> Int
configIntOrErr key config = case fmap T.unpack (lookup key config) of
  Nothing -> error $ "config does not contain key=" <> T.unpack key
  Just val -> case readMaybe val of
    Nothing -> error $ "config no int from val=" <> val <> " for key=" <> 
                       T.unpack key
    Just ival -> ival

-- TODO: FILE IO
configFromFile :: FilePath -> IO ServerConfig
configFromFile fp = TIO.readFile fp >>= return . configFromText

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
-- Warp Settings

warpRedirectSettings :: ServerConfig -> LogQueue -> Settings
warpRedirectSettings c ilq =
  setPort (configIntOrErr "httpPort" c) $
  setServerName (encodeUtf8 $ configTextOrErr "serverName" c <> "-r") $
  setOnException (handleRedirectException ilq) $
  setOnExceptionResponse serveExceptionResponse $
  defaultSettings

warpMainSettings :: ServerConfig -> LogQueue -> Settings
warpMainSettings c ilq =
  setPort (configIntOrErr "httpsPort" c) $
  setServerName (encodeUtf8 $ configTextOrErr "serverName" c) $
  setOnException (handleWarpException ilq) $
  setOnExceptionResponse serveExceptionResponse $
  defaultSettings

warpTLSSettings :: ServerConfig -> TLSSettings
warpTLSSettings c = defaultTlsSettings {
  certFile = T.unpack $ configTextOrErr "sslCertPath" c
, keyFile = T.unpack $ configTextOrErr "sslKeyPath" c
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


