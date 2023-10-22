module Sebweb.Middleware (
  -- Request Augmentation
    withRevision
  , withAuth
  , withRequestLogging

  -- Request / Response Checks
  , withHeaderHostCheck
  , withMethodCheck
  , withContentLengthCheck
  , withResponseTimeoutCheck

  -- Response Augmentation
  , withCommonHeaders
  , withHeadRequestStripping
  , withDev

  -- Standalone Handlers
  , withCertbotACMEHandler
  , withStaticFileHandler
) where

import System.Timeout (timeout)
import System.Directory
import qualified Data.Text as T
import qualified Data.ByteString.Builder as BBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Text.Encoding
import Data.Maybe
import qualified Data.Vault.Lazy as Vault
import Data.Time
import Network.Wai
import Network.Wai.Internal
import Network.HTTP.Types
import Network.HTTP.Types.Header
import Network.Socket

import Sebweb.Utils
import Sebweb.Common
import Sebweb.Log
import Sebweb.LogI
import Sebweb.LogH
import Sebweb.Session
import Sebweb.ResponseCommon


-- ------------------------------------------------------------------------
-- Request Augmentation

withRevision :: T.Text -> Middleware
withRevision rev app req respond = do
  let v = Vault.insert revKey rev (vault req)
  app (req { vault = v }) respond

-- TODO: Re-set cookie if invalid / expired
withAuth :: ILogQueue -> SessionStore -> T.Text -> Middleware
withAuth ilq ss cn app req respond = do
  let mCookieText = fmap decodeUtf8' (lookup hCookie (requestHeaders req))
  case mCookieText of
    Just (Right cookieText) -> do
      let cookies = parseCookieText cookieText
      case lookup cn cookies of
        Just v -> do
          mSess <- querySessionStore ss v
          now <- getCurrentTime
          case mSess of
            Just s -> do
              case isExpiredSession now s of
                False -> do
                  insertAuthAndPass (AuthAuthorized (sUser s))
                True -> do
                  logAuth ILInfo "auth: session expired"
                  removeSessionByToken ss v
                  insertAuthAndResetCookie now AuthUnauthorized
            Nothing -> do
              logAuth ILInfo "auth: session not found"
              insertAuthAndResetCookie now AuthUnauthorized
        Nothing -> do
          logAuth ILCrit $ "auth: invalid cookie: " <> T.take 64 cookieText
          insertAuthAndPass AuthVisitor
    Just (Left _) -> do
      logAuth ILCrit "auth: invalid cookie: error on decodeUtf8"
      insertAuthAndPass AuthVisitor
    Nothing -> insertAuthAndPass AuthVisitor
  where insertAuthAndPass auth = do
          let v = Vault.insert authKey auth (vault req)
          app (req { vault = v }) respond
        insertAuthAndResetCookie now auth = do
          let v = Vault.insert authKey auth (vault req)
          app (req { vault = v }) $
            respond . setCookieIfUnset (emptyCookieBS cn now)
        logAuth level msg = logEnqueue ilq $ mkILogData level ITAuth msg

withRequestLogging :: HLogQueue -> Middleware
withRequestLogging hlq app req respond = app req $ \r -> do
  mHostName <- getHostName req
  logEnqueue hlq $ mkHLogData
    mHostName
    ("HTTP/" <> versionToText (httpVersion req))
    (decodeUtf8Ignore $ requestMethod req)
    (decodeUtf8Ignore $ rawPathInfo req)
    (T.pack $ show $ statusCode $ responseStatus r)
    (fmap decodeUtf8Ignore (requestHeaderHost req))
    (fmap decodeUtf8Ignore (requestHeaderReferer req))
    (fmap decodeUtf8Ignore (requestHeaderUserAgent req))
    (isJust $ lookup hCookie (requestHeaders req))
    (maybe False parseDNT (lookup "DNT" $ requestHeaders req))
    Nothing
  respond r
  where parseDNT "1" = True
        parseDNT _ = False
        versionToText (HttpVersion mj mn) = T.pack (show mj <> "." <> show mn)

-- ------------------------------------------------------------------------
-- Request Checks

--   Passes on requests with host header mydomain
--   Redirects www.mydomain to https://mydomain.tdl
--   allowWWW for http-acme challange set to true for http-redirector
--   only to circumvent redirection
--   Serves a 400 error response for any other host
withHeaderHostCheck :: T.Text -> ErrorPage -> Middleware
withHeaderHostCheck hostName err app req respond = do
  case fmap stripPort (requestHeaderHost req) of
    Just hn | hn == hostName -> app req respond
            -- | hn == "www." <> hostName ->
            --   if allowWWW
            --   then app req respond
            --   else respond $ buildFullRedirectResp hostName req
            | otherwise -> respond $ buildErrorResp' err status400 req
    Nothing -> respond $ buildErrorResp' err status400 req
  where stripPort t = safeHead "" (T.splitOn ":" $ decodeUtf8Ignore t)

-- Serves a 405 error for anything not GET, POST or HEAD
withMethodCheck :: ErrorPage -> Middleware
withMethodCheck err app req respond =
  if requestMethod req `elem` allowedMethods
    then app req respond
    else respond $ buildErrorResp' err status405 req
  where allowedMethods = ["GET", "POST", "HEAD"]

-- Only checks the Content-Length header
-- TODO: Also check for ChunkedBody like in wai-extra
withContentLengthCheck :: Int -> ErrorPage -> Middleware
withContentLengthCheck maxByteLen err app req respond =
  case requestBodyLength req of
    KnownLength l -> if fromIntegral l > maxByteLen
                     then respond $ buildErrorResp' err status413 req
                     else app req respond
    ChunkedBody -> app req respond

-- Timeout after some amount of time has passed
withResponseTimeoutCheck :: Int -> ErrorPage -> Middleware
withResponseTimeoutCheck seconds err app req respond = do
  mResponseReceived <- timeout (seconds * 1000000) (app req respond)
  case mResponseReceived of
    Nothing -> respond $ buildErrorResp' err status503 req
    Just rr -> return rr


-- ------------------------------------------------------------------------
-- Response Augmentation

withCommonHeaders :: Middleware
withCommonHeaders app req respond = app req (respond . addHs)
  where addHs = addResponseHeaders [ ("Content-Security-Policy", csp)
                                   , ("Strict-Transport-Security", sts)
                                   , ("Tk", "N") ]
        csp = "default-src 'self'; " <>
              "style-src 'self'; " <>
              "img-src 'self'; " <>
              "form-action 'self'; " <>
              "connect-src 'none'; "
        sts = "max-age=31536000"

-- If we go a HEAD request, strip the response body
withHeadRequestStripping :: Middleware
withHeadRequestStripping app req respond = case requestMethod req of
  "HEAD" -> app req $ respond . stripResponseBody
  _ -> app req respond
  where stripResponseBody r = responseBuilder (responseStatus r)
                              (responseHeaders r) mempty

withDev :: ErrorPage -> Middleware
withDev err app req respond = do
  mHostName <- getHostName req
  case mHostName of
    Just "127.0.0.1" -> do
      let v = Vault.insert authKey (AuthAuthorized "dev") (vault req)
      app (req { vault = v }) $ \res -> do
        newRes <- case res of
          ResponseBuilder ss hs b -> do
            let bs = LBS.toStrict $ BBS.toLazyByteString b
            let (beforeBody, body) = BS.breakSubstring "<body>" bs
            if BS.null body
              then return res
              else do let builder = BBS.byteString beforeBody <>
                                    BBS.byteString "<body>" <>
                                    BBS.byteString devHtml <>
                                    BBS.byteString (BS.drop 6 body)
                      return $ ResponseBuilder ss hs builder
          _ -> return res
        respond newRes
    _ -> do
      respond $ buildErrorResp err status500 "Not 127-y enough" req
  where devHtml = "<div class=\"dev\">" <> "</div>"


-- ------------------------------------------------------------------------
-- Standalone Handlers

withCertbotACMEHandler :: ILogQueue -> [T.Text] -> Middleware
withCertbotACMEHandler ilq staticDirs app req respond = do
  case (isValidPath staticDirs (pathInfo req), pathInfo req) of
    (True, ".well-known" : "acme-challenge" : _) -> do
      let fpth = T.intercalate "/" (pathInfo req)
      -- TODO: FILE IO
      fe <- doesFileExist (T.unpack fpth)
      case fe of
        True -> do
          logACME ILInfo "acme: successfully serving acme challenge"
          -- TODO: Add headers?
          respond $ fileResponse fpth []
        False -> do
          logACME ILCrit ("acme: challenge invalid file path: " <>
                          safeLast "" (pathInfo req))
          app req respond
    _ -> app req respond
  where logACME l m = logEnqueue ilq $ mkILogData l ITOther m

-- TODO: use rawPath instead, especially for valid path?
-- Use staticDirPaths as a check only and base the file request on the
-- "server root" dir instead
withStaticFileHandler :: [T.Text] -> Middleware
withStaticFileHandler staticDirs app req respond = do
  let suff = fromMaybe "" $ extractPathSuffix' (pathInfo req)
  if isStaticSuffix suff && isValidPath staticDirs (pathInfo req)
    then do
      let fpth = T.intercalate "/" (pathInfo req)
      -- TODO: FILE IO
      fe <- doesFileExist (T.unpack fpth)
      case (fe, isWebSuffix suff) of
        (True, True) -> do
          now <- getCurrentTime
          respond $ addResponseHeaders (ctHeader suff) $ serveWebFile fpth now
        (True, False) -> do
          -- TODO: FILE IO
          now <- getCurrentTime
          modTim <- getModificationTime (T.unpack fpth)
          respond $ addResponseHeaders (ctHeader suff) $
                    serveStaticFile fpth now modTim (requestHeaders req)
        _ -> app req respond
    else app req respond

serveWebFile :: T.Text -> UTCTime -> Response
serveWebFile fpth now = fileResponse fpth hs
  where hs = [(hCacheControl, "max-age=31536000, immutable")] ++
             [(hExpires, B8.pack $ formatTimeHttp $ addSeconds 31536000 now)]

serveStaticFile :: T.Text -> UTCTime -> UTCTime -> RequestHeaders -> Response
serveStaticFile fpth now modTim reqHs =
  case (lookup hIfNoneMatch reqHs, mGetModSince reqHs) of
    (Just clientEtag, _) ->
      condFileResponse (gendEtag `elem` parseETag clientEtag) fpth hs
    (_, Just clientMod) ->
      condFileResponse (diffUTCTime modTim clientMod <= 1.0) fpth hs
    _ -> fileResponse fpth hs
  where mGetModSince = compMF2 (parseTimeHttp . B8.unpack)
                               (lookup hIfModifiedSince)
        hs = staticFileHeaders etagString modTim now
        gendEtag = B8.pack $ formatTime defaultTimeLocale "%Y%m%d%H%M%S" modTim
        etagString = "W/\"" <> gendEtag <> "\""

-- TODO: Deal with comma-separated lists and edge cases
parseETag :: BS.ByteString -> [BS.ByteString]
parseETag bs =
  let bs1 = applyIgnoreNothing (B8.stripPrefix "W/") bs
      bs2 = compMF2 (B8.stripPrefix "\"") (B8.stripSuffix "\"") bs1
  in maybeToList bs2


fileResponse :: T.Text -> [Header] -> Response
fileResponse fpth hs = responseFile status200 hs (T.unpack fpth) Nothing

-- If True, the cache-condition matched and we send a 304
condFileResponse :: Bool -> T.Text -> [Header] -> Response
condFileResponse True _ hs = addResponseHeaders hs $ buildEmptyResp status304
condFileResponse False fpth hs = fileResponse fpth hs

staticFileHeaders :: BS.ByteString -> UTCTime -> UTCTime -> [Header]
staticFileHeaders etag modTim now =
  [(hCacheControl, "private, max-age=86400")] ++
  --[(hCacheControl, "no-cache")] ++
  [(hExpires, B8.pack $ formatTimeHttp $ addSeconds 86400 now)] ++
  [(hETag, etag)] ++
  [(hLastModified, B8.pack $ formatTimeHttp modTim)]

getHostName :: Request -> IO (Maybe T.Text)
getHostName req = do
  (mHostName, _) <- getNameInfo [NI_NUMERICHOST] True False (remoteHost req)
  return $ fmap T.pack mHostName

isValidPath :: [T.Text] -> [T.Text] -> Bool
isValidPath pth staticDirs = not (T.isPrefixOf "/" (safeHead "" pth)) &&
    not (any (T.isInfixOf "..") pth)

ctHeader :: T.Text -> [Header]
ctHeader "css" = [(hContentType, "text/css")]
ctHeader "js"  = [(hContentType, "text/javascript")]
ctHeader "txt" = [(hContentType, "text/plain")]
ctHeader "svg" = [(hContentType, "image/svg+xml")]
ctHeader "png" = [(hContentType, "image/png")]
ctHeader "jpg" = [(hContentType, "image/jpeg")]
ctHeader "pdf" = [(hContentType, "application/pdf")]
ctHeader "woff2" = [(hContentType, "font/woff2")]
ctHeader _ = []

