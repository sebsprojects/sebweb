module Sebweb.Response (
    serveStaticContent
  , serveGenericLogh
  , serveGenericLogi

  , processLogoutRequest
  , processLoginRequest
) where

import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Text.Read as TR (decimal)
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.Maybe
import Network.Wai
import Network.HTTP.Types
import Text.Blaze.Html (Html)

import Sebweb.Utils
import Sebweb.Common
import Sebweb.Session
import Sebweb.ResponseCommon
import Sebweb.ToggleQuery
import Sebweb.Log
import Sebweb.LogI
import Sebweb.LogH
import Sebweb.ViewLogh
import Sebweb.ViewLogi


-- ------------------------------------------------------------------------
-- GET Handler

serveStaticContent :: T.Text -> T.Text -> RequestData -> ErrorPage ->
                      (RequestData -> T.Text -> Html) -> IO Response
serveStaticContent contDir ext rd errorPage htmlBuilder = do
  let fp = T.unpack $ contDir <> "/" <> (rdPath rd) <> "." <> ext
  -- TODO: FILE IO
  putStrLn $ "Serving static content at " <> fp
  fe <- doesFileExist fp
  case fe of
    True -> do
      -- TODO: FILE IO
      content <- TI.readFile fp
      return $ buildHtmlResp' (htmlBuilder rd content)
    False -> do
      putStrLn $ "File does not exist"
      return $ htmlErrorResp' errorPage rd status404

-- TODO: Imposes simpleQueryString
-- TODO: rename loghDefault to hlogDefault
-- TODO: default vs. definition of a tqs
-- TODO: tqsFromQuery should be one function specific for each tqs in question
-- TODO: Should this be 404 on auth error
serveGenericLogh :: ILogQueue -> T.Text -> RequestData ->
                    ErrorPage -> HtmlWrapper -> IO Response
serveGenericLogh ilq logDir rd errorPage htmlWrapper = do
  case rdAuth rd of
    AuthAuthorized _ -> do
      let tqs = (tqsFromQuery (rdSQ rd) loghDefaultTqs)
      hlQuery <- hlqFromTqs tqs
      hlData <- hLogPerformQuery logDir hlQuery
      return $ buildHtmlResp' (siteLogh rd htmlWrapper hlData tqs)
    _ -> return $ htmlErrorResp' errorPage rd status404

serveGenericLogi :: ILogQueue -> T.Text -> RequestData ->
                    ErrorPage -> HtmlWrapper -> IO Response
serveGenericLogi ilq logDir rd errorPage htmlWrapper = do
  case rdAuth rd of
    AuthAuthorized _ -> do
      let tqs = (tqsFromQuery (rdSQ rd) logiDefaultTqs)
      ilQuery <- ilqFromTqs tqs
      ilData <- iLogPerformQuery logDir ilQuery
      putStrLn $ show ilData
      return $ buildHtmlResp' (siteLogi rd htmlWrapper ilData tqs)
    _ -> return $ htmlErrorResp' errorPage rd status404

-- TODO: THis should not be here
hlqFromTqs :: ToggleQueryState -> IO HLogQuery
hlqFromTqs tqs = do
  now <- getCurrentTime
  let dMult = readIntDef 1 (extractFirstTqi 0 tqs)
  let d = addUTCTime (fromIntegral (dMult * (-86400) :: Int)) now
  let status = fillStatus $ extractFirstTqi 1 tqs
  let method = fillMethod $ extractFirstTqi 2 tqs
  let static = fillStatic $ extractFirstTqi 3 tqs
  return $ HLogQuery d status method static
  where fillStatus "2xx" = ["200"]
        fillStatus "3xx" = ["301", "303", "304"]
        fillStatus "4xx" = ["400", "404", "405", "413", "415"]
        fillStatus "5xx" = ["500", "503"]
        fillStatus _ = []
        fillStatic "all" = (True, True)
        fillStatic "1" = (False, True)
        fillStatic _ = (True, False)
        fillMethod "all" = ["HEAD", "GET", "POST", "PUT", "DELETE",
                            "OPTIONS", "CONNECT", "TRACE", "PATCH"]
        fillMethod m = [m]

ilqFromTqs :: ToggleQueryState -> IO ILogQuery
ilqFromTqs tqs = do
  now <- getCurrentTime
  let dMult = readIntDef 1 (extractFirstTqi 0 tqs)
  let d = addUTCTime (fromIntegral (dMult * (-86400) :: Int)) now
  let ls = catMaybes $ map textToIL $ extractAllTqis 1 tqs
  let ts = catMaybes $ map textToIT $ extractAllTqis 2 tqs
  return $ ILogQuery d ls ts

readIntDef :: Int -> T.Text -> Int
readIntDef d t = either (\_ -> d) fst (TR.decimal t)

-- ------------------------------------------------------------------------
-- POST Handler

processLogoutRequest :: ILogQueue -> SessionStore -> T.Text -> T.Text ->
                        UserAccess -> IO Response
processLogoutRequest ilq sessionStore loginURL cookieName userAccess = do
  now <- getCurrentTime
  resp <- case userAccess of
    AuthAuthorized userName -> do
      removeSessionByUser sessionStore userName
      logl ILInfo ("logout: successful for user " <> userName)
      return $ buildRedirectResp "/"
    _ -> do
      logl ILCrit ("logout: unauthed logout attempt " <>
                  (T.pack $ show userAccess))
      return $ buildRedirectResp $ "/" <> loginURL
  return $ setCookieIfUnset (emptyCookieBS cookieName now) resp
  where logl level msg = logEnqueue ilq $ mkILogData level ITAuth msg


-- Assumptions for the login process:
--      # URL for failed login redirects to /loginURL?f=1
--      # query parameters: name, pass, ref
--      # body length limit of 128 bytes
processLoginRequest :: ILogQueue -> SessionStore -> T.Text -> T.Text ->
                       T.Text -> Int -> Request -> IO Response
processLoginRequest ilq sessionStore usersPath loginURL
                    cookieName sessionDuration req = do
  mBody <- readRequestBody bodyLengthLimit req
  now <- getCurrentTime
  case mBody of
    Just b -> do
      let toks = parseSimpleQuery b
      case (lookup "name" toks, lookup "pass" toks, lookup "ref" toks) of
        (Just name, Just pass, mref) -> do
          nameText <- case TE.decodeUtf8' name of
            Right t -> return t
            Left _ -> do
              logl ILCrit "login: utf8-decoding for form-name failed"
              return ""
          userExists <- doesUserExist nameText usersPath
          case userExists of
            True -> do
              userAccess <- validateUser nameText pass usersPath
              case userAccess of
                AuthAuthorized _ -> do
                  logl ILInfo $ "login: successful for user " <> nameText
                  let mpref = decodeUtf8Ignore (fromMaybe "" mref)
                  tok <- generateRandom64
                  insertSession sessionStore tok nameText sessionDuration
                  let cookieBS = mkCookie now tok
                  return $ setCookieIfUnset cookieBS $
                    buildRedirectResp $ "/" <> mpref
                _ -> do
                  logl ILError $ "login: invalid password for user " <> nameText
                  return $ buildRedirectResp $ "/" <> loginURL <> "?f=1"
            False -> do
              logl ILError $ "login: invalid user name " <> nameText
              return $ buildRedirectResp $ "/" <> loginURL <> "?f=1"
        f -> do
          logl ILCrit $ "login: invalid form, fields missing " <>
                        T.pack (show f)
          return $ jsonErrorResp' status400
    Nothing -> do
      logl ILCrit $ "login: invalid form, body length exceeds limit " <>
                    T.pack (show bodyLengthLimit)
      return $ jsonErrorResp' status413
  where logl level msg = logEnqueue ilq $ mkILogData level ITAuth msg
        bodyLengthLimit = 128
        mkCookie now tok = TE.encodeUtf8 $ assembleCookie cookieName tok
                                           (addSeconds sessionDuration now)
                                           sessionDuration
