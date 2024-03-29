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

serveStaticContent :: T.Text -> RequestData -> ErrorPage ->
                      (RequestData -> T.Text -> Html) -> IO Response
serveStaticContent filePath rd errorPage htmlBuilder = do
  -- TODO: FILE IO
  fe <- doesFileExist $ T.unpack filePath
  case fe of
    True -> do
      -- TODO: FILE IO
      content <- TI.readFile $ T.unpack filePath
      return $ buildHtmlResp' (htmlBuilder rd content)
    False -> do
      putStrLn "File does not exist"
      return $ htmlErrorResp' errorPage rd status404

-- TODO: Imposes simpleQueryString
-- TODO: rename loghDefault to hlogDefault
-- TODO: default vs. definition of a tqs
-- TODO: tqsFromQuery should be one function specific for each tqs in question
-- TODO: Should this be 404 on auth error?
serveGenericLogh :: ILogQueue -> T.Text -> RequestData ->
                    ErrorPage -> HtmlWrapper -> IO Response
serveGenericLogh ilq logDir rd errorPage htmlWrapper = do
  case rdAuth rd of
    AuthAuthorized _ -> do
      let tqs = tqsFromQuery (rdSQ rd) loghDefaultTqs
      hlQuery <- hlqFromTqs tqs
      hlData <- hLogPerformQuery logDir hlQuery
      return $ buildHtmlResp' (siteLogh htmlWrapper hlData tqs)
    _ -> do
      logEnqueue ilq $ mkILogData ILCrit ITAuth "logh: unauthed request"
      return $ htmlErrorResp' errorPage rd status404

serveGenericLogi :: ILogQueue -> T.Text -> RequestData ->
                    ErrorPage -> HtmlWrapper -> IO Response
serveGenericLogi ilq logDir rd errorPage htmlWrapper = do
  case rdAuth rd of
    AuthAuthorized _ -> do
      let tqs = tqsFromQuery (rdSQ rd) logiDefaultTqs
      ilQuery <- ilqFromTqs tqs
      ilData <- iLogPerformQuery logDir ilQuery
      print ilData
      return $ buildHtmlResp' (siteLogi htmlWrapper ilData tqs)
    _ -> do
      logEnqueue ilq $ mkILogData ILCrit ITAuth "logi: unauthed request"
      return $ htmlErrorResp' errorPage rd status404


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
      logl ILCrit ("logout: unauthed logout attempt " <> T.pack (show userAccess))
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
