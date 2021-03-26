module Sebweb.ResponseCommon (
    buildEmptyResp
  , buildJsonResp
  , buildHtmlResp
  , buildHtmlResp'
  , buildPlainResp
  , buildRedirectResp
  , buildFullRedirectResp

  , ErrorPage
  , errorResp
  , htmlErrorResp
  , htmlErrorResp'
  , jsonErrorResp
  , jsonErrorResp'
  , plainErrorResp
  , buildErrorResp
  , buildErrorResp'
) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.ByteString.Lazy (fromStrict)
import Data.Binary.Builder (empty, fromByteString)
import Network.Wai
import Network.HTTP.Types
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)

import Sebweb.Utils
import Sebweb.Common


-- ------------------------------------------------------------------------
-- Builders

buildEmptyResp :: Status -> Response
buildEmptyResp s = responseBuilder s [] empty

buildJsonResp :: Status -> T.Text -> Response
buildJsonResp s t = responseBuilder s
  [(hContentType, "application/json")] (fromByteString $ TE.encodeUtf8 t)

buildHtmlResp :: Status -> Html -> Response
buildHtmlResp status h = responseBuilder status [] (renderHtmlBuilder h)

buildHtmlResp' :: Html -> Response
buildHtmlResp' = buildHtmlResp status200

buildPlainResp :: Status -> BS.ByteString -> Response
buildPlainResp s bs =
  responseLBS s [(hContentType, "text/plain")] $ fromStrict bs

buildRedirectResp :: T.Text -> Response
buildRedirectResp target =
  responseBuilder status303 [(hLocation, TE.encodeUtf8 target)] empty

buildFullRedirectResp :: T.Text -> Request -> Response
buildFullRedirectResp host req = 
  responseBuilder status301 [(hLocation, p)] mempty
  where p = "https://" <> TE.encodeUtf8 host <> rawPathInfo req <>
            rawQueryString req


-- ------------------------------------------------------------------------
-- Error Response (general, html, json, plain)

type ErrorPage = RequestData -> Status -> Maybe T.Text -> T.Text -> Html


errorResp :: ErrorPage -> RequestData -> Status -> Maybe T.Text -> T.Text ->
             Response
errorResp err rd status (Just "GET") msg = htmlErrorResp err rd status msg
errorResp _ _ status (Just "POST") msg = jsonErrorResp status msg
errorResp _ _ status _ msg = plainErrorResp status msg

htmlErrorResp :: ErrorPage -> RequestData -> Status -> T.Text -> Response
htmlErrorResp errorPage rd status msg =
  errorResp errorPage rd status (Just "GET") msg

htmlErrorResp' :: ErrorPage -> RequestData -> Status -> Response
htmlErrorResp' err rd status =
  htmlErrorResp err rd status (standardErrorMessage $ statusCode status)

jsonErrorResp :: Status -> T.Text -> Response
jsonErrorResp status msg = buildJsonResp status $
  "{\n" <>
  "  \"errorCode\": " <> T.pack (show $ statusCode status) <> ",\n" <>
  "  \"errorMessage\": \"" <> decodeUtf8Ignore (statusMessage status) <>
  "\",\n" <>
  case T.null msg of
    True -> "}\n"
    False -> "  \"errorInfo\": \"" <> msg <> "\"\n}\n"

jsonErrorResp' :: Status -> Response
jsonErrorResp' status =
  jsonErrorResp status (standardErrorMessage $ statusCode status)

plainErrorResp :: Status -> T.Text -> Response
plainErrorResp status msg = buildPlainResp status $
  "HTTP Error\n" <>
  "Error Code: " <> (B8.pack $ show (statusCode status)) <> "\n" <>
  "Error Message: " <> (statusMessage status) <> "\n" <>
  case T.null msg of
    True -> ""
    False -> "Error Info: " <> TE.encodeUtf8 msg <> "\n"

standardErrorMessage :: Int -> T.Text
standardErrorMessage 400 = "invalid header host"
standardErrorMessage 404 = "not found"
standardErrorMessage 405 = "invalid request method"
standardErrorMessage 413 = "maximum request body length exceeded"
standardErrorMessage 503 = "response timeout exceeded"
standardErrorMessage _ = ""


-- ------------------------------------------------------------------------
-- Error-From-Request Wrapper

buildErrorResp :: ErrorPage -> Status -> T.Text -> Request -> Response
buildErrorResp errorPage status msg req =
  errorResp errorPage (getRequestData req) status
            (Just $ decodeUtf8Ignore $ requestMethod req) msg

buildErrorResp' :: ErrorPage -> Status -> Request -> Response
buildErrorResp' err status req =
  buildErrorResp err status (standardErrorMessage $ statusCode status) req
