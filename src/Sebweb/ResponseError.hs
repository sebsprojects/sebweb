module Sebweb.ResponseError (
    ErrorPage
  , buildErrorResp
  , buildErrorResp'
  , errorResp

  , htmlErrorResp
  , jsonErrorResp
  , plainErrorResp
) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as B8
import Network.Wai
import Network.HTTP.Types
import Text.Blaze.Html (Html)

import Sebweb.Utils
import Sebweb.Common
import Sebweb.Response


type ErrorPage = RequestData -> Status -> Maybe T.Text -> Maybe T.Text -> Html

buildErrorResp' :: ErrorPage -> Status -> Request -> Response
buildErrorResp' err status req =
  buildErrorResp err status (standardErrorMessage $ statusCode status) req

buildErrorResp :: ErrorPage -> Status -> T.Text -> Request -> Response
buildErrorResp errorPage status msg req =
  errorResp errorPage
            (getRequestData req)
            status
            (Just $ decodeUtf8Ignore $ requestMethod req)
            (if T.null msg then Nothing else Just msg)

errorResp :: ErrorPage -> RequestData -> Status -> Maybe T.Text ->
             Maybe T.Text -> Response
errorResp ehtm rd status (Just "GET") mmsg = htmlErrorResp ehtm rd status mmsg
errorResp _ _ status (Just "POST") mmsg = jsonErrorResp status mmsg
errorResp _ _ status _ mmsg = plainErrorResp status mmsg

htmlErrorResp :: ErrorPage -> RequestData -> Status -> Maybe T.Text -> Response
htmlErrorResp errorPage rd status mmsg =
  buildHtmlResp status (errorPage rd status (Just "GET") mmsg)

jsonErrorResp :: Status -> Maybe T.Text -> Response
jsonErrorResp status mmsg = buildJsonResp status $
  "{\n" <>
  "  \"errorCode\": " <> T.pack (show $ statusCode status) <> ",\n" <>
  "  \"errorMessage\": \"" <> decodeUtf8Ignore (statusMessage status) <>
  "\",\n" <>
  case mmsg of
    Nothing -> "}\n"
    Just msg -> "  \"errorInfo\": \"" <> msg <> "\"\n}\n"

plainErrorResp :: Status -> Maybe T.Text -> Response
plainErrorResp status mmsg = buildPlainResp status $
  "HTTP Error\n" <>
  "Error Code: " <> (B8.pack $ show (statusCode status)) <> "\n" <>
  "Error Message: " <> (statusMessage status) <> "\n" <>
  case mmsg of
    Nothing -> ""
    Just msg -> "Error Info: " <> TE.encodeUtf8 msg <> "\n"

standardErrorMessage :: Int -> T.Text
standardErrorMessage 400 = "invalid header host"
standardErrorMessage 405 = "invalid request method"
standardErrorMessage 413 = "maximum request body length exceeded"
standardErrorMessage 503 = "response timeout exceeded"
standardErrorMessage _ = ""

