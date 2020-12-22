module Sebweb.ResponseError (
    ErrorPage
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
import Sebweb.Response


type ErrorPage = Status -> Maybe T.Text -> Maybe T.Text -> Html


errorResp:: ErrorPage -> Status -> Maybe T.Text -> Maybe T.Text -> Response
errorResp ehtml status (Just "GET") mmsg = htmlErrorResp ehtml status mmsg
errorResp _ status (Just "POST") mmsg = jsonErrorResp status mmsg
errorResp _ status _ mmsg = plainErrorResp status mmsg

htmlErrorResp :: ErrorPage -> Status -> Maybe T.Text -> Response
htmlErrorResp errorPage status mmsg =
  buildHtmlResp status (errorPage status (Just "GET") mmsg)

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

