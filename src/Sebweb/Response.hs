module Sebweb.Response (
    buildEmptyResp
  , buildJsonResp
  , buildHtmlResp
  , buildHtmlResp'
  , buildPlainResp
  , buildRedirectResp
  , buildFullRedirectResp
) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import Data.Binary.Builder (empty, fromByteString)
import Network.Wai
import Network.HTTP.Types
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)


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

