module Sebweb.View (
    makeRelativeLink
  , makeRevisionedLink
) where

import Prelude hiding (div, id, head)
import qualified Data.Text as T
--import Text.Blaze.Html5 hiding (style, map, code)
--import Text.Blaze.Html5.Attributes hiding (form, title, max)


makeRelativeLink :: T.Text -> T.Text -> T.Text
makeRelativeLink pth ln = pref <> "/" <> ln
  where pref = T.intercalate "/" (replicate n "..")
        n = max 0 ((length $ T.splitOn "/" pth ) - 1)

makeRevisionedLink :: T.Text -> T.Text -> T.Text
makeRevisionedLink rev ln = ln <> "?rev=" <> rev
