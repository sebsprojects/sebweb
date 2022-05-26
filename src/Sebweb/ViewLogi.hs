module Sebweb.ViewLogi (
    siteLogi
) where

import Prelude hiding (head, id, div)
import Data.Time
import Data.Maybe
import Data.Ord (comparing)
import Data.List (sortBy)
import qualified Data.Text as T
import Text.Blaze.Html5 hiding (style, map, code)
import Text.Blaze.Html5.Attributes hiding (form, title, max, summary)

import Sebweb.Utils
import Sebweb.Common
import Sebweb.ToggleQuery
import Sebweb.LogI


siteLogi :: RequestData -> (Html -> Html) -> [ILogData] -> ToggleQueryState ->
            Html
siteLogi rd htmlWrapper logData tqs = htmlWrapper $ div ! class_ "sel-cont" $ do
  ""
