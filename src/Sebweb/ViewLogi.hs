module Sebweb.ViewLogi (
    siteLogi
) where

import Prelude hiding (head, id, div)
import Data.Time
import Text.Blaze.Html5 hiding (style, map, code)
import Text.Blaze.Html5.Attributes hiding (form, title, max, summary)

import Sebweb.ToggleQuery
import Sebweb.LogI

mkLogLine :: ILogData -> Html
mkLogLine ild = div ! class_ "log-line" $ do
  let tim = ildTime ild
  div ! class_ (cl "tim") $ toHtml (formatTime defaultTimeLocale "%T" tim)
  div ! class_ (cl "lv") $ toHtml $ show (ildLevel ild)
  div ! class_ (cl "typ") $ toHtml $ show (ildType ild)
  div ! class_ (cl "msg") $ toHtml $ ildMessage ild
  where cl typ = textValue ("log-report-field log-report-item-" <> typ)

siteLogi :: (Html -> Html) -> [ILogData] -> ToggleQueryState -> Html
siteLogi htmlWrapper logData tqs = htmlWrapper $ div ! class_ "sel-cont" $ do
  div ! class_ "sel-section" $ do
    tglLink tqs (tglExclM1 0) 0 0 "/logi" "-1d"
    tglLink tqs (tglExclM1 1) 0 1 "/logi" "-2d"
    tglLink tqs (tglExclM1 2) 0 2 "/logi" "-3d"
  div ! class_ "sel-section" $ do
    tglLink tqs (tglAddM1 0) 1 0 "/logi" "Crt"
    tglLink tqs (tglAddM1 1) 1 1 "/logi" "Err"
    tglLink tqs (tglAddM1 2) 1 2 "/logi" "Inf"
  div ! class_ "sel-section" $ do
    tglLink tqs (tglAddM1 0) 2 0 "/logi" "Ath"
    tglLink tqs (tglAddM1 1) 2 1 "/logi" "Wrk"
    tglLink tqs (tglAddM1 2) 2 2 "/logi" "Oth"
    tglLink tqs (tglAddM1 3) 2 3 "/logi" "Wrp"
  div ! class_ "log-report-cont" $ do
    toHtml (map mkLogLine logData)
