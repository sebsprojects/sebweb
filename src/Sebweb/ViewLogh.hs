module Sebweb.ViewLogh (
    siteLogh
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
import Sebweb.LogH


mkLogSection :: HLogData -> Int -> Bool -> Html -> Html
mkLogSection hld num isOpen h = detailTag $ do
  summary $ do
    div ! class_ (cl "nu") $ toHtml (show num)
    div ! class_ (cl "ip") $ toHtml (fromMaybe "hidden IP" $ hldIP hld)
    div ! class_ (cl "co") $ toHtml (fromMaybe "?" $ hldCountry hld)
    div ! class_ (cl "ua") $ toHtml (fromMaybe "unknownUA" $ hldUserAgent hld)
  mkLogLine hld
  h
  where detailTag h = if isOpen then details ! open "" $ h else details  $ h
        cl typ = textValue "log-report-field log-report-item-" <> typ

mkLogLine :: HLogData -> Html
mkLogLine hld = div ! class_ "log-line" $ do
  let tim = hldTime hld
  div ! class_ (cl "tim") $ toHtml (formatTime defaultTimeLocale "%T" tim)
  div ! class_ (cl "meth") $ toHtml (hldMethod hld)
  div ! class_ (cl "stat") $ toHtml (hldStatus hld)
  div ! class_ (cl "hh") $ toHtml (fromMaybe "" $ hldHeaderHost hld)
  div ! class_ (cl "path") $ toHtml (hldPath hld)
  div ! class_ (cl "ref") $ toHtml (fromMaybe "" $ hldReferer hld)
  where cl typ = textValue ("log-report-field log-report-details-" <> typ)

processLine :: [HLogData] -> Bool -> Maybe T.Text -> [Html -> Html] ->
               [Html -> Html]
processLine [] _ _ acc = acc
processLine (hld : logData) isOpen _ [] =
  let cip = hldIP hld
      num = 1 + (length $ filter (\x -> cip == hldIP x) logData)
  in processLine logData isOpen cip [mkLogSection hld num isOpen]
processLine (hld : logData) isOpen lip (a : acc) = case lip == (hldIP hld) of
  True ->
    let appD hld' f h = f (mkLogLine hld' >> h)
    in processLine logData isOpen (hldIP hld) ((appD hld a) : acc)
  False ->
    let cip = hldIP hld
        num = 1 + (length $ filter (\x -> cip == hldIP x) logData)
    in processLine logData isOpen cip ((mkLogSection hld num isOpen) : a : acc)

siteLogh :: RequestData -> (Html -> Html) -> [HLogData] -> ToggleQueryState ->
            Html
siteLogh rd htmlWrapper logData tqs = htmlWrapper $ div ! class_ "sel-cont" $ do
  div ! class_ "sel-section" $ do
    tglLink tqs (tglExclM1 0) 0 0 "/logh" "-1d"
    tglLink tqs (tglExclM1 1) 0 1 "/logh" "-2d"
    tglLink tqs (tglExclM1 2) 0 2 "/logh" "-3d"
  div ! class_ "sel-section" $ do
    tglLink tqs (tglExclM1 0) 1 0 "/logh" "2xx"
    tglLink tqs (tglExclM1 1) 1 1 "/logh" "3xx"
    tglLink tqs (tglExclM1 2) 1 2 "/logh" "4xx"
    tglLink tqs (tglExclM1 3) 1 3 "/logh" "5xx"
  div ! class_ "sel-section" $ do
    tglLink tqs (tglExclM1 0) 2 0 "/logh" "GET"
    tglLink tqs (tglExclM1 1) 2 1 "/logh" "POST"
    tglLink tqs (tglExclM1 2) 2 2 "/logh" "all"
  div ! class_ "sel-section" $ do
    tglLink tqs (tglExclM1 0) 3 0 "/logh" "-s"
    tglLink tqs (tglExclM1 1) 3 1 "/logh" "+s"
    tglLink tqs (tglExclM1 2) 3 2 "/logh" "all"
  div ! class_ "sel-section" $ do
    div ! class_ "sel-item" $
      a ! href (textValue $ "/logh" <> tqsToQueryString loghDefaultTqs) $ "d"
    div ! class_ "sel-item" $
      a ! href (textValue $ "/logh" <> tqsToQueryString tqs400) $ "e"
    div ! class_ "sel-item" $
      a ! href (textValue $ "/logh" <> tqsToQueryString tqs500) $ "E"
  div ! class_ "sel-details" $ div ! class_ "sel-item" $
      a ! href (textValue $ "/logh" <> tqsToQueryString tqsDetails) $ "details"
  div ! class_ "log-report-cont" $ do
    toHtml $ map (\h -> h "") (processLine (sortByIPthenTime logData) (queryTqs 4 1 tqs) Nothing [])
  where tqsAll = toggleTqs' 2 (tglExclM1 2) $
                 toggleTqs' 3 (tglExclM1 2) loghDefaultTqs
        tqs400 = toggleTqs' 1 (tglExclM1 2) tqsAll
        tqs500 = toggleTqs' 1 (tglExclM1 3) tqsAll
        tqsDetails = toggleTqs' 4 tglSwap tqs

sortByIPthenTime :: [HLogData] -> [HLogData]
sortByIPthenTime = sortBy (comparing (\hld -> (hldIP hld, hldTime hld)))
