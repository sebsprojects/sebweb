module Sebweb.ToggleQuery (
  tqsToQueryString
, tqsFromQuery
, queryTqs
, tglAny
, tglSwap
, tglExclM1
, toggleTqs'
, tglLink
, extractFirstTqi
, extractAllTqis
, ToggleQueryState(..)
, loghDefaultTqs
, logiDefaultTqs
) where

import Prelude hiding (div)
import Data.Maybe
import qualified Data.Text as T
import Text.Blaze.Html5 hiding (style, map, code)
import Text.Blaze.Html5.Attributes hiding (form, title, max)

import Sebweb.Utils


type ToggleQueryState = [ToggleQueryItem]
type ToggleQueryItem = (T.Text, [(T.Text, Bool)])

type ToggleFunction = [(T.Text, Bool)] -> [(T.Text, Bool)]


-- ------------------------------------------------------------------------
-- Parsing Http-Query arugments to / from TQS

tqsToQueryString :: ToggleQueryState -> T.Text
tqsToQueryString tqs = "?" <> T.intercalate "&" (map tqiToQueryString tqs)

tqiToQueryString :: ToggleQueryItem -> T.Text
tqiToQueryString (nam, vals) = nam <> "=" <>
  T.intercalate "," (map fst (filter snd vals))

tqsFromQuery :: [(T.Text, T.Text)] -> ToggleQueryState -> ToggleQueryState
tqsFromQuery query def = map (tqiFromQuery query) def

-- If the tqi-name is not present in the query string then returns 
-- all false (and not the def input)
tqiFromQuery :: [(T.Text, T.Text)] -> ToggleQueryItem -> ToggleQueryItem
tqiFromQuery qDict (nam, els) =
  (nam, map (tqelFromQuery (T.splitOn "," qs)) els)
  where qs = fromMaybe "" $ lookup nam qDict

tqelFromQuery :: [T.Text] -> (T.Text, Bool) -> (T.Text, Bool)
tqelFromQuery qss (nam, _) = (nam, nam `elem` qss)

-- Returns the first tqi with value "True" at tqs[ind]
extractFirstTqi :: Int -> ToggleQueryState -> T.Text
extractFirstTqi ind tqs = findTqi (snd (tqs !! ind))
  where findTqi [] = ""
        findTqi (x : xs) = if snd x then fst x else findTqi xs

extractAllTqis :: Int -> ToggleQueryState -> [T.Text]
extractAllTqis ind tqs = map fst $ filter snd (snd $ tqs !! ind)

queryTqs :: Int -> Int -> ToggleQueryState -> Bool
queryTqs it el tqs = snd ((snd (tqs !! it)) !! el)


-- ------------------------------------------------------------------------
-- Wrapper Functions

wrapTqi :: ToggleFunction -> ToggleQueryItem -> ToggleQueryItem
wrapTqi f tqi = (fst tqi, f $ snd tqi)

toggleTqs :: Int -> (ToggleQueryItem -> ToggleQueryItem) -> ToggleQueryState ->
             ToggleQueryState
toggleTqs _ _ [] = []
toggleTqs 0 tog (x : xs) = (tog x) : xs
toggleTqs ind tog (x : xs) = x : (toggleTqs (ind - 1) tog xs)

toggleTqs' :: Int -> ToggleFunction -> ToggleQueryState -> ToggleQueryState
toggleTqs' it fun tqs = toggleTqs it (wrapTqi fun) tqs


-- ------------------------------------------------------------------------
-- Toggle Functions

tglAny :: Int -> ToggleFunction
tglAny ind xs = modifyList ind (\(t, bo) -> (t, not bo)) xs

tglSwap :: ToggleFunction
tglSwap = (tglAny 1) . (tglAny 0)


tglAddM1 :: Int -> ToggleFunction
tglAddM1 ind xs = let xs' = tglAny ind xs
                  in if countElem True (map snd xs') == 0 then xs else xs'

tglExclM1 :: Int -> ToggleFunction
tglExclM1 _ [] = []
tglExclM1 0 (x : xs) = (fst x, True) : (tglExclM1 (-1) xs)
tglExclM1 ind (x : xs) = (fst x, False) : (tglExclM1 (ind - 1) xs)


-- ------------------------------------------------------------------------
-- Default TQS for various pages to break dependencies problems with View

loghDefaultTqs :: ToggleQueryState
loghDefaultTqs = [
  ("day", [("1", True), ("2", False), ("3", False)])
  , ("status", [("2xx", True), ("3xx", False), ("4xx", False), ("5xx", False)])
  , ("method", [("GET", True), ("POST", False), ("all", False)])
  , ("static", [("0", True), ("1", False), ("all", False)])
  , ("details", [("0", True), ("1", False)])
  ]

logiDefaultTqs :: ToggleQueryState
logiDefaultTqs = [
  ("day", [("1", True), ("2", False), ("3", False)])
  , ("level", [("Crt", True), ("Err", True), ("Inf", False)])
  , ("type", [("Ath", True), ("Wrk", True), ("Oth", True), ("Wrp", False)])
  , ("details", [("0", True), ("1", False)])
  ]


-- ------------------------------------------------------------------------
-- HTML Combinators

tglLink :: ToggleQueryState -> ToggleFunction -> Int -> Int -> T.Text -> Html ->
           Html
tglLink tqs fun it el bpath htm = div ! class_ "sel-item" $
  a ! class_ (textValue selClass) ! href (textValue tglRef) $ htm
  where selClass = if queryTqs it el tqs then "sel-active" else ""
        tglRef = bpath <> tqsToQueryString (toggleTqs' it fun tqs)

