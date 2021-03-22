{-# LANGUAGE ScopedTypeVariables #-}

module Sebweb.Utils (
  extractPathSuffix
, extractPathSuffix'
, isWebSuffix
, isResourceSuffix
, isStaticSuffix
, parseStrictKeyValuePairs
, parseCookieText
, decodeUtf8Ignore

, formatTimeHttp
, parseTimeHttp
, errorTime
, addSeconds
, epochSecondsToUTCTime
, epochDate

, readRequestBody
, addResponseHeaders
, setCookieIfUnset
, assembleCookie
, emptyCookieBS

, safeHead
, safeLast
, safeIndex
, safeInit
, safeTail
, countElem
, modifyList
, deleteAtIndex
, compMF2
, applyIgnoreNothing

, parseCSV
, parseCSVLine
, encodeCSV
, dropQuotation

, openFileRetryTimeout
, openFileRetry
, openFileSafe

, generateRandom64
) where

import System.IO
import System.IO.Error
import System.Timeout
import Control.Exception
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (ignore)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Base64 as B64
import Data.Time
import Data.Time.Clock.System
import Data.Maybe
import Data.Int (Int64)
import Network.Wai
import Network.HTTP.Types.Header
import Crypto.Random

-- ---------------------------------------------------------------------------
-- Parsing Utility

-- Accepts "any.string.with.proper.suffix" but not "..onlysuffix"
extractPathSuffix :: T.Text -> Maybe T.Text
extractPathSuffix x = case reverse $ T.splitOn "." x of
  (a : b : _) -> if a /= "" && b /= "" then Just a else Nothing
  _ -> Nothing

extractPathSuffix' :: [T.Text] -> Maybe T.Text
extractPathSuffix' [] = Nothing
extractPathSuffix' xs = extractPathSuffix $ last xs

isWebSuffix :: T.Text -> Bool
isWebSuffix suff = suff `elem` ["css", "js"]

isResourceSuffix :: T.Text -> Bool
isResourceSuffix suff = suff `elem` ["txt", "png", "jpg", "svg", "pdf", "ico"]

isStaticSuffix :: T.Text -> Bool
isStaticSuffix suff = isWebSuffix suff || isResourceSuffix suff

parseStrictKeyValuePairs :: T.Text -> T.Text -> T.Text -> [(T.Text, T.Text)]
parseStrictKeyValuePairs tokDel valDel t =
  let rawToks = map (T.splitOn valDel) (T.splitOn tokDel t)
      parseTok [x, y] | (not . T.null) x && (not . T.null) y = Just (x, y)
                      | otherwise = Nothing
      parseTok _ = Nothing
  in catMaybes $ map parseTok $ map (map T.strip) rawToks

parseCookieText :: T.Text -> [(T.Text, T.Text)]
parseCookieText = parseStrictKeyValuePairs ";" "="

decodeUtf8Ignore :: BS.ByteString -> T.Text
decodeUtf8Ignore = TE.decodeUtf8With ignore


-- ---------------------------------------------------------------------------
-- Time Utilty

formatTimeHttp :: UTCTime -> String
formatTimeHttp = formatTime defaultTimeLocale httpFormatString

parseTimeHttp :: String -> Maybe UTCTime
parseTimeHttp = parseTimeM True defaultTimeLocale httpFormatString

httpFormatString :: String
httpFormatString = "%a, %d-%b-%Y %X GTM"

errorTime :: UTCTime
errorTime = UTCTime (fromGregorian 2000 1 1) 0

addSeconds :: Int -> UTCTime -> UTCTime
addSeconds s tim = addUTCTime (realToFrac s) tim

epochSecondsToUTCTime :: Int64 -> UTCTime
epochSecondsToUTCTime s =
  let (days, secs) = s `divMod` 86400
      day = addDays (fromIntegral days) systemEpochDay
  in UTCTime day (secondsToDiffTime $ fromIntegral secs)

epochDate :: UTCTime
epochDate = UTCTime systemEpochDay 0

-- ---------------------------------------------------------------------------
-- Request / Response Utility

readRequestBody :: Int -> Request -> IO (Maybe BS.ByteString)
readRequestBody = readRequestBody' ""

readRequestBody' :: BS.ByteString -> Int -> Request -> IO (Maybe BS.ByteString)
readRequestBody' prevBody byteLimit req = do
  bodyChunk <- getRequestBodyChunk req
  case bodyChunk of
    "" -> return $ wlCheck prevBody
    bs -> do
      let body = prevBody <> bs
      if isJust $ wlCheck body
      then readRequestBody' body byteLimit req
      else return Nothing
  where wlCheck b | BS.length b > byteLimit = Nothing
                  | otherwise = Just b

addResponseHeaders :: [Header] -> Response -> Response
addResponseHeaders xs = mapResponseHeaders (\hs -> hs ++ xs)

setCookieIfUnset :: BS.ByteString -> Response -> Response
setCookieIfUnset bs = mapResponseHeaders f
  where f hs | hSetCookie `elem` (map fst hs) = hs
             | otherwise = hs ++ [(hSetCookie, bs)]

assembleCookie :: T.Text -> T.Text -> UTCTime -> Int -> T.Text
assembleCookie name val expires maxAge = name <> "=" <> val <>
  "; Expires=" <> T.pack (formatTimeHttp expires) <>
  "; Max-Age=" <> T.pack (show maxAge) <>
  "; Secure; HttpOnly; SameSite=Strict; Path=/"

emptyCookieBS :: T.Text -> UTCTime -> BS.ByteString
emptyCookieBS cn now = TE.encodeUtf8 $ assembleCookie cn "" now 0


-- ---------------------------------------------------------------------------
-- General Utility

safeHead :: a -> [a] -> a
safeHead x [] = x
safeHead _ xs = head xs

safeLast :: a -> [a] -> a
safeLast x [] = x
safeLast _ xs = last xs

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs ind | ind >= length xs = Nothing
                 | otherwise = Just $ xs !! ind

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

countElem :: Eq a => a -> [a] -> Int
countElem x xs = length (filter (== x) xs)

modifyList :: Int -> (a -> a) -> [a] -> [a]
modifyList _ _ [] = []
modifyList 0 f (x : xs) = (f x) : xs
modifyList ind f (x : xs) = x : (modifyList (ind - 1) f xs)

deleteAtIndex :: Int -> [a] -> [a]
deleteAtIndex _ [] = []
deleteAtIndex 0 (_ : xs) = xs
deleteAtIndex ind (x : xs) = x : (deleteAtIndex ind xs)

compMF2 :: (b -> Maybe c) -> (a -> Maybe b) -> (a -> Maybe c)
compMF2 f g a = fromMaybe Nothing (fmap f $ g a)

applyIgnoreNothing :: (a -> Maybe a) -> a -> a
applyIgnoreNothing f a = case f a of
  Nothing -> a
  Just x -> x


-- ---------------------------------------------------------------------------
-- CSV Parsing

parseCSV :: T.Text -> [[T.Text]]
parseCSV t = map parseCSVLine (T.lines t)

parseCSVLine :: T.Text -> [T.Text]
parseCSVLine t =
  let toks = map T.strip (T.splitOn "," t)
  in consolidateToks toks False []

consolidateToks :: [T.Text] -> Bool -> [T.Text] -> [T.Text]
consolidateToks [] _ acc = acc
consolidateToks (x : xs) False acc =
  consolidateToks xs (isOpening x) (acc ++ [x])
consolidateToks (x : xs) True acc
  | isClosing x = consolidateToks xs False (concatToLast acc x)
  | otherwise = consolidateToks xs True (concatToLast acc x)
  where concatToLast [] y = [y] -- should never occur
        concatToLast ys y = init ys ++ [last ys <> "," <> y]

isOpening :: T.Text -> Bool
isOpening "\"" = True
isOpening y = (T.isPrefixOf "\"") y && (not $ isClosing y)

isClosing :: T.Text -> Bool
isClosing y = (not $ T.isSuffixOf "\\\"" y) && (T.isSuffixOf "\"" y)

dropQuotation :: T.Text -> T.Text
dropQuotation = T.dropAround (== '\"')

encodeCSV :: Int -> T.Text -> T.Text
encodeCSV maxLen t = buildCSVString $ T.take maxLen t

buildCSVString :: T.Text -> T.Text
buildCSVString "" = ""
buildCSVString t = T.singleton '\"' <> T.concatMap escape t <> T.singleton '\"'
  where escape '\\' = "\\\\"
        escape '\"' = "\\\""
        escape '\n' = "\\n"
        escape c = T.singleton c


-- ---------------------------------------------------------------------------
-- File Operations

openFileRetryTimeout :: Int -> Int -> FilePath -> IOMode -> IO (Maybe Handle)
openFileRetryTimeout toTime ri fp m =
  timeout toTime (openFileRetry ri fp m) >>= pure . (fromMaybe Nothing)

openFileRetry :: Int -> FilePath -> IOMode -> IO (Maybe Handle)
openFileRetry ri fp m =
  catch (openFile fp m >>= pure . Just) handleException
  where handleException e
          | isAlreadyInUseError e = do threadDelay ri
                                       openFileRetry ri fp m
          | otherwise = pure Nothing

openFileSafe :: FilePath -> IOMode -> IO (Maybe Handle)
openFileSafe fp m =
  catch (openFile fp m >>= pure . Just) (\(_ :: IOException) -> pure Nothing)

generateRandom64 :: IO T.Text
generateRandom64 = do
  sysDRG <- getSystemDRG
  let (dat, _) = randomBytesGenerate 24 sysDRG
  return $ T.pack $ B8.unpack $ B64.encode dat

