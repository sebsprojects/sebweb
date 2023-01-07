{-# LANGUAGE ScopedTypeVariables #-}

module Sebweb.WorkerWhoisLooker (
    workerWhoisLooker
) where

import System.IO
import System.Timeout
import System.Directory
import Data.Maybe
import Data.List (union)
import Data.Time
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Control.Exception
import Control.Concurrent
import Network.Socket
import Network.Socket.ByteString

import Sebweb.Utils
import Sebweb.Log
import Sebweb.LogI
import Sebweb.Worker


-- --------------------------------------------------------------------------
-- Top Level Worker Thread and Looker

workerWhoisLooker :: ILogQueue -> Int -> T.Text -> T.Text -> IO ()
workerWhoisLooker ilq startt logDir hSuff =
  dailyWorker ilq "whoislooker" startt $ do
    now <- getCurrentTime
    let yesterday = addUTCTime (fromIntegral (-86400 :: Int)) now
    logEnqueue ilq $ mkILogData ILInfo ITWorker
      "whoislooker: starting whois lookups"
    let logFP = T.unpack logDir <> "/" <> tf yesterday <> T.unpack hSuff
    -- TODO: FILE IO
    fe <- doesFileExist logFP
    case fe of
      False ->
        logEnqueue ilq $ mkILogData ILError ITWorker $
          "whoislooker: could not find yesterdays log with path " <>
          T.pack logFP
      True -> do
        ips <- withFile logFP ReadMode (flip gatherIps [])
        ipLocs <- mapM (issueWhoisQuery ilq) ips
        let ipCountryPairs = concatMap makeIpCountryPair ipLocs
        -- TEMP: Write out the full whois query result to a separate file
        -- let tempPath = T.unpack logDir <> "/" <> tf yesterday <> "_whoislog.txt"
        --_ <- bracket (openFile tempPath WriteMode) hClose $ \h -> do
        --  let xs = map (\(ip, ipLoc) -> ("ip", ip) : ipLoc) ipLocs
        --  let wlns = map (\ent -> T.intercalate "," (entToLine ent)) xs
        --  mapM_ (TIO.hPutStrLn h) wlns
        updateLogFile logFP ipCountryPairs
        logEnqueue ilq $ mkILogData ILInfo ITWorker $
          "whoislooker: finished with " <> T.pack (show $ length ips) <>
          " whois lookups"
  where tf = formatTime defaultTimeLocale "%F"
        makeIpCountryPair (ip, locs) = case lookup "country" locs of
                                              Nothing -> []
                                              Just c -> [(ip, c)]
        -- entToLine = map (\(k,v) -> k <> "=" <> v)


-- --------------------------------------------------------------------------
-- Log Operations

updateLogFile :: FilePath -> IPInfo -> IO ()
updateLogFile logPath ipLocs = do
  -- TODO: FILE IO
  hOld <- openFile logPath ReadMode
  hNew <- openFile (logPath <> ".new") WriteMode
  writeBackLogLines hOld hNew ipLocs
  hFlush hNew >> hClose hNew
  hClose hOld
  -- TEMP: replace with rename file to keep the old version
  -- renameFile logPath (logPath <> ".old")
  removeFile logPath
  renameFile (logPath <> ".new") logPath

issueWhoisQuery :: ILogQueue -> T.Text -> IO (T.Text, IPInfo)
issueWhoisQuery ilq ipText = do
  mLoc <- timeout (1 * 1000000) $ whoisQueryRec ilq ipText
  threadDelay (1 * 1000000)
  return (ipText, fromMaybe [] mLoc)

gatherIps :: Handle -> [T.Text] -> IO [T.Text]
gatherIps h acc = do
  eof <- hIsEOF h
  if eof
    then return acc
    else TIO.hGetLine h >>= \l -> gatherIps h (acc `union` extractIp l)

extractIp :: T.Text -> [T.Text]
extractIp l = maybeToList (safeIndex (T.splitOn "," l) 1)

writeBackLogLines :: Handle -> Handle -> IPInfo -> IO ()
writeBackLogLines hOld hNew ipLocs = do
  eof <- hIsEOF hOld
  if eof
    then return ()
    else do l <- TIO.hGetLine hOld
            TIO.hPutStr hNew $ updateLine l ipLocs
            writeBackLogLines hOld hNew ipLocs

updateLine :: T.Text -> IPInfo -> T.Text
updateLine l ipLocs = case extractIp l of
  [ip] -> case lookup ip ipLocs of
    Just c -> l <> c <> "\n"
    Nothing -> l <> "\n"
  _ -> l <> "\n"


-- --------------------------------------------------------------------------
-- Constants and dealing with host names

type IPInfo = [(T.Text, T.Text)]

whoisPort :: String
whoisPort = "43"

arinServer :: T.Text
arinServer = "whois.arin.net"

ripeServer :: T.Text
ripeServer = "whois.ripe.net"

apnicServer :: T.Text
apnicServer = "whois.apnic.net"

lacnicServer :: T.Text
lacnicServer = "whois.lacnic.net"

afrinicServer :: T.Text
afrinicServer = "whois.afrinic.net"

knownServers :: [T.Text]
knownServers = [arinServer, ripeServer, apnicServer, lacnicServer,
                afrinicServer]

getQuery :: T.Text -> (T.Text -> T.Text)
getQuery t
  | t == arinServer = \q -> "n + " <> q <> "\r\n"
  | t == ripeServer = \q -> "-r " <> q <> "\r\n"
  | t == lacnicServer = \q -> q <> "\r\n"
  | t == apnicServer = \q -> "-r " <> q <> "\r\n"
  | t == afrinicServer = \q -> "" <> q <> "\r\n"
  | otherwise = \q -> q <> "\r\n"

getServerName :: T.Text -> T.Text
getServerName hn =
  mStripPrefixText "whois." $
  mStripPrefixText "rwhois." $
  mStripSuffixText ".net"
  hn

mStripPrefixText :: T.Text -> T.Text -> T.Text
mStripPrefixText pref t = applyIgnoreNothing (T.stripPrefix pref) t

mStripSuffixText :: T.Text -> T.Text -> T.Text
mStripSuffixText pref t = applyIgnoreNothing (T.stripSuffix pref) t


-- --------------------------------------------------------------------------
-- Main Whois

whoisQueryRec :: ILogQueue -> T.Text -> IO IPInfo
whoisQueryRec ilq ip = do
  arinInfo <- whoisQuery ilq arinServer ip
  case fmap stripRef $ lookup "referralserver" arinInfo of
    Nothing -> return $ appInf arinServer 0 arinInfo
    Just ref -> if ref `elem` knownServers
      then do refInfo <- whoisQuery ilq ref ip
              case lookup "country" refInfo of
                Nothing -> return $ appInf arinServer 0 arinInfo
                Just _ -> return $ appInf ref 1 refInfo
      else return $ appInf arinServer 0 arinInfo
  where appInf s c xs = xs ++ [("server", getServerName s)] ++
                        [("refcount", T.pack $ show (c :: Int))]
        stripRef = mStripPrefixText "whois://"

whoisQuery :: ILogQueue -> T.Text -> T.Text -> IO IPInfo
whoisQuery ilq server ip = do
  mresp <- makeWhoisQuery ilq (T.unpack server) (T.unpack ip)
  return $ parseWhoisResponse (T.pack $ B8.unpack $ fromMaybe "" mresp)


-- --------------------------------------------------------------------------
-- Parsing

parseWhoisResponse :: T.Text -> IPInfo
parseWhoisResponse t =
  let lns = filterComments $ filterEmpty $ map stripCR (T.lines t)
      vs = mapMaybe (tryKeys keys) lns
  in concatMap (\k -> lookupToPair k vs) keys
  where stripCR = T.takeWhile (/= '\r')
        filterComments bss = filter (not . T.isPrefixOf "#") bss
        filterEmpty bss = filter (/= "") bss
        keys = ["referralserver", "country", "city"]
        lookupToPair k di = case lookup k di of
          Nothing -> []
          Just v -> [(k, v)]

tryKeys :: [T.Text] -> T.Text -> Maybe (T.Text, T.Text)
tryKeys keys t =
  let toks = T.splitOn ":" t
  in case toks of
    (k : v : vs) -> if T.toLower (T.strip k) `elem` keys
                    then Just (T.toLower $ T.strip k,
                               T.strip $ T.intercalate ":" (v : vs))
                    else Nothing
    _ -> Nothing


-- --------------------------------------------------------------------------
-- Low level socket operations

makeWhoisQuery :: ILogQueue -> String -> String ->
                  IO (Maybe BS.ByteString)
makeWhoisQuery ilq server ipString = withSocketsDo $ do
  maddr <- resolve server whoisPort
  case maddr of
    Just a -> do
      resp <- sendAndReceive a `catch` (\(e :: SomeException) -> handleUEx e)
      return $ Just resp
    Nothing -> do
      -- Logging the exceptions hidden in resolve
      logWhois "whoislooker: address Info (hidden)"
      return Nothing
  where sendAndReceive a = bracket (open a) close query
        query = makeQuery $ TE.encodeUtf8 $
                getQuery (T.pack server) (T.pack ipString)
        handleUEx e = do
          logWhois $ "whoislooker: " <> T.pack (show e)
          return ""
        logWhois msg = do
          now <- getCurrentTime
          logEnqueue ilq $ ILogData now ILInfo ITWorker msg

-- Hides exceptions but return [] should raise an exception further up
resolve :: HostName -> ServiceName -> IO (Maybe AddrInfo)
resolve h p = do
  let hints = defaultHints { addrSocketType = Stream }
  addrs <- getAddrInfo (Just hints) (Just h) (Just p) `catches`
           [Handler (\(ex :: IOException) -> handleIOException ex),
            Handler (\(ex :: SomeException) -> handleUnknownException ex)]
  return $ listToMaybe addrs
  where handleIOException _ = return []
        handleUnknownException _ = return []

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  return sock

makeQuery :: BS.ByteString -> Socket -> IO BS.ByteString
makeQuery q sock = do
  sendAll sock q
  recvAll sock

recvAll :: Socket -> IO BS.ByteString
recvAll s = whileM (/= BS.empty) [] (recv s 1024) " " >>= return . BS.concat

whileM :: Monad m => (a -> Bool) -> [a] -> m a -> a -> m [a]
whileM cond acc f x
  | cond x = f >>= \y -> whileM cond (acc ++ [y]) f y
  | otherwise = return acc
