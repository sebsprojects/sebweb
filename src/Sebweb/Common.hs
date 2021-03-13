module Sebweb.Common (
    UserAccess(..)
  , RequestData(..)

  , authKey
  , revKey

  , getRequestRev
  , getRequestAuth
  , getRequestPath
  , getRequestData

  , isAuthenticated
  , doesUserExist
  , validateUser
) where

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Directory
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vault.Lazy as Vault
import qualified Data.ByteString as BS
import Crypto.KDF.BCrypt
import Network.Wai


data UserAccess =
  AuthVisitor |
  AuthUnauthorized |
  AuthAuthorized T.Text |
  AuthUnknown
  deriving (Show)

data RequestData = RequestData {
  rdPath :: T.Text
, rdRev :: T.Text
, rdAuth :: UserAccess }


-- ---------------------------------------------------------------------------
-- Request Vault Keys

-- We need unsafePerformIO AND the NOINLINE because the key must be generated
-- once and must be unique. This is safe to do according to
-- https://www.yesodweb.com/blog/2015/10/using-wais-vault
authKey :: Vault.Key UserAccess
authKey = unsafePerformIO Vault.newKey
{-# NOINLINE authKey #-}

revKey :: Vault.Key T.Text
revKey = unsafePerformIO Vault.newKey
{-# NOINLINE revKey #-}


-- ---------------------------------------------------------------------------
-- Request Data

getRequestData :: Request -> RequestData
getRequestData req =
  RequestData (getRequestPath req) (getRequestRev req) (getRequestAuth req)

getRequestPath :: Request -> T.Text
getRequestPath req = T.intercalate "/" (pathInfo req)

getRequestRev :: Request -> T.Text
getRequestRev req = fromMaybe "noRev" $ Vault.lookup revKey (vault req)

getRequestAuth :: Request -> UserAccess
getRequestAuth req = fromMaybe AuthUnknown $ Vault.lookup authKey (vault req)


-- --------------------------------------------------------------------------
-- User Authentication

isAuthenticated :: UserAccess -> Bool
isAuthenticated (AuthAuthorized _) = True
isAuthenticated _ = False

doesUserExist :: T.Text -> T.Text -> IO Bool
doesUserExist name path =
  -- TODO: FILE IO
  doesFileExist $ T.unpack $ path <> "/" <> name <> ".user"

validateUser :: T.Text -> BS.ByteString -> T.Text -> IO UserAccess
validateUser name pass path = do
  let filePath = path <> "/" <> name <> ".user"
  -- TODO: FILE IO
  h <- openFile (T.unpack filePath) ReadMode
  hash <- BS.hGetLine h
  case validatePassword pass hash of
    True -> return $ AuthAuthorized name
    False -> return AuthUnauthorized

