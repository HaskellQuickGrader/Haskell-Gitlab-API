{-# LANGUAGE OverloadedStrings #-}
module SystemHook.Key where

import Query
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 as CH
import qualified Network.HTTP.Client as C
import Network.HTTP.Simple

data Key = Key {
      created_at :: String,
      updated_at :: String,
      event_name :: String,
      username :: String,
      key :: String,
      id :: Integer      
} deriving (Show)

type SResp = Key

getKeyRsp :: Either String Object -> Either String SResp
getKeyRsp (Right r) = flip parseEither r $ (\o -> do
  created_at <- o .: "created_at" 
  updated_at <- o .: "updated_at" 
  event_name <- o .: "event_name" 
  username  <- o .: "username" 
  id <- o .: "id"
  key <- o .: "key"
  return $ Key created_at updated_at event_name username id key)
getKeyRsp (Left e) = (Left e)

decodeRsp :: ByteString -> Either String (Either ErrorResp SResp)
decodeRsp b = decodeGLResp getKeyRsp b
