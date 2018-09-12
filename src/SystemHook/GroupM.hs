{-# LANGUAGE OverloadedStrings #-}
module SystemHook.GroupM where

import Query
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 as CH
import qualified Network.HTTP.Client as C
import Network.HTTP.Simple

data GroupM = GroupM {
      created_at :: String,
      updated_at :: String,
      event_name :: String,
      group_access :: String,
      group_id :: Integer,
      group_name :: String,
      group_path :: String,
      user_email :: String,
      user_name :: String,
      user_username :: String,
      user_id :: Integer
} deriving (Show)

type SResp = GroupM

getGroupMRsp :: Either String Object -> Either String SResp
getGroupMRsp (Right r) = flip parseEither r $ (\o -> do
  created_at <- o .: "created_at" 
  updated_at <- o .: "updated_at" 
  event_name <- o .: "event_name" 
  group_access <- o .: "group_access" 
  group_id  <- o .: "group_id" 
  group_name  <- o .: "group_name" 
  group_path  <- o .: "group_path" 
  user_email  <- o .: "user_email" 
  user_name  <- o .: "user_name" 
  user_username  <- o .: "user_username" 
  user_id  <- o .: "user_id" 
  return $ GroupM created_at updated_at event_name group_access
                  group_id group_name group_path user_email user_name
                  user_username user_id)
getGroupMRsp (Left e) = (Left e)

decodeRsp :: ByteString -> Either String (Either ErrorResp SResp)
decodeRsp b = decodeGLResp getGroupMRsp b
