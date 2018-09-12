{-# LANGUAGE OverloadedStrings #-}
module SystemHook.GroupCR where

import Query
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 as CH
import qualified Network.HTTP.Client as C
import Network.HTTP.Simple

data GroupCR = GroupCR {
      created_at :: String,
      updated_at :: String,
      event_name :: String,
      name :: String,
      owner_email :: String,
      owner_name :: String,
      path :: String,
      group_id :: Integer      
} deriving (Show)

type SResp = GroupCR

getGroupCRRsp :: Either String Object -> Either String SResp
getGroupCRRsp (Right r) = flip parseEither r $ (\o -> do
  created_at <- o .: "created_at" 
  updated_at <- o .: "updated_at" 
  event_name <- o .: "event_name" 
  name  <- o .: "name" 
  group_id <- o .: "group_id"
  owner_email <- o .: "owner_email"
  owner_name <- o .: "owner_name"
  path <- o .: "path"
  return $ GroupCR created_at updated_at event_name name owner_email owner_name path group_id)
getGroupCRRsp (Left e) = (Left e)

decodeRsp :: ByteString -> Either String (Either ErrorResp SResp)
decodeRsp b = decodeGLResp getGroupCRRsp b
