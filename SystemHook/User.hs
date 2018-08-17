{-# LANGUAGE OverloadedStrings #-}
module SystemHook.User where

import Query
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 as CH
import qualified Network.HTTP.Client as C
import Network.HTTP.Simple

data User = User {
      created_at :: String,
      updated_at :: String,
      event_name :: String,
      email :: String,
      name :: String,
      username :: String,
      user_id :: Integer      
} deriving (Show)

type SResp = User

getUserRsp :: Either String Object -> Either String SResp
getUserRsp (Right r) = flip parseEither r $ (\o -> do
  created_at <- o .: "created_at" 
  updated_at <- o .: "updated_at" 
  event_name <- o .: "event_name" 
  email <- o .: "email" 
  name <- o .: "name" 
  username  <- o .: "username" 
  user_id <- o .: "user_id"   
  return $ User created_at updated_at event_name email name username user_id)
getUserRsp (Left e) = (Left e)

decodeRsp :: ByteString -> Either String (Either ErrorResp SResp)
decodeRsp b = decodeGLResp getUserRsp b
