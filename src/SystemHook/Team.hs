{-# LANGUAGE OverloadedStrings #-}
module SystemHook.Team where

import Query
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 as CH
import qualified Network.HTTP.Client as C
import Network.HTTP.Simple

data Team = Team {
      created_at :: String,
      updated_at :: String,
      event_name :: String,
      project_access :: String,      
      project_id :: Integer,
      project_path :: String,
      project_with_namespace :: String,
      user_email :: String,
      user_name :: String,
      user_username :: String,
      user_id :: Integer,                                  
      project_visibility :: String
} deriving (Show)

type SResp = Team

getTeamRsp :: Either String Object -> Either String SResp
getTeamRsp (Right r) = flip parseEither r $ (\o -> do
  created_at <- o .: "created_at" 
  updated_at <- o .: "updated_at" 
  event_name <- o .: "event_name" 
  project_access <- o .: "project_access" 
  project_id <- o .: "project_id" 
  project_path <- o .: "project_path" 
  project_with_namespace <- o .: "project_with_namespace" 
  user_email <- o .: "user_email" 
  user_name <- o .: "user_name" 
  user_username  <- o .: "user_username" 
  user_id <- o .: "user_id" 
  project_visibility <- o .: "project_visibility" 
  return $ Team created_at updated_at event_name project_access project_id
                project_path project_with_namespace user_email user_name
                user_username user_id project_visibility)
getTeamRsp (Left e) = (Left e)

decodeRsp :: ByteString -> Either String (Either ErrorResp SResp)
decodeRsp b = decodeGLResp getTeamRsp b
