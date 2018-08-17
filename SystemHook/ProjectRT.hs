{-# LANGUAGE OverloadedStrings #-}
module SystemHook.ProjectRT where

import Query
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 as CH
import qualified Network.HTTP.Client as C
import Network.HTTP.Simple

data ProjectRT = ProjectRT {
      created_at :: String,
      updated_at :: String,
      event_name :: String,
      name :: String,
      owner_email :: String,
      owner_name :: String,
      path :: String,
      path_with_namespace :: String,
      project_id :: Integer,
      project_visibility :: String,
      old_path_with_namespace :: String
} deriving (Show)

type SResp = ProjectRT

getPCRsp :: Either String Object -> Either String SResp
getPCRsp (Right r) = flip parseEither r $ (\o -> do
  created_at <- o .: "created_at"
  updated_at <- o .: "updated_at"
  event_name <- o .: "event_name"
  name <- o .: "name"
  owner_email <- o .: "owner_email"
  owner_name <- o .: "owner_name"
  path <- o .: "path"
  path_with_namespace <- o .: "path_with_namespace"
  project_id <- o .: "project_id"
  project_visibility <- o .: "project_visibility"
  old_path_with_namespace <- o .: "old_path_with_namespace"
  return $ ProjectRT created_at  updated_at event_name name
                     owner_email owner_name path       path_with_namespace
                     project_id  project_visibility old_path_with_namespace)
getPCRsp (Left e) = (Left e)

decodeRsp :: ByteString -> Either String (Either ErrorResp SResp)
decodeRsp b = decodeGLResp getPCRsp b
