{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module ProjectCreate where

import Data.Aeson
import Data.Aeson.Types
import qualified Network.HTTP.Client as C
import Network.HTTP.Simple    
import Data.Text as T
import Data.ByteString.Lazy.Char8 as CH

import User   
import Query
import qualified Namespace as NS

-- Gitlab Success Response: (user_id,project_id,project_http_url)
type SResp = (Integer,Integer,String)

getProjRsp :: Integer -> Either String Object -> Either String SResp
getProjRsp uid (Right r) = flip parseEither r $ (\o -> do
  id <- o .: "id"
  url <- o .: "http_url_to_repo"
  return (uid,id,url))
getProjRsp _ (Left e) = (Left e)

decodeRsp :: Integer -> ByteString -> Either String (Either ErrorResp SResp)
decodeRsp uid b = decodeGLResp (getProjRsp uid) b

userToParams :: Integer -> String -> User -> [(String,String)]
userToParams nid pt (User _ _ _ _ _ _ _ _ uid pn ns ie v) =
    [("private_token",pt),
     ("name",pn),
     ("namespace_id",(show nid)),
     ("issues_enabled",ie),
     ("visibility",v)] 

response :: User -> IO (Either String (Either ErrorResp SResp))
response user = do
  enid <- NS.getNID (namespace user)
  case enid of
    Right (Right (_,nid)) -> postRequest (userToParams nid) "/projects" user
                         >>= httpLBS
                         >>= (\r -> return $ (((decodeRsp (uid user)) $ C.responseBody r) :: Either String (Either ErrorResp SResp)))
    Right (Left e) -> return $ Right (Left e)
    Left e -> return $ Left e

responses :: (Either String (Either ErrorResp SResp) -> a) -> [User] -> IO [a]
responses f [] = return []
responses f (u:urs) = do
  r <- response u
  rs <- responses f urs
  return $ (f r) : rs
