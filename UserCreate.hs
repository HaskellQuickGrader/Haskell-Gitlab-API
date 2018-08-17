{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module UserCreate where

import Data.Aeson
import Data.Aeson.Types
import Network.HTTP.Types    
import qualified Network.HTTP.Client as C
import Network.HTTP.Simple    
import Data.Text as T
import Data.ByteString.Lazy.Char8 as CH

import User   
import Query

-- Gitlab Success Response: (username,UID)
type SResp = (String,Integer)

getUIDRsp :: Either String Object -> Either String SResp
getUIDRsp (Right r) = flip parseEither r $ (\o -> do
  id <- o .: "id"
  un <- o .: "username"
  return (un,id))
getUIDRsp (Left e) = (Left e)

decodeRsp :: ByteString -> Either String (Either ErrorResp SResp)
decodeRsp b = decodeGLResp getUIDRsp b
          
userToParams :: String -> User -> [(String,String)]
userToParams pt (User e rp u n pl a cg cp _ _ _ _ _) =
    [("private_token",pt),
     ("email",e),
     ("reset_password",rp),
     ("username",u),
     ("name",n),
     ("projects_limit",pl),
     ("admin",a),
     ("can_create_group",cg),
     ("can_create_project",cp)]

response :: User -> IO (Either String (Either ErrorResp SResp))
response user = postRequest userToParams "/users" user
                   >>= httpLBS
                   >>= (\r -> return $ ((decodeRsp $ C.responseBody r) :: Either String (Either ErrorResp SResp)))

responses :: (Either String (Either ErrorResp SResp) -> a) -> [User] -> IO [a]
responses f [] = return []
responses f (u:urs) = do
  r <- response u
  rs <- responses f urs
  return $ (f r) : rs

ex_rsp = "{\"name\":\"Prof Eades\",\"username\":\"hde\",\"id\":62,\"state\":\"active\",\"avatar_url\":null,\"web_url\":\"http://gitlab.metatheorem.org/hde\",\"created_at\":\"2017-09-24T14:24:09.982Z\",\"bio\":null,\"location\":null,\"skype\":\"\",\"linkedin\":\"\",\"twitter\":\"\",\"website_url\":\"\",\"organization\":null,\"last_sign_in_at\":null,\"confirmed_at\":null,\"last_activity_on\":null,\"email\":\"heades@augusta.edu\",\"color_scheme_id\":1,\"projects_limit\":0,\"current_sign_in_at\":null,\"identities\":[],\"can_create_group\":false,\"can_create_project\":false,\"two_factor_enabled\":false,\"external\":false}"
