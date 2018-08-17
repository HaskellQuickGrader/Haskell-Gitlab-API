{-# LANGUAGE OverloadedStrings #-}
module Namespace where

import Query
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 as CH
import qualified Network.HTTP.Client as C
import Network.HTTP.Simple

data Namespace = Namespace {
      id :: Integer,
      name :: String,
      path :: String,
      description :: String,
      visibility_level :: Integer,
      lfs_enabled :: Bool,
      request_access_enabled :: Bool,
      full_name :: String,
      full_path :: String,
      parent_id :: Maybe Integer
} deriving (Show)

type SResp = Namespace

getNSRsp :: Either String [Object] -> Either String SResp
getNSRsp (Right []) = Left "No namespace returned from Gitlab."
getNSRsp (Right [r]) = flip parseEither r $ (\o -> do
  id  <- o .: "id"
  n   <- o .: "name"
  p   <- o .: "path"
  d   <- o .: "description"
  v   <- o .: "visibility_level"
  l   <- o .: "lfs_enabled"
  r   <- o .: "request_access_enabled"
  fn  <- o .: "full_name"
  fp  <- o .: "full_path"
  pid <- o .: "parent_id"  
  return $ Namespace id n p d v l r fn fp pid)
getNSRsp (Left e) = (Left e)

decodeRsp :: ByteString -> Either String (Either ErrorResp SResp)
decodeRsp b = decodeGLResp getNSRsp b

nsParams :: String              -- Private Token
         -> String              -- Name of the namespace
         -> [(String,String)]   -- Parameters for request URL
nsParams pt n =
    [("private_token",pt),
     ("search",n)]

response :: String -> IO (Either String (Either ErrorResp SResp))
response name = getRequest nsParams "/groups" name
                   >>= httpLBS
                   >>= (\r -> return $ ((decodeRsp $ C.responseBody r) :: Either String (Either ErrorResp SResp)))

getNID :: String -> IO (Either String (Either ErrorResp (String, Integer)))
getNID name = response name >>= return.procResp
 where
   procResp (Right (Right ns)) = Right (Right (Namespace.name ns,Namespace.id ns))
   procResp (Right (Left e)) = Right (Left e)
   procResp (Left e) = Left e

getNIDs :: [String] -> IO [Either String (Either ErrorResp (String,Integer))]
getNIDs [] = return []
getNIDs (n:ns) = do
  nid <- getNID n
  r <- getNIDs ns
  return $ nid : r

