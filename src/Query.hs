{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Query where

import Network.URL
import Network.HTTP.Simple
import qualified Network.HTTP.Client as C
import Network.HTTP.Types.Method    
import System.Environment
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Text as T
import Data.ByteString.Lazy.Char8 as CH hiding (putStrLn)
import Data.String.Combinators

privateTokenVar :: String
privateTokenVar = "GITLAB_API_PRIVATE_TOKEN"

apiEndpointVar :: String
apiEndpointVar = "GITLAB_API_ENDPOINT"

privateToken :: IO String
privateToken = getEnv privateTokenVar

apiEndpoint :: IO String
apiEndpoint = getEnv apiEndpointVar

requestURL :: (String -> a -> [(String,String)]) -- Private Key -> Add Data -> URL Parameters
           -> String                             -- Gitlab query string
           -> URL                                -- API URL
           -> String                             -- Private key
           -> a                                  -- Data to toP (first arg)
           -> String                             -- The full query URL
requestURL toP q u pt x = exportURL url
 where
   url :: URL
   url = u { url_params = toP pt x,
             url_path = url_path u ++ q }           

request :: StdMethod                          -- Type of query, e.g. POST or GET
        -> (String -> a -> [(String,String)]) -- Private Key -> Add Data -> URL Parameters          
        -> String                             -- Gitlab query string
        -> a                                  -- Data to toP (first arg)
        -> IO Request                         -- The Gitlab request used with httpLBS
request method toP q x = do
   pt <- privateToken
   gurl <- apiEndpoint  
   case (importURL gurl) of
     Just u -> do let url = requestURL toP q u pt x
                  iRep <- parseRequest url
                  return (iRep {C.method = renderMethod (Right method)})
     Nothing -> error "GITLAB_API_ENDPOINT not set correctly"       

postRequest :: (String -> a -> [(String,String)]) -- Private Key -> Add Data -> URL Parameters          
            -> String                             -- Gitlab query string
            -> a                                  -- Data to toP (first arg)
            -> IO Request                         -- The Gitlab request used with httpLBS
postRequest = request POST

getRequest :: (String -> a -> [(String,String)]) -- Private Key -> Add Data -> URL Parameters          
            -> String                             -- Gitlab query string
            -> a                                  -- Data to toP (first arg)
            -> IO Request                         -- The Gitlab request used with httpLBS
getRequest = request GET

-- Gitlab Message Response
data MResp = MResp {
  message :: String
} deriving (Generic,Show)
           
-- Gitlab Error Response
data EResp = EResp {
  err :: String
} deriving (Generic,Show)

type ErrorResp = Either EResp MResp

appendER :: ErrorResp -> ErrorResp -> ErrorResp
appendER (Left (EResp e1))  (Left (EResp e2))  = Left  (EResp (e1 $$ e2))
appendER (Left (EResp e))   (Right (MResp m))  = Left  (EResp (e  $$ m))
appendER (Right (MResp m))  (Left (EResp e))   = Left  (EResp (m  $$ e))
appendER (Right (MResp m1)) (Right (MResp m2)) = Right (MResp (m1 $$ m2))
    
decodeMResp :: Either String Object -> Either String MResp
decodeMResp (Right r) = flip parseEither r $ (\o -> do
  m <- o .: "message"
  return (MResp m))
decodeMResp (Left e) = (Left e)

decodeEResp :: Either String Object -> Either String EResp
decodeEResp (Right r) = flip parseEither r $ (\o -> do
  e <- o .: "error"
  return (EResp e))
decodeEResp (Left e) = (Left e)
           
decodeGLResp :: FromJSON b => (Either String b -> Either String a)  -- Decoder for a particular respose type
             -> ByteString                                          -- JSON that needs decoding
             -> Either String (Either ErrorResp a)                  -- Either an error message or the parsed JSON.
decodeGLResp aDecode s = case (aDecode eo) of
                           Right aR -> Right (Right aR)
                           Left e1 -> case (decodeMResp eo) of
                                        Right mR -> Right (Left (Right mR))
                                        Left e2 -> case (decodeEResp eo) of
                                                     Right eR -> Right (Left (Left eR))
                                                     Left e3 -> Left (e1 $$ e2 $$ e3)
 where
   eo :: FromJSON c => Either String c
   eo = eitherDecode s

defaultResp :: Either String (Either ErrorResp a) -> Either String a
defaultResp (Right (Right r)) = Right r
defaultResp (Right (Left (Right (MResp m)))) = Left m
defaultResp (Right (Left (Left (EResp e)))) = Left e
defaultResp (Left e) = Left e

data Sum a b = Injl a | Injr b
 deriving Show

(<++>) :: Either String (Either ErrorResp a)
       -> Either String (Either ErrorResp b)
       -> Either String (Either ErrorResp (Sum a b))
(Left m1)         <++> (Left m2)         = Left $ m1 $$ m2
(Left m)          <++> (Right (Left e))  = Left m
(Right (Left e))  <++> (Left m)          = Left m
(Right (Left e1)) <++> (Right (Left e2)) = Right $ Left $ e1 `appendER` e2
(Left m)          <++> (Right (Right y)) = Right $ Right $ Injr y
(Right (Left e))  <++> (Right (Right y)) = Right $ Right $ Injr y
(Right (Right x)) <++> (Left m)          = Right $ Right $ Injl x
(Right (Right x)) <++> (Right (Left e))  = Right $ Right $ Injl x
(Right (Right x)) <++> (Right (Right y)) = Left "This is a bug: ambiguous JSON in <++>"
