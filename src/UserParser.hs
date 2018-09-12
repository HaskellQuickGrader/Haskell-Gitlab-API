{-# 
LANGUAGE 
  NoMonomorphismRestriction, 
  PackageImports, 
  TemplateHaskell, 
  FlexibleContexts 
#-}
module UserParser where

import Data.Char
import Text.Parsec hiding (Empty)
import Text.Parsec.Token    
import Text.Email.Validate
import qualified Data.ByteString.Char8 as B

import User
import qualified UserCreate as UC
    
parseUsername = do
  a <- lower
  r <- many alphaNum
  char '$'
  return $ a : r

parseName = do
  last <- upper >>= (\c -> many (lower <|> upper <|> char '-' <|> char '\'') >>= return.(c:))
  char ','
  first <- upper >>= (\c -> many (lower <|> upper <|> char '-') >>= return.(c:))
  char '$'
  return $ first ++ " " ++ last

parseStrictName = do
  r <- many (lower <|> upper)
  char '$'
  return $ r

parseGroupName = do
  a <- lower <|> upper
  r <- many alphaNum
  char '$'
  return $ a : r

validateName n = do
  case (parse parseName "" n) of
    Left _ -> Nothing
    Right n -> Just n

validateBool p u = do
  case (parse p "" u) of
    Left _ -> False
    Right _ -> True

validateUsername = validateBool parseUsername
validateProjName = validateBool parseStrictName
validateGroupName = validateBool parseGroupName
         
validateEmail = isValid.B.pack

dropSpaces :: String -> String
dropSpaces = filter (not.isSpace)
                
buildUsers :: [[String]] -> IO [User]
buildUsers [] = return []
buildUsers ([name,username,email,groupName,repoName]:ls)
    | (validateUsername (username++"$")) && (validateEmail email)
                                         && (validateGroupName (groupName++"$"))
                                         && (validateProjName (repoName++"$"))
    = case (validateName (name++"$")) of
          Nothing -> error $ "Parse Error on name "++(show name)
          Just name' -> do rest <- buildUsers ls
                           return $ usr name' : rest
 where
   usr nm = User {
           -- User Creation:
           email = email,
           reset_password = "true",
           username = username,
           name = nm,
           projects_limit = "0",
           admin = "false",
           can_create_group = "false",
           can_create_project = "false",
           -- Project Creation:
           uid = -1, -- default value meaning unset
           project_name = repoName,
           namespace = groupName,
           issues_enabled = "true",
           visibility = "private"
         }
buildUsers _ = error "Parse Error: somethings wrong with the username, email, group name, or repo name fields."
