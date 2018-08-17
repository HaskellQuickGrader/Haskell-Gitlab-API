{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module SystemHook.Push where

import Query
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.Char8 as CH

data Project = Project {
      name :: String,
      description :: String,
      web_url :: String,
      avatar_url :: String,
      git_ssh_url :: String,
      git_http_url :: String,
      namespace :: String,
      visibility_level :: Integer,
      path_with_namespace :: String,
      default_branch :: String,
      homepage :: String,
      url :: String,
      ssh_url :: String,
      http_url :: String
} deriving (Show)

data Repository = Repository {
      name :: String,
      url :: String,
      description :: String,
      homepage :: String,
      git_http_url :: String,
      git_ssh_url :: String,
      visibility_level :: Integer
} deriving (Show)

data Author = Author {
      name :: String,
      email :: String
} deriving (Show)

data Commit = Commit {
      id :: Integer,
      message :: String,
      timestamp :: String,
      url :: String,
      author :: Author
} deriving (Show)

data Push = Push {
      event_name :: String,
      before :: String,
      after :: String,
      ref :: String,
      checkout_sha :: String,
      user_id :: String,
      user_name :: String,
      user_email :: String,
      user_avatar :: String,
      project_id :: Integer,
      project :: Project,
      repository :: Repository,             
      commits :: [Commit],
      total_commits_count :: Integer
    } deriving (Show)

type SResp = Push

getPushRsp :: Either String Object -> Either String SResp
getPushRsp (Right r) = flip parseEither r $ (\o -> do
  event_name <- o .: "event_name"
  before <- o .: "before"
  after <- o .: "after"
  ref <- o .: "ref"
  checkout_sha <- o .: "checkout_sha"
  user_id <- o .: "user_id"
  user_name <- o .: "user_name"
  user_email <- o .: "user_email"
  user_avatar <- o .: "user_avatar"
  project_id <- o .: "project_id"
  project <- do j <- o .: "project"
                name <- j .: "name"
                description <- j .: "description"
                web_url <- j .: "web_url"
                avatar_url <- j .: "avatar_url"
                git_ssh_url <- j .: "git_ssh_url"
                git_http_url <- j .: "git_http_url"
                namespace <- j .: "namespace"
                visibility_level <- j .: "visibility_level"
                path_with_namespace <- j .: "path_with_namespace"
                default_branch <- j .: "default_branch"
                homepage <- j .: "homepage"
                url <- j .: "url"
                ssh_url <- j .: "ssh_url"
                http_url  <- j .: "http_url"
                return $ Project name description web_url avatar_url
                                 git_ssh_url git_http_url namespace
                                 visibility_level path_with_namespace
                                 default_branch homepage url ssh_url http_url
  repository <- do j <- o .: "repository"
                   name' <- j .: "name"
                   url' <- j .: "url"
                   description' <- j .: "description"
                   homepage' <- j .: "homepage"
                   git_http_url' <- j .: "git_http_url"
                   git_ssh_url' <- j .: "git_ssh_url"
                   visibility_level' <- j .: "visibility_level"
                   return $ Repository name' url' description' homepage' git_http_url' git_ssh_url' visibility_level'
  commits <- return [] -- TODO: Not sure how to parse a JSON list yet.
  total_commits_count <- o .: "total_commits_count"
             
  return $ Push event_name before after ref
                checkout_sha user_id user_name
                user_email user_avatar project_id project repository commits total_commits_count)
getPushRsp (Left e) = (Left e)

getTagPushRsp :: Either String Object -> Either String SResp
getTagPushRsp (Right r) = flip parseEither r $ (\o -> do
  event_name <- o .: "event_name"
  before <- o .: "before"
  after <- o .: "after"
  ref <- o .: "ref"
  checkout_sha <- o .: "checkout_sha"
  user_id <- o .: "user_id"
  user_name <- o .: "user_name"
  user_avatar <- o .: "user_avatar"
  project_id <- o .: "project_id"
  project <- do j <- o .: "project"
                name <- j .: "name"
                description <- j .: "description"
                web_url <- j .: "web_url"
                avatar_url <- j .: "avatar_url"
                git_ssh_url <- j .: "git_ssh_url"
                git_http_url <- j .: "git_http_url"
                namespace <- j .: "namespace"
                visibility_level <- j .: "visibility_level"
                path_with_namespace <- j .: "path_with_namespace"
                default_branch <- j .: "default_branch"
                homepage <- j .: "homepage"
                url <- j .: "url"
                ssh_url <- j .: "ssh_url"
                http_url  <- j .: "http_url"
                return $ Project name description web_url avatar_url
                                 git_ssh_url git_http_url namespace
                                 visibility_level path_with_namespace
                                 default_branch homepage url ssh_url http_url
  repository <- do j <- o .: "repository"
                   name' <- j .: "name"
                   url' <- j .: "url"
                   description' <- j .: "description"
                   homepage' <- j .: "homepage"
                   git_http_url' <- j .: "git_http_url"
                   git_ssh_url' <- j .: "git_ssh_url"
                   visibility_level' <- j .: "visibility_level"
                   return $ Repository name' url' description' homepage' git_http_url' git_ssh_url' visibility_level'
  commits <- return [] -- TODO: Not sure how to parse a JSON list yet.
  total_commits_count <- o .: "total_commits_count"
             
  return $ Push event_name before after ref
                checkout_sha user_id user_name
                "" user_avatar project_id project repository commits total_commits_count)
getTagPushRsp (Left e) = (Left e)

pushDecodeRsp :: ByteString -> Either String (Either ErrorResp SResp)
pushDecodeRsp b = decodeGLResp getPushRsp b

tagPushDecodeRsp :: ByteString -> Either String (Either ErrorResp SResp)
tagPushDecodeRsp b = decodeGLResp getTagPushRsp b

test = "{\"event_name\":\"repository_update\",
         \"user_id\":1,
         \"user_name\":\"Harley Eades\",
         \"user_email\":\"harley.eades@gmail.com\",\"user_avatar\":null,\"project_id\":16,\"project\":{\"name\":\"heades\",\"description\":\"\",\"web_url\":\"http://gitlab.metatheorem.org/CSCI3300/heades\",\"avatar_url\":null,\"git_ssh_url\":\"git@gitlab.metatheorem.org:CSCI3300/heades.git\",\"git_http_url\":\"http://gitlab.metatheorem.org/CSCI3300/heades.git\",\"namespace\":\"CSCI3300\",\"visibility_level\":0,\"path_with_namespace\":\"CSCI3300/heades\",\"default_branch\":\"master\",\"ci_config_path\":null,\"homepage\":\"http://gitlab.metatheorem.org/CSCI3300/heades\",\"url\":\"git@gitlab.metatheorem.org:CSCI3300/heades.git\",\"ssh_url\":\"git@gitlab.metatheorem.org:CSCI3300/heades.git\",\"http_url\":\"http://gitlab.metatheorem.org/CSCI3300/heades.git\"},\"changes\":[{\"before\":\"ddc6ca3be47497a036990c59602ad38cb54c817f\",\"after\":\"11344193f557eec6dc9ec62753d1de574592e03d\",\"ref\":\"refs/heads/master\"}],\"refs\":[\"refs/heads/master\"]}"