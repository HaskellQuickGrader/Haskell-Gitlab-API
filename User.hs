module User where

import Data.Strings

data User = User {
      -- User Creation:
      email :: String,
      reset_password :: String,
      username :: String,
      name :: String,
      projects_limit :: String,
      admin :: String,
      can_create_group :: String,
      can_create_project :: String,
      -- Project Creation:
      uid :: Integer,
      project_name :: String,
      namespace :: String,
      issues_enabled :: String,
      visibility :: String      
} deriving (Show)

updateUserID :: User -> Integer -> User
updateUserID u uid = u { uid = uid }

findUser :: [User] -> String -> Maybe User
findUser [] _ = Nothing
findUser (u:us) nm | (username u) `strEq` nm = Just u
                   | otherwise = findUser us nm

namespaces :: [User] -> [String]
namespaces = map namespace

