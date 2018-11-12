{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.IO (readFile)
import Data.Time (getCurrentTime)
import Data.Aeson (encode, FromJSON, ToJSON)
import Control.Monad (replicateM)
import System.Environment (getArgs)
import Web.Scotty
import Data.Monoid ((<>))
import GHC.Generics

greet name = "Hello " ++ name ++ "!"

printNumbers = do
  putStrLn (show (3+4))

printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents

printTime = do
  time <- getCurrentTime
  putStrLn (show time)

numbers :: [Int]
numbers = [1, 2, 3, 4]

main0 :: IO ()
main0 = do
  putStrLn $ greet "oscar"
  putStrLn $ greet "world"
  printNumbers
  printConfig
  print $ encode numbers

maybeMonad :: Maybe Int
maybeMonad = do
  a <- Just 6
  b <- Just a
  return b

exercise :: Int -> IO ()
exercise n = do
  putStrLn $ "enter your message"
  m <- getLine
  replicateM n $ putStrLn m
  return ()


main1 :: IO ()
main1 = do
  args <- getArgs
  file <- mapM readFile args
  putStrLn $ file!!0

-- Scotty part of the project

data User =
  User { userId :: Int, userName :: String }
  deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }

allUsers :: [User]
allUsers = [bob, jenny]

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

main :: IO ()
main = do
  putStrLn "Starting server ..."
  scotty 3000 $ do
    get "/hello/:name" $ do
      name <- param "name"
      text ("hello " <> name <> "!")
    get "/users" $ do
      json allUsers
    get "/users/:id" $ do
      id <- param "id"
      json $ filter (matchesId id) allUsers 