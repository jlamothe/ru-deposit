-- Ripple Union Deposit Workflow

-- Copyright (C) 2014 Jonathan Lamothe <jonathan@jlamothe.net>

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see
-- <http://www.gnu.org/licenses/>.

module Main where

import Control.Exception
import Control.Monad
import Data.Char
import Data.Maybe
import System.Environment
import System.IO
import Text.CSV

import Config
import Types

main = do
  contactsPath <- getContactsPath
  contacts <- loadContacts contactsPath
  case contacts of
    [] -> do
      proceed <- yesNo "No contacts found.  Proceed?"
      when proceed $ processTXN contactsPath []
    _ -> processTXN contactsPath contacts

getContactsPath :: IO FilePath
getContactsPath = do
  args <- getArgs
  case args of
    (x : _) -> return x
    _       -> prompt "Path to contacts: "

loadContacts :: FilePath -> IO [Contact]
loadContacts path = do
  contents <- try $ readFile path :: IO (Either SomeException String)
  case contents of
    Right contents ->
      case parseCSV path contents of
        Right xs -> return $ mapMaybe recordToContact xs
        Left _   -> do
          putStrLn "Error parsing contacts"
          return []
    Left _ -> do
      putStrLn "Error reading contacts"
      return []

recordToContact :: Record -> Maybe Contact
recordToContact (addr : name : _) = Just $ Contact addr name
recordToContact _                 = Nothing

processTXN :: FilePath -> [Contact] -> IO ()
processTXN path contacts = do
  addr <- prompt "Depositor's ripple address: "
  case findContact addr contacts of
    Just contact -> do
      putStrLn $ "User " ++ contactName contact ++ " found."
      checkCard path contacts contact
    Nothing -> do
      putStrLn "User not found."
      name <- prompt "Contact name: "
      let contact = Contact addr name
      checkCard path (contacts ++ [contact]) contact

findContact :: String -> [Contact] -> Maybe Contact
findContact addr [] = Nothing
findContact addr (x : xs) =
  if addr == rippleAddress x
  then Just x
  else findContact addr xs

checkCard :: FilePath -> [Contact] -> Contact -> IO ()
checkCard = undefined

prompt :: String -> IO String
prompt str = do
  bufMode <- hGetBuffering stdout
  hSetBuffering stdout NoBuffering
  putStr str
  result <- getLine
  hSetBuffering stdout bufMode
  return result

yesNo :: String -> IO Bool
yesNo prompt = do
  inBufMode <- hGetBuffering stdin
  outBufMode <- hGetBuffering stdout
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStr $ prompt ++ " (Y/N): "
  ans <- getChar
  putChar '\n'
  hSetBuffering stdin inBufMode
  hSetBuffering stdout outBufMode
  case toUpper ans of
    'Y' -> return True
    'N' -> return False
    _   -> do
      putStrLn "Please answer yes or no."
      yesNo prompt

-- jl
