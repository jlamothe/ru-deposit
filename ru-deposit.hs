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
import Data.Char
import Data.Maybe
import System.Environment
import System.IO
import Text.CSV

import Config
import Types

main = do
  hSetBuffering stdout NoBuffering
  contactsPath <- getContactsPath
  contacts <- loadContacts contactsPath
  case contacts of
    [] -> do
      proceed <- yesNo "No contacts found.  Proceed?"
      if proceed
        then processTXN contactsPath []
        else return ()
    _ -> processTXN contactsPath contacts

getContactsPath :: IO FilePath
getContactsPath = do
  args <- getArgs
  case args of
    (x : _) -> return x
    _       -> do
      putStr "Path to contacts: "
      getLine

loadContacts :: String -> IO [Contact]
loadContacts path = do
  contents <- try $ readFile path :: IO (Either SomeException String)
  case contents of
    Right contents -> do
      case parseCSV path contents of
        Right xs -> return $ mapMaybe recordToContact xs
        _        -> do
          putStrLn "Error parsing contacts"
          return []
    _ -> do
      putStrLn "Error reading contacts"
      return []

recordToContact :: Record -> Maybe Contact
recordToContact (addr : name : _) = Just $ Contact addr name
recordToContact _                 = Nothing

processTXN :: FilePath -> [Contact] -> IO ()
processTXN = undefined

yesNo :: String -> IO Bool
yesNo prompt = do
  bufMode <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  putStr $ prompt ++ " (Y/N): "
  ans <- getChar
  putChar '\n'
  hSetBuffering stdin bufMode
  case toUpper ans of
    'Y' -> return True
    'N' -> return False
    _   -> do
      putStrLn "Please answer yes or no."
      yesNo prompt

-- jl
