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
  processTXN contactsPath contacts

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
  result <- parseCSVFromFile path
  case result of
    Right xs -> return $ mapMaybe recordToContact xs
    _        -> do
      putStrLn "Error loading contacts"
      return []

recordToContact :: Record -> Maybe Contact
recordToContact (addr : name : _) = Just $ Contact addr name
recordToContact _                 = Nothing

processTXN :: FilePath -> [Contact] -> IO ()
processTXN = undefined

-- jl
