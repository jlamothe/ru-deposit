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

import System.IO
import Text.CSV

import Config

main = do
  parsedContacts <- getContactsPath >>= parseCSVFromFile
  case parsedContacts of
    Left _         -> putStrLn "Error reading contacts file"
    Right contacts -> start contacts

getContactsPath :: IO String
getContactsPath = undefined

start :: CSV -> IO ()
start = undefined

-- jl
