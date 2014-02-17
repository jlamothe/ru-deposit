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
import qualified Network.HTTP.Base as HTTP
import Safe
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
      when proceed $ processTxn contactsPath []
    _ -> processTxn contactsPath contacts

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

processTxn :: FilePath -> [Contact] -> IO ()
processTxn path contacts = do
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
checkCard path contacts contact = do
  val <- promptNum $ "What is the value of the card in " ++ currency ++ "? (0 for bad card) "
  if val < txnFee
    then do
    putStrLn "This deposit is too small."
    restart path contacts
    else txnIn path contacts contact val

txnIn :: FilePath -> [Contact] -> Contact -> Double -> IO ()
txnIn path contacts contact amt = do
  sendTo ourAddress (Just ourName) currency amt
  pause
  graph $ rippleAddress contact
  xrp <- promptNum "How much XRP do they have? "
  when (xrp < xrpFloor) $ do
    sendTo (rippleAddress contact) Nothing "XRP" (xrpFloor - xrp)
    pause
  if amt > limit
    then do
    waive <- yesNo "This transaction will incur an overlimit fee.  Waive it?"
    txnOut path contacts contact $ if waive
                                   then amt - txnFee
                                   else amt * (1 - overlimitFee) - txnFee
    else txnOut path contacts contact $ amt - txnFee

txnOut :: FilePath -> [Contact] -> Contact -> Double -> IO ()
txnOut = undefined

restart :: FilePath -> [Contact] -> IO ()
restart path contacts = do
  again <- yesNo "Process another transaction?"
  when again $ processTxn path contacts

graph :: String -> IO ()
graph addr = putStrLn $ "https://ripple.com/graph/#" ++ addr

sendTo :: String -> Maybe String -> String -> Double -> IO ()
sendTo addr name currency amt = do
  let prefix = "https://ripple.com//send?to=" ++ addr
  let suffix = "&amount=" ++ show amt ++ "/" ++ currency
  case name of
    Just x -> do
      putStrLn $ prefix ++ "&name=" ++ HTTP.urlEncode x ++ suffix
      putStrLn $ "Send " ++ show amt ++ " " ++ currency ++ " to " ++ x
    Nothing -> do
      putStrLn $ prefix ++ suffix
      putStrLn $ "Send " ++ show amt ++ " " ++ currency ++ " to " ++ addr

prompt :: String -> IO String
prompt str = do
  bufMode <- hGetBuffering stdout
  hSetBuffering stdout NoBuffering
  putStr str
  result <- getLine
  hSetBuffering stdout bufMode
  return result

promptNum :: String -> IO Double
promptNum prompt = do
  bufMode <- hGetBuffering stdout
  hSetBuffering stdout NoBuffering
  putStr prompt
  str <- getLine
  hSetBuffering stdout bufMode
  case (readMay str :: Maybe Double)  of
    Just x  -> return x
    Nothing -> do
      putStrLn "That is not a valid number."
      promptNum prompt

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

pause :: IO ()
pause = do
  inBufMode <- hGetBuffering stdin
  outBufMode <- hGetBuffering stdout
  echoMode <- hGetEcho stdin
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  putStr "Press any key to continue..."
  getChar
  putChar '\n'
  hSetBuffering stdin inBufMode
  hSetBuffering stdout outBufMode
  hSetEcho stdin echoMode

-- jl
