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
  contacts     <- loadContacts contactsPath
  let state = State contactsPath contacts 0 0 0
  case contacts of
    [] -> do
      proceed <- yesNo "No contacts found.  Proceed?"
      when proceed $ processTxn state
    _ -> processTxn state

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

saveContacts :: State -> IO Bool
saveContacts state = do
  let csv = map contactToRecord $ contacts state
  result <- try $ writeFile (contactsPath state) $ printCSV csv :: IO (Either SomeException ())
  case result of
    Right () -> return True
    Left _   -> return False

recordToContact :: Record -> Maybe Contact
recordToContact (addr : name : _) = Just $ Contact addr name
recordToContact _                 = Nothing

contactToRecord :: Contact -> Record
contactToRecord contact =
  [rippleAddress contact, contactName contact]

processTxn :: State -> IO ()
processTxn state = do
  addr <- prompt "Depositor's ripple address: "
  case findContact addr (contacts state) of 
    Just contact -> do
      putStrLn $ "User " ++ contactName contact ++ " found."
      graph addr
      cooldown <- yesNo "Has it been long enough since their last deposit?"
      if cooldown
        then checkCard state contact
        else restart state
    Nothing -> do
      putStrLn "User not found."
      name <- prompt "Contact name: "
      proceed <- yesNo $ "Add user " ++ name ++ " with address " ++ addr ++ " to contacts?"
      if proceed
        then do
        let contact = Contact addr name
        let state' = state { contacts = contacts state ++ [contact] }
        didSave <- saveContacts state'
        if didSave
          then checkCard state' contact
          else do
          putStrLn "Contact could not be saved."
          restart state
        else restart state

findContact :: String -> [Contact] -> Maybe Contact
findContact addr [] = Nothing
findContact addr (x : xs) =
  if addr == rippleAddress x
  then Just x
  else findContact addr xs

checkCard :: State -> Contact -> IO ()
checkCard state contact = do
  val <- promptNum $ "What is the value of the card in " ++ currency ++ "? (0 for bad card) "
  if val < txnFee
    then do
      putStrLn "This deposit is too small."
      restart state
    else startTxn state { valIn = valIn state + val } contact val

startTxn :: State -> Contact -> Double -> IO ()
startTxn state contact amt = do
  graph $ rippleAddress contact
  xrp <- promptNum "How much XRP do they have? "
  when (xrp < xrpFloor) $ do
    sendTo (rippleAddress contact) Nothing "XRP" (xrpFloor - xrp)
    pause
  if amt > limit
    then do
      waive <- yesNo "This transaction will incur an overlimit fee.  Waive it?"
      let amt' = if waive then amt - txnFee else amt * (1 - overlimitFee) - txnFee
      txnOut state contact amt'
    else txnOut state contact $ amt - txnFee

txnOut :: State -> Contact -> Double -> IO ()
txnOut state contact amt = do
  sendTo (rippleAddress contact) Nothing currency amt
  success <- yesNo "Was there sufficient liquidity?"
  if success
    then restart state { valOut = valOut state + amt }
    else do
      putStrLn "Add the following card to Trello:"
      putStrLn $ "Send " ++ show amt ++ " " ++ currency ++ " to " ++ contactName contact ++ " " ++ rippleAddress contact
      restart state { valHeld = valHeld state + amt }

restart :: State -> IO ()
restart state = do
  again <- yesNo "Process another transaction?"
  if again 
    then processTxn state
    else finalize state

finalize :: State -> IO ()
finalize state = do
  when (valIn state > 0) $ do
    sendTo ourAddress (Just ourName) currency $ valIn state
    pause
  when (valHeld state > 0) $ do
    sendTo pendingAddress (Just pendingName) currency $ valHeld state
    pause
  putStrLn ""
  putStrLn "*** FINAL REPORT ***"
  putStrLn ""
  putStrLn $ "     Amount in: " ++ show (valIn state)
  putStrLn $ "    Amount out: " ++ show (valOut state)
  putStrLn $ "   Amount held: " ++ show (valHeld state)
  putStrLn $ "Fees collected: " ++ show (valIn state - valOut state)
  putStrLn ""

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
  cachedMode <- pushControlMode LineBuffering NoBuffering True
  putStr str
  result <- getLine
  restoreControlMode cachedMode
  return result

promptNum :: String -> IO Double
promptNum prompt = do
  cachedMode <- pushControlMode LineBuffering NoBuffering True
  putStr prompt
  str <- getLine
  restoreControlMode cachedMode
  case (readMay str :: Maybe Double)  of
    Just x  -> return x
    Nothing -> do
      putStrLn "That is not a valid number."
      promptNum prompt

yesNo :: String -> IO Bool
yesNo prompt = do
  cachedMode <- pushControlMode NoBuffering NoBuffering True
  putStr $ prompt ++ " (Y/N): "
  ans <- getChar
  putChar '\n'
  restoreControlMode cachedMode
  case toUpper ans of
    'Y' -> return True
    'N' -> return False
    _   -> do
      putStrLn "Please answer yes or no."
      yesNo prompt

pause :: IO ()
pause = do
  cachedMode <- pushControlMode NoBuffering NoBuffering False
  putStr "Press any key to continue..."
  getChar
  putChar '\n'
  restoreControlMode cachedMode

pushControlMode :: BufferMode -> BufferMode -> Bool -> IO ControlMode
pushControlMode newInBufMode newOutBufMode newEchoMode = do
  inBufMode  <- hGetBuffering stdin
  outBufMode <- hGetBuffering stdout
  echoMode   <- hGetEcho      stdin
  hSetBuffering stdin  newInBufMode
  hSetBuffering stdout newOutBufMode
  hSetEcho      stdin  newEchoMode
  return $ ControlMode inBufMode outBufMode echoMode

restoreControlMode :: ControlMode -> IO ()
restoreControlMode controlMode = do
  hSetBuffering stdin  $ inBufMode  controlMode
  hSetBuffering stdout $ outBufMode controlMode
  hSetEcho      stdin  $ echoMode   controlMode

-- jl
