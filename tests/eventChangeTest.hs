{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Control.Concurrent.Chan
import qualified Data.ByteString.Char8 as B

onMessage1 :: EventFunc
onMessage1 s m
  | msg == "|change" = do
    changeEvents s [(Privmsg onMessage1)]
  | msg == "|set" = do
    changeEvents s events
  where msg = mMsg m

onMessage :: EventFunc
onMessage s m
  | msg == "|hai" = do
    sendMsg s chan "hai thar!"
  | B.isPrefixOf "|say" msg = do
    sendMsg s chan (B.drop 1 $ B.dropWhile (/= ' ') msg)
  | otherwise = return ()
  where chan = fromJust $ mChan m
        msg = mMsg m
        
events = [(Privmsg onMessage), (Privmsg onMessage1)]

freenode = IrcConfig 
  "irc.freenode.net" -- Address
  6667 -- Port
  "SimpleIRCBot" -- Nickname
  "simpleirc"  -- Username
  "simple irc" -- Realname
  ["#()"] -- Channels to join on connect
  events -- Events to bind

main = do
  connect freenode False
