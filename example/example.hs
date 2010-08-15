{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B

onMessage :: EventFunc
onMessage s m
  | msg == "|hai" = do
    sendMsg s chan "hai thar!"
  | B.isPrefixOf "|say" msg = do
    sendMsg s chan (B.drop 1 $ B.dropWhile (/= ' ') msg)
  | otherwise = return ()
  where chan = fromJust $ mChan m
        msg = fromJust $ mMsg m
        
events = [(Privmsg onMessage)]

ninthbit = IrcConfig 
  "irc.freenode.net" -- Address
  6667 -- Port
  "SimpleIRCBot" -- Nickname
  "simpleirc"  -- Username
  "simple irc" -- Realname
  ["#()"] -- Channels to join on connect
  events -- Events to bind

main = do
  connect ninthbit False
