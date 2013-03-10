{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Control.Concurrent.Chan
import qualified Data.ByteString.Char8 as B

onMessage :: EventFunc
onMessage s m
  | msg == "|hai" = do
    sendMsg s chan "hai thar!"
  | B.isPrefixOf "|say" msg = do
    sendMsg s chan (B.drop 1 $ B.dropWhile (/= ' ') msg)
  | msg == "|nick" = do 
    sendCmd s (MNick "SimpleIRCBot1")
  | msg == "|nick?" = do
    nick <- getNickname s
    sendMsg s chan nick
  | msg == "|chans?" = do 
    chans <- getChannels s
    sendMsg s chan (B.pack $ show chans)
  | msg == "|part" = do 
    sendCmd s (MPart chan "BAI")
  | B.isPrefixOf "|join" msg = do
    sendCmd s (MJoin (B.drop 1 $ B.dropWhile (/= ' ') msg) Nothing)
  | otherwise = return ()
  where chan = fromJust $ mChan m
        msg = mMsg m
        
events = [(Privmsg onMessage)]

freenode = (mkDefaultConfig "irc.freenode.net" "SimpleIRCBot")
              { cChannels = ["#()", "#HSBotTest"], cEvents = events}

main = do
  connect freenode False True
