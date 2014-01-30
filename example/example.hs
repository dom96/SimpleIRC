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
  | otherwise = putStrLn $ show m
  where chan = fromJust $ mChan m
        msg = mMsg m

events = [(Privmsg onMessage)]

freenode = (mkDefaultConfig "irc.freenode.net" "SimpleIRCBot")
            { cChannels = ["#()"] -- Channels to join on connect
            , cEvents   = events -- Events to bind
            }

main = do
  connect freenode False True
