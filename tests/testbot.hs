{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B

privmsgTest :: EventFunc
privmsgTest s msg = do
  putStrLn $ show $ privmsg
  putStrLn $ show $ privmsg == "|test"
  if privmsg == "|test" || privmsg == "$kill"
    then sendMsg s origin ("DIE!")
    else return ()
  where privmsg = mMsg msg
        origin  = fromJust $ mOrigin msg

quitMsg :: EventFunc
quitMsg s msg
  | mMsg msg == "|quit" = do
    disconnect s "Bai!"
  | otherwise = return ()

events = [(Privmsg privmsgTest)
         ,(Privmsg quitMsg)
         ]

freenode = (mkDefaultConfig "irc.freenode.net" "SimpleIRCBot") 
              {cChannels = ["#()"], cEvents = events}

main = do
  --connect freenode True True
  connect freenode False True
