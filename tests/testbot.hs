{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B

privmsgTest :: EventFunc
privmsgTest s msg = do
  putStrLn $ show $ privmsg
  putStrLn $ show $ privmsg == "|test"
  if privmsg == "|test" || privmsg == "$kill"
    then do sendMsg s chan ("Works! -- "
                `B.append` (B.pack $ show $ sChannels s) `B.append`
                " -- " `B.append` (B.pack $ show $ sAddr s))
    else return ()
  where privmsg = mMsg msg
        chan    = fromJust $ mChan msg

quitMsg :: EventFunc
quitMsg s msg
  | mMsg msg == "|quit" = do
    disconnect s "Bai!"
  | otherwise = return ()

onDisconnect :: IrcServer -> IO ()
onDisconnect s = B.putStrLn $ "Disconnected from " `B.append` (sAddr s)

events = [(Privmsg privmsgTest)
         ,(Privmsg quitMsg)
         ,(Disconnect onDisconnect)
         ]

freenode = IrcConfig 
  "irc.freenode.net" 
  6667 
  "SimpleIRCBot" 
  "simpleirc" 
  "test 1" 
  ["#()"] 
  events 

ninthbit = IrcConfig 
  "irc.ninthbit1212.net" 
  6667 
  "SimpleIRCBot" 
  "simpleirc" 
  "test 1" 
  ["#bots"] 
  events 

main = do
  --connect freenode True True
  ret <- connect ninthbit False True
  either (\e -> putStrLn $ "Unable to connect: " ++ (show e)) (\_ -> return ()) ret
