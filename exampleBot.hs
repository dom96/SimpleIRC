{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B

privmsgTest :: IrcServer -> IrcMessage -> IO IrcServer
privmsgTest s msg = do
  putStrLn $ show $ privmsg
  putStrLn $ show $ privmsg == "|test"
  if privmsg == "|test" || privmsg == "$kill"
    then do sendRaw s $ "PRIVMSG " `B.append` chan `B.append` " :Works! -- "
                `B.append` (B.pack $ show $ sChannels s) `B.append` " -- " `B.append` (B.pack $ show $ sAddr s)
            return s
    else return s
  where privmsg = fromJust $ mMsg msg
        chan    = fromJust $ mChan msg

main = do
  let freenode = IrcConfig "irc.freenode.net" 6667 "SimpleIRCBot" "simpleirc" "test 1" ["#()"] [(Privmsg privmsgTest)]
  let ninthbit = IrcConfig "irc.ninthbit.net" 6667 "SimpleIRCBot" "simpleirc" "test 1" ["#bots"] [(Privmsg privmsgTest)]
  connect freenode True
  connect ninthbit False
