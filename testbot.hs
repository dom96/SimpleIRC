{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B

privmsgTest :: IrcServer -> IrcMessage -> IO IrcServer
privmsgTest s msg = do
  putStrLn $ show $ privmsg
  putStrLn $ show $ privmsg == "|test"
  if privmsg == "|test" || privmsg == "$kill"
    then do sendMsg s chan ("Works! -- "
                `B.append` (B.pack $ show $ sChannels s) `B.append`
                " -- " `B.append` (B.pack $ show $ sAddr s))
            return s
    else return s
  where privmsg = fromJust $ mMsg msg
        chan    = fromJust $ mChan msg

init1 :: IrcServer -> IrcMessage -> IO IrcServer
init1 s msg
  | (fromJust $ mMsg msg) == "|init" = do
    let chans = sChannels s
    return s {sChannels = "test" : chans}
  | otherwise = return s

init2 :: IrcServer -> IrcMessage -> IO IrcServer
init2 s msg
  | (fromJust $ mMsg msg) == "|init" = do
    let chans = sChannels s
    return s {sChannels = "test2" : chans}
  | otherwise = return s

main = do
  let freenode = IrcConfig "irc.freenode.net" 6667 "SimpleIRCBot" "simpleirc" "test 1" ["##XAMPP", "#()"] [(Privmsg privmsgTest), (Privmsg init1),(Privmsg init2)]
  let ninthbit = IrcConfig "irc.ninthbit.net" 6667 "SimpleIRCBot" "simpleirc" "test 1" ["#bots"] [(Privmsg privmsgTest)]
  connect freenode True
  connect ninthbit False
