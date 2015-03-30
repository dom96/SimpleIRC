{-# LANGUAGE OverloadedStrings #-}

import Network.SimpleIRC
import qualified Data.ByteString.Char8 as B

-- | Other way to express this is:
-- | onMessage :: MIrc -> IrcMessage -> IO ()
-- | MIrc is the irc server
onMessage :: EventFunc
onMessage server message
  | actualMessage == (B.pack "|hai") = do
      sendMsg server channel "hai thar!"
  | B.isPrefixOf "|say" actualMessage = do
    sendMsg server channel (B.drop 1 $ B.dropWhile (/= ' ') actualMessage)
  | otherwise = putStrLn $ show message
  where channel :: B.ByteString
        channel = case mChan message of
                   Just channel -> channel
                   Nothing -> B.pack "" -- Not sure if an empty string is a good idea.
        actualMessage :: B.ByteString
        actualMessage = mMsg message


freenode :: IrcConfig
freenode = (mkDefaultConfig "irc.freenode.net" "zippy-bot")
            { cChannels = ["#()"] -- Channels to join on connect
            , cEvents   = events -- Events to bind
            }


events :: [IrcEvent]
events = [(Privmsg onMessage)]


main :: IO (Either IOError MIrc)
main = do
  connect freenode False True
