{-# LANGUAGE OverloadedStrings #-}
import Network.SimpleIRC
import Data.Maybe
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as B

onDisconnect :: MIrc -> IO ()
onDisconnect mIrc = do
  addr <- getAddress mIrc
  putStrLn $ "Disconnected from " ++ (B.unpack addr)
  m <- reconnect mIrc
  either (\err -> putStrLn $ "Unable to reconnect: " ++ show err)
         (\_   -> putStrLn "Successfully reconnected!")
         m

events = [(Disconnect onDisconnect)]

freenode = defaultConfig { cAddr = "irc.ninthbit.net", cNick = "SimpleIRCBot", cChannels = ["#bots"], cEvents = events}

main = do
  connect freenode True True
  waitForever
  where waitForever = do 
          threadDelay 50000
          waitForever
