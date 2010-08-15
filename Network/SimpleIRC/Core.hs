-- |
-- Module : Network.SimpleIRC.Core
-- Copyright : (c) Dominik Picheta 2010
-- License : BSD3
--
-- Maintainer : morfeusz8@gmail.com
-- Stability : Alpha
-- Portability : non-portable
--
-- Core module
--
{-# LANGUAGE OverloadedStrings #-}
module Network.SimpleIRC.Core
  ( 
    -- * Functions
    connect
  , disconnect
  , sendRaw
  , sendMsg
  ) where
  
import Network
import System.IO
import Data.Maybe
import Data.Char (isNumber)
import Control.Monad
import Control.Concurrent
import Network.SimpleIRC.Messages
import Network.SimpleIRC.Types
import qualified Data.ByteString.Char8 as B

-- TODO: Get rid of the debug putStrLn's

internalEvents = [(Numeric joinChans), (Ping pong)]

-- |Connects to a server
connect :: IrcConfig       -- ^ Configuration
           -> Bool         -- ^ Run in a new thread
           -> IO IrcServer -- ^ IrcServer instance
connect config threaded = do
  h <- connectTo (cAddr config) (PortNumber $ fromIntegral $ cPort config)
  hSetBuffering h NoBuffering
  
  let server = toServer config internalEvents h
  -- Initialize connection with the server
  greetServer server
  
  -- Start listening
  if threaded
    then do listenId <- forkIO (listenLoop server)
            return server {sListenThread = Just listenId}
    else do listenLoop server
            return server
    
-- |Sends a QUIT command to the server.
disconnect :: IrcServer
              -> B.ByteString -- ^ Quit message
              -> IO IrcServer
disconnect server quitMsg = do
  write h $ "QUIT :" `B.append` quitMsg
  return server
  where h = fromJust $ sSock server


toServer :: IrcConfig -> [IrcEvent] -> Handle -> IrcServer
toServer config events h = 
  IrcServer (B.pack $ cAddr config) (cPort config) (B.pack $ cNick config) 
            (B.pack $ cUsername config) (B.pack $ cRealname config) (map B.pack $ cChannels config) 
            (cEvents config ++ events) (Just h) Nothing  

greetServer :: IrcServer -> IO IrcServer
greetServer server = do
  write h $ "NICK " `B.append` nick
  write h $ "USER " `B.append` user `B.append` " " `B.append`
      user `B.append` " " `B.append` addr `B.append` " :" `B.append` real
  
  return server
  where nick = sNickname server
        user = sUsername server
        real = sRealname server
        addr = sAddr server
        h    = fromJust $ sSock server

listenLoop :: IrcServer -> IO ()
listenLoop server = do
  let h = fromJust $ sSock server
  eof <- hIsEOF h
  if eof 
    then do
      let comp   = (\a -> a `eqEvent` (Disconnect undefined))
          events = filter comp (sEvents server)
          eventCall = (\obj -> (eventFuncD obj) server)
      putStrLn $ show events
      mapM eventCall events
      return ()
    else do
      line <- B.hGetLine h
      B.putStrLn $ (B.pack ">> ") `B.append` line
      
      newServ <- callEvents server (parse line)
      -- Call the listenLoop again.
      -- Events can edit the server.
      listenLoop newServ
    
-- Internal Events
joinChans :: EventFunc
joinChans server msg = do
  if code == "001"
    then do mapM (\chan -> write h $ "JOIN " `B.append` chan) (sChannels server)
            return server {sChannels = []}
    else return server
  where h    = fromJust $ sSock server
        code = (fromJust $ mCode msg)

pong :: EventFunc
pong server msg = do
  putStrLn "In pong function"
  write h $ "PONG :" `B.append` pingMsg
  return server
  where h       = fromJust $ sSock server
        pingMsg = fromJust $ mMsg msg
        code    = fromJust $ mCode msg

events :: IrcServer -> IrcEvent -> IrcMessage -> IO IrcServer
events server event msg = do
  putStrLn $ show events
  
  foldM eventCall server events
  where comp   = (\a -> a `eqEvent` event)
        events = filter comp (sEvents server)
        eventCall = (\s obj -> (eventFunc obj) s msg)

callEvents :: IrcServer -> IrcMessage -> IO IrcServer
callEvents server msg
  | fromJust (mCode msg) == "PRIVMSG"     = do
    events server (Privmsg undefined) msg
    
  | fromJust (mCode msg) == "PING"        = do
    events server (Ping undefined) msg

  | fromJust (mCode msg) == "JOIN"        = do
    events server (Join undefined) msg
  
  | fromJust (mCode msg) == "PART"        = do
    events server (Part undefined) msg

  | fromJust (mCode msg) == "MODE"        = do
    events server (Mode undefined) msg

  | fromJust (mCode msg) == "TOPIC"       = do
    events server (Topic undefined) msg

  | fromJust (mCode msg) == "INVITE"      = do
    events server (Invite undefined) msg

  | fromJust (mCode msg) == "KICK"        = do
    events server (Kick undefined) msg

  | fromJust (mCode msg) == "QUIT"        = do
    events server (Quit undefined) msg

  | fromJust (mCode msg) == "NICK"        = do
    events server (Nick undefined) msg

  | B.all isNumber (fromJust $ mCode msg) = do
    events server (Numeric undefined) msg
  
  | otherwise                = do
    events server (RawMsg undefined) msg

(Privmsg _) `eqEvent` (Privmsg _) = True
(Numeric _) `eqEvent` (Numeric _) = True
(Ping    _) `eqEvent` (Ping    _) = True
(Join    _) `eqEvent` (Join    _) = True
(Part    _) `eqEvent` (Part    _) = True
(Mode    _) `eqEvent` (Mode    _) = True
(Topic   _) `eqEvent` (Topic   _) = True
(Invite  _) `eqEvent` (Invite  _) = True
(Kick    _) `eqEvent` (Kick    _) = True
(Quit    _) `eqEvent` (Quit    _) = True
(Nick    _) `eqEvent` (Nick    _) = True
(RawMsg  _) `eqEvent` (RawMsg  _) = True
(Disconnect  _) `eqEvent` (Disconnect  _) = True
_ `eqEvent` _                     = False

eventFunc :: IrcEvent -> EventFunc
eventFunc (Privmsg f) = f
eventFunc (Numeric f) = f
eventFunc (Ping    f) = f
eventFunc (Join    f) = f
eventFunc (Part    f) = f
eventFunc (Mode    f) = f
eventFunc (Topic   f) = f
eventFunc (Invite  f) = f
eventFunc (Kick    f) = f
eventFunc (Quit    f) = f
eventFunc (Nick    f) = f
eventFunc (RawMsg  f) = f
eventFuncD (Disconnect  f) = f

-- |Sends a raw command to the server
sendRaw :: IrcServer -> B.ByteString -> IO ()
sendRaw server msg = write (fromJust $ sSock server) msg

-- |Sends a message to a channel
sendMsg :: IrcServer 
           -> B.ByteString -- ^ Channel
           -> B.ByteString -- ^ Message
           -> IO ()
sendMsg server chan msg =
  sendRaw server ("PRIVMSG " `B.append` chan `B.append` " :" `B.append` msg)

write :: Handle -> B.ByteString -> IO ()
write h msg = do
  B.putStrLn $ "<< " `B.append` msg `B.append` "\\r\\n"
  B.hPutStr h (msg `B.append` "\r\n")
