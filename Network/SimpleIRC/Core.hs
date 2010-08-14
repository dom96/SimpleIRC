-- |
-- Module : Network.SimpleIRC.Core
-- Copyright : (c) Dominik Picheta 2010
-- License : BSD3
--
-- Maintainer : morfeusz8@gmail.com
-- Stability : experimental
-- Portability : non-portable
--
-- Core module
--
{-# LANGUAGE OverloadedStrings #-}
module Network.SimpleIRC.Core
  ( 
    -- * Datatypes
    IrcConfig(..)
  , IrcServer(..)
  , IrcEvent(..)
  
    -- * Functions
  , connect
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
import qualified Data.ByteString.Char8 as B

-- TODO: Get rid of the debug putStrLn's

data IrcConfig = IrcConfig
  { cAddr     :: String   -- ^ Server address to connect to
  , cPort     :: Int      -- ^ Server port to connect to
  , cNick     :: String   -- ^ Nickname
  , cUsername :: String   -- ^ Username
  , cRealname :: String   -- ^ Realname
  , cChannels :: [String]   -- ^ List of channels to join on connect
  , cEvents   :: [IrcEvent] -- ^ Events to bind
  }

data IrcServer = IrcServer
  { sAddr     :: B.ByteString
  , sPort     :: Int
  , sNickname :: B.ByteString
  , sUsername :: B.ByteString
  , sRealname :: B.ByteString
  , sChannels :: [B.ByteString]
  , sEvents   :: [IrcEvent]
  , sSock     :: Maybe Handle
  , sListenThread :: Maybe ThreadId
  } deriving Show

type EventFunc = (IrcServer -> IrcMessage -> IO IrcServer)

-- When adding events here, remember add them in callEvents and in eventFunc
-- AND also in the Show instance and Eq instance
data IrcEvent = 
    Privmsg EventFunc -- ^ PRIVMSG
  | Numeric EventFunc -- ^ Numeric, 001, 002, 372 etc.
  | Ping EventFunc    -- ^ PING
  
instance Show IrcEvent where
  show (Privmsg _) = "IrcEvent - Privmsg"
  show (Numeric _) = "IrcEvent - Numeric"
  show (Ping    _) = "IrcEvent - Ping"

instance Eq IrcEvent where
  (Privmsg _) == (Privmsg _) = True
  (Numeric _) == (Numeric _) = True
  (Ping    _) == (Ping    _) = True
  _ == _                     = False

internalEvents = [(Numeric joinChans), (Ping pong)]

-- |Connects to a server
connect :: IrcConfig -- ^ Configuration
           -> Bool   -- ^ Run in a new thread
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
    
toServer :: IrcConfig -> [IrcEvent] -> Handle -> IrcServer
toServer config events h = 
  IrcServer (B.pack $ cAddr config) (cPort config) (B.pack $ cNick config) 
            (B.pack $ cUsername config) (B.pack $ cRealname config) (map B.pack $ cChannels config) 
            (cEvents config ++ events) (Just h) Nothing

-- |Sends a QUIT command to the server.
disconnect :: IrcServer
              -> B.ByteString -- ^ Quit message
              -> IO IrcServer
disconnect server quitMsg = do
  write h $ "QUIT :" `B.append` quitMsg
  return server
  where h = fromJust $ sSock server

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
  line <- B.hGetLine h
  B.putStrLn $ (B.pack ">> ") `B.append` line
  
  newServ <- callEvents server (parse line)
  -- Call the listenLoop again.
  -- Events can edit the server.
  listenLoop newServ
  where h = fromJust $ sSock server

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
  where comp   = (\a -> a == event)
        events = filter comp (sEvents server)
        eventCall = (\s obj -> (eventFunc obj) s msg)

callEvents :: IrcServer -> IrcMessage -> IO IrcServer
callEvents server msg
  | fromJust (mCode msg) == "PRIVMSG"     = do
    putStrLn "Calling PRIVMSG"
    events server (Privmsg undefined) msg
    
  | fromJust (mCode msg) == "PING"        = do
    putStrLn "Calling PING"
    events server (Ping undefined) msg

  | B.all isNumber (fromJust $ mCode msg) = do
    events server (Numeric undefined) msg
  
  | otherwise                = do return server

eventFunc :: IrcEvent -> EventFunc
eventFunc (Privmsg f) = f
eventFunc (Numeric f) = f
eventFunc (Ping    f) = f

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
