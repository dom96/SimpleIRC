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
  , addEvent
  , changeEvents
  ) where
  
import Network
import System.IO
import Data.Maybe
import Data.Char (isNumber)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Network.SimpleIRC.Messages
import Network.SimpleIRC.Types
import qualified Data.ByteString.Char8 as B

-- TODO: Get rid of the debug putStrLn's

internalEvents = [joinChans, pong, onJoin]

-- |Connects to a server
connect :: IrcConfig       -- ^ Configuration
           -> Bool         -- ^ Run in a new thread
           -> IO IrcServer -- ^ IrcServer instance
connect config threaded = do
  h <- connectTo (cAddr config) (PortNumber $ fromIntegral $ cPort config)
  hSetBuffering h NoBuffering
  
  cmdChan <- newChan
  
  let server = toServer config h cmdChan
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
              -> IO ()
disconnect server quitMsg = do
  write h $ "QUIT :" `B.append` quitMsg
  return ()
  where h = fromJust $ sSock server


toServer :: IrcConfig -> Handle -> Chan IrcCommand -> IrcServer
toServer config h cmdChan = 
  IrcServer (B.pack $ cAddr config) (cPort config) (B.pack $ cNick config) 
            (B.pack $ cUsername config) (B.pack $ cRealname config) (map B.pack $ cChannels config) 
            (cEvents config) (Just h) Nothing cmdChan

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

-- TODO: I think this should execute all commands that are available.
execCmds :: IrcServer -> IO IrcServer
execCmds server = do
  empty <- isEmptyChan $ sCmdChan server
  if not $ empty 
    then do cmd <- readChan $ sCmdChan server
            case cmd of (IrcAddEvent event) -> return server {sEvents = event:(sEvents server)}
                        (IrcChangeEvents events) -> return server {sEvents = events}
    else return server

listenLoop :: IrcServer -> IO ()
listenLoop s = do
  server <- execCmds s

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
      
      newServ <- foldM (\s f -> f s (parse line)) server internalEvents
      
      callEvents newServ (parse line)

      listenLoop newServ
    
-- Internal Events - They can edit the server
joinChans :: IrcServer -> IrcMessage -> IO IrcServer
joinChans server msg = do
  if code == "001"
    then do mapM (\chan -> write h $ "JOIN " `B.append` chan) (sChannels server)
            return server {sChannels = []}
    else return server
  where h    = fromJust $ sSock server
        code = mCode msg

pong :: IrcServer -> IrcMessage -> IO IrcServer
pong server msg = do
  if code == "PING"
    then do
      putStrLn "In pong function"
      write h $ "PONG :" `B.append` pingMsg
      return server
    else return server
    
  where h       = fromJust $ sSock server
        pingMsg = mMsg msg
        code    = mCode msg

onJoin :: IrcServer -> IrcMessage -> IO IrcServer
onJoin server msg
  | code == "JOIN" = do
    let nick = fromJust $ mNick msg
        chan  = mMsg msg
    if nick == sNickname server
      then return server { sChannels = chan:(sChannels server) }
      else return server
  | otherwise = return server
  
  where code = mCode msg


-- Event code
events :: IrcServer -> IrcEvent -> IrcMessage -> IO ()
events server event msg = do
  mapM eventCall events
  return ()
  where comp   = (\a -> a `eqEvent` event)
        events = filter comp (sEvents server)
        eventCall = (\obj -> (eventFunc obj) server msg)

callEvents :: IrcServer -> IrcMessage -> IO ()
callEvents server msg
  | mCode msg == "PRIVMSG"     = do
    events server (Privmsg undefined) msg
    
  | mCode msg == "PING"        = do
    events server (Ping undefined) msg

  | mCode msg == "JOIN"        = do
    events server (Join undefined) msg
  
  | mCode msg == "PART"        = do
    events server (Part undefined) msg

  | mCode msg == "MODE"        = do
    events server (Mode undefined) msg

  | mCode msg == "TOPIC"       = do
    events server (Topic undefined) msg

  | mCode msg == "INVITE"      = do
    events server (Invite undefined) msg

  | mCode msg == "KICK"        = do
    events server (Kick undefined) msg

  | mCode msg == "QUIT"        = do
    events server (Quit undefined) msg

  | mCode msg == "NICK"        = do
    events server (Nick undefined) msg

  | B.all isNumber (mCode msg) = do
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

addEvent :: IrcServer -> IrcEvent -> IO ()
addEvent s event = do
  writeChan (sCmdChan s) (IrcAddEvent event)

changeEvents :: IrcServer -> [IrcEvent] -> IO ()
changeEvents s events = do
  writeChan (sCmdChan s) (IrcChangeEvents events)

write :: Handle -> B.ByteString -> IO ()
write h msg = do
  B.putStrLn $ "<< " `B.append` msg `B.append` "\\r\\n"
  B.hPutStr h (msg `B.append` "\r\n")
