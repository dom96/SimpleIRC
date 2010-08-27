-- |
-- Module : Network.SimpleIRC.Core
-- Copyright : (c) Dominik Picheta 2010
-- License : BSD3
--
-- Maintainer : morfeusz8@gmail.com
-- Stability : Alpha
-- Portability : portable
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
  , sendCmd
  , addEvent
  , changeEvents
  , defaultConfig
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
import Network.SimpleIRC.Utils
import Data.Unique
import System.IO.Error
import Data.Time
import System.Locale
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map

internalEvents     = [joinChans, pong, onJoin]
internalNormEvents = [Privmsg ctcpHandler]

-- |Connects to a server
connect :: IrcConfig       -- ^ Configuration
           -> Bool         -- ^ Run in a new thread
           -> Bool         -- ^ Print debug messages
           -> IO (Either IOError IrcServer) -- ^ IrcServer instance
connect config threaded debug = try $ do
  if debug
    then B.putStrLn $ "Connecting to " `B.append` (B.pack $ cAddr config)
    else return ()
  h <- connectTo (cAddr config) (PortNumber $ fromIntegral $ cPort config)
  hSetBuffering h NoBuffering
  
  cmdChan <- newChan
  
  server <- toServer config h cmdChan debug
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
  write server $ "QUIT :" `B.append` quitMsg
  return ()
  where h = fromJust $ sSock server

genUnique :: IrcEvent -> IO (Unique, IrcEvent)
genUnique e = do
  u <- newUnique
  return (u, e)

genUniqueMap :: [IrcEvent] -> IO (Map.Map Unique IrcEvent)
genUniqueMap events = do
  uEvents <- mapM genUnique events
  return $ Map.fromList uEvents

toServer :: IrcConfig -> Handle -> Chan SIrcCommand -> Bool -> IO IrcServer
toServer config h cmdChan debug = do
  uniqueEvents <- genUniqueMap $ internalNormEvents ++ (cEvents config)

  return $ IrcServer (B.pack $ cAddr config) (cPort config)
              (B.pack $ cNick config) (B.pack $ cUsername config) 
              (B.pack $ cRealname config) (map B.pack $ cChannels config) 
              uniqueEvents (Just h) Nothing cmdChan debug
              (cCTCPVersion config) (cCTCPTime config)

greetServer :: IrcServer -> IO IrcServer
greetServer server = do
  write server $ "NICK " `B.append` nick
  write server $ "USER " `B.append` user `B.append` " " `B.append`
      user `B.append` " " `B.append` addr `B.append` " :" `B.append` real
  
  return server
  where nick = sNickname server
        user = sUsername server
        real = sRealname server
        addr = sAddr server

-- TODO: I think this should execute all commands that are available.
execCmds :: IrcServer -> IO IrcServer
execCmds server = do
  empty <- isEmptyChan $ sCmdChan server
  if not $ empty 
    then do cmd <- readChan $ sCmdChan server
            case cmd of (SIrcAddEvent uEvent) -> return server {sEvents = Map.insert (fst uEvent) (snd uEvent) (sEvents server)}
                        (SIrcChangeEvents events) -> return server {sEvents = events}
                        (SIrcRemoveEvent key) -> return server {sEvents = Map.delete key (sEvents server)}
    else return server

listenLoop :: IrcServer -> IO ()
listenLoop s = do
  server <- execCmds s

  let h = fromJust $ sSock server
  eof <- hIsEOF h
  -- If EOF then we are disconnected
  if eof 
    then do
      let comp   = (\a -> a `eqEvent` (Disconnect undefined))
          events = Map.filter comp (sEvents server)
          eventCall = (\obj -> (eventFuncD $ snd obj) server)
      debugWrite s $ B.pack $ show $ length $ Map.toList events
      mapM eventCall (Map.toList events)
      return ()
    else do
      line <- B.hGetLine h
      debugWrite s $ (B.pack ">> ") `B.append` line
      
      newServ <- foldM (\s f -> f s (parse line)) server internalEvents
      
      callEvents newServ (parse line)

      listenLoop newServ
    
-- Internal Events - They can edit the server
joinChans :: IrcServer -> IrcMessage -> IO IrcServer
joinChans server msg = do
  if code == "001"
    then do mapM (\chan -> write server $ "JOIN " `B.append` chan) (sChannels server)
            return server {sChannels = []}
    else return server
  where h    = fromJust $ sSock server
        code = mCode msg

pong :: IrcServer -> IrcMessage -> IO IrcServer
pong server msg = do
  if code == "PING"
    then do
      write server $ "PONG :" `B.append` pingMsg
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

-- Internal normal events
ctcpHandler :: EventFunc
ctcpHandler server iMsg
  | msg == "\x01VERSION\x01" =
    sendCmd server
      (MNotice chan ("\x01VERSION " `B.append`
        (B.pack $ sCTCPVersion server) `B.append` "\x01"))
  | msg == "\x01TIME\x01" = do
    time <- sCTCPTime server
    sendCmd server
      (MNotice chan ("\x01TIME " `B.append`
        (B.pack time) `B.append` "\x01"))
  | otherwise = return ()
  where msg  = mMsg iMsg
        chan = getChan server iMsg
-- Event code
events :: IrcServer -> IrcEvent -> IrcMessage -> IO ()
events server event msg = do
  mapM eventCall (Map.toList events)
  return ()
  where comp   = (\a -> a `eqEvent` event)
        events = Map.filter comp (sEvents server)
        eventCall = (\obj -> (eventFunc $ snd obj) server msg)

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

  | mCode msg == "NOTICE"      = do
    events server (Notice undefined) msg

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
(Notice  _) `eqEvent` (Notice  _) = True
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
eventFunc (Notice  f) = f
eventFunc (RawMsg  f) = f

eventFuncD (Disconnect  f) = f

-- |Sends a raw command to the server
sendRaw :: IrcServer -> B.ByteString -> IO ()
sendRaw server msg = write server msg

-- |Sends a message to a channel
-- |
-- |Please note: As of now this function doesn't provide flood control.
-- |So be careful.
sendMsg :: IrcServer 
           -> B.ByteString -- ^ Channel
           -> B.ByteString -- ^ Message
           -> IO ()
sendMsg server chan msg = do
  mapM (s) lins
  return ()
  where lins = B.lines msg
        s m = sendCmd server (MPrivmsg chan m)
sendCmd :: IrcServer
           -> Command -- Command to send
           -> IO ()
sendCmd server cmd =
  sendRaw server (showCommand cmd)

addEvent :: IrcServer -> IrcEvent -> IO Unique
addEvent s event = do
  u <- newUnique
  writeChan (sCmdChan s) (SIrcAddEvent (u, event))
  return u

changeEvents :: IrcServer -> [IrcEvent] -> IO ()
changeEvents s events = do
  uniqueEvents <- genUniqueMap events
  writeChan (sCmdChan s) (SIrcChangeEvents uniqueEvents)

remEvent :: IrcServer -> Unique -> IO ()
remEvent s uniq = do
  writeChan (sCmdChan s) (SIrcRemoveEvent uniq)

debugWrite :: IrcServer -> B.ByteString -> IO ()
debugWrite s msg = do
  if sDebug s
    then B.putStrLn msg
    else return ()

write :: IrcServer -> B.ByteString -> IO ()
write s msg = do
  debugWrite s $ "<< " `B.append` msg `B.append` "\\r\\n"
  B.hPutStr h (msg `B.append` "\r\n")
  where h = fromJust $ sSock s
  

defaultConfig = IrcConfig
  { cPort     = 6667
  , cUsername = "simpleirc"
  , cRealname = "SimpleIRC Bot"
  , cChannels = []
  , cEvents   = []
  , cCTCPVersion = "SimpleIRC v0.1"
  , cCTCPTime    =  getZonedTime >>= 
    (\t -> return $ formatTime defaultTimeLocale "%c" t)
  }
