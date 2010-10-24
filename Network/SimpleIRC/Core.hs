-- |
-- Module : Network.SimpleIRC.Core
-- Copyright : (c) Dominik Picheta 2010
-- License : BSD3
--
-- Maintainer : morfeusz8@gmail.com
-- Stability : provisional
-- Portability : portable
--
-- For information on how to use this library please take a look at the readme file on github, <http://github.com/dom96/SimpleIRC#readme>.
{-# LANGUAGE OverloadedStrings #-}
module Network.SimpleIRC.Core
  ( 
    -- * Types
    MIrc 
  , EventFunc
  , IrcConfig(..)
  , IrcEvent(..)
    
    -- * Functions
  , connect
  , disconnect
--  , reconnect
  , sendRaw
  , sendMsg
  , sendCmd
  , addEvent
  , changeEvents
  , remEvent
  , defaultConfig
  
   -- * Utils
  , getDest
  
   -- * Accessors
  , getChannels
  , getNickname
  , getAddress
  , getPort
  , getUsername
  , getRealname
  ) where
  
import Network
import System.IO
import Data.Maybe
import Data.List (delete)
import Data.Char (isNumber)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Network.SimpleIRC.Messages
import Data.Unique
import System.IO.Error
import Data.Time
import System.Locale
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map

internalEvents     = [joinChans, pong, trackChanges]
internalNormEvents = [Privmsg ctcpHandler]

type MIrc = MVar IrcServer

data IrcConfig = IrcConfig
  { cAddr     :: String   -- ^ Server address to connect to
  , cPort     :: Int      -- ^ Server port to connect to
  , cNick     :: String   -- ^ Nickname
  , cUsername :: String   -- ^ Username
  , cRealname :: String   -- ^ Realname
  , cChannels :: [String]   -- ^ List of channels to join on connect
  , cEvents   :: [IrcEvent] -- ^ Events to bind
  , cCTCPVersion :: String  -- ^ What to send on CTCP VERSION
  , cCTCPTime    :: IO String  -- ^ What to send on CTCP TIME
  }

data SIrcCommand =
    SIrcAddEvent (Unique, IrcEvent)
  | SIrcChangeEvents (Map.Map Unique IrcEvent)
  | SIrcRemoveEvent Unique

data IrcServer = IrcServer
  { sAddr         :: B.ByteString
  , sPort         :: Int
  , sNickname     :: B.ByteString
  , sUsername     :: B.ByteString
  , sRealname     :: B.ByteString
  , sChannels     :: [B.ByteString]
  , sEvents       :: Map.Map Unique IrcEvent
  , sSock         :: Maybe Handle
  , sListenThread :: Maybe ThreadId
  , sCmdThread    :: Maybe ThreadId
  , sCmdChan      :: Chan SIrcCommand
  , sDebug        :: Bool
  -- Other info
  , sCTCPVersion  :: String
  , sCTCPTime     :: IO String
  }

-- When adding events here, remember add them in callEvents and in eventFunc
-- AND also in the Show instance and Eq instance

data IrcEvent = 
    Privmsg EventFunc -- ^ PRIVMSG
  | Numeric EventFunc -- ^ Numeric, 001, 002, 372 etc.
  | Ping EventFunc    -- ^ PING
  | Join EventFunc    -- ^ JOIN
  | Part EventFunc    -- ^ PART
  | Mode EventFunc    -- ^ MODE
  | Topic EventFunc   -- ^ TOPIC
  | Invite EventFunc  -- ^ INVITE
  | Kick EventFunc    -- ^ KICK
  | Quit EventFunc    -- ^ QUIT
  | Nick EventFunc    -- ^ NICK
  | Notice EventFunc  -- ^ NOTICE
  | RawMsg EventFunc  -- ^ This event gets called on every message received
  | Disconnect (MIrc -> IO ()) -- ^ This event gets called whenever the
                                    --   connection with the server is dropped
  
instance Show IrcEvent where
  show (Privmsg _) = "IrcEvent - Privmsg"
  show (Numeric _) = "IrcEvent - Numeric"
  show (Ping    _) = "IrcEvent - Ping"
  show (Join    _) = "IrcEvent - Join"
  show (Part    _) = "IrcEvent - Part"
  show (Mode    _) = "IrcEvent - Mode"
  show (Topic   _) = "IrcEvent - Topic"
  show (Invite  _) = "IrcEvent - Invite"
  show (Kick    _) = "IrcEvent - Kick"
  show (Quit    _) = "IrcEvent - Quit"
  show (Nick    _) = "IrcEvent - Nick"
  show (Notice  _) = "IrcEvent - Notice"
  show (RawMsg  _) = "IrcEvent - RawMsg"
  show (Disconnect  _) = "IrcEvent - Disconnect"

type EventFunc = (MIrc -> IrcMessage -> IO ())

-- |Connects to a server
connect :: IrcConfig       -- ^ Configuration
           -> Bool         -- ^ Run in a new thread
           -> Bool         -- ^ Print debug messages
           -> IO (Either IOError MIrc) -- ^ IrcServer instance
connect config threaded debug = try $ do
  (when debug $
    B.putStrLn $ "Connecting to " `B.append` B.pack (cAddr config))
  
  h <- connectTo (cAddr config) (PortNumber $ fromIntegral $ cPort config)
  hSetBuffering h NoBuffering
  
  cmdChan <- newChan
  
  server <- toServer config h cmdChan debug
  -- Initialize connection with the server
  greetServer server
  
  -- Create a new MVar
  res <- newMVar server
  
  -- Start the loops, listen and exec cmds
  if threaded
    then do listenId <- forkIO (listenLoop res)
            cmdId <- forkIO (execCmdsLoop res)
            modifyMVar_ res (\srv -> return $ srv {sListenThread = Just listenId}) 
            return res
    else do listenLoop res
            return res

-- |Sends a QUIT command to the server.
disconnect :: MIrc
              -> B.ByteString -- ^ Quit message
              -> IO ()
disconnect server quitMsg = do
  s <- readMVar server
  
  let h = fromJust $ sSock s
  write s $ "QUIT :" `B.append` quitMsg
  return ()

-- TODO: Reconnect 
{-
-- |Reconnects to the server.
reconnect :: MIrc -> IO (Either IOError MIrc)
reconnect server = do
  s <- readMVar server
  
  let conf = IrcConfig (B.unpack $ sAddr s) (sPort s) 
                       (B.unpack $ sNickname s) (B.unpack $ sUsername s) 
                       (B.unpack $ sRealname s) (map (B.unpack) (sChannels s)) 
                       (elems $ sEvents s) (sCTCPVersion s) (sCTCPTime s)
  connect conf True (sDebug s)
-}

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
  uniqueEvents <- genUniqueMap $ internalNormEvents ++ cEvents config

  return $ IrcServer (B.pack $ cAddr config) (cPort config)
              (B.pack $ cNick config) (B.pack $ cUsername config) 
              (B.pack $ cRealname config) (map B.pack $ cChannels config) 
              uniqueEvents (Just h) Nothing Nothing cmdChan debug
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

execCmdsLoop :: MIrc -> IO ()
execCmdsLoop mIrc = do
  server <- readMVar mIrc
  cmd <- readChan $ sCmdChan server
  case cmd of (SIrcAddEvent uEvent)     -> do
                swapMVar mIrc (server {sEvents = 
                  (uncurry Map.insert uEvent) (sEvents server)}) 
                execCmdsLoop mIrc
              (SIrcChangeEvents events) -> do
                swapMVar mIrc (server {sEvents = events}) 
                execCmdsLoop mIrc
              (SIrcRemoveEvent key)     -> do
                swapMVar mIrc (server {sEvents = 
                  Map.delete key (sEvents server)})
                execCmdsLoop mIrc


listenLoop :: MIrc -> IO ()
listenLoop s = do
  server <- readMVar s

  let h = fromJust $ sSock server
  eof <- hIsEOF h
  
  -- If EOF then we are disconnected
  if eof 
    then do
      let comp   = (\a -> a `eqEvent` (Disconnect undefined))
          events = Map.filter comp (sEvents server)
          eventCall = (\obj -> (eventFuncD $ snd obj) s)
      debugWrite server $ B.pack $ show $ length $ Map.toList events
      mapM_ eventCall (Map.toList events)
    else do
      line <- B.hGetLine h
      
      server1 <- takeMVar s
      
      -- Print the received line.
      debugWrite server1 $ (B.pack ">> ") `B.append` line
      
      -- Call the internal events
      newServ <- foldM (\sr f -> f sr (parse line)) server1 internalEvents
      
      putMVar s newServ -- Put the MVar back.
      
      let parsed = (parse line)
      -- Call the events
      callEvents s parsed

      -- Call the RawMsg Events.
      events s (RawMsg undefined) parsed
      

      listenLoop s
    
-- Internal Events - They can edit the server
joinChans :: IrcServer -> IrcMessage -> IO IrcServer
joinChans server msg =
  if code == "001"
    then do mapM_ (\chan -> write server $ "JOIN " `B.append` chan) (sChannels server)
            return server {sChannels = []}
    else return server
  where h    = fromJust $ sSock server
        code = mCode msg

pong :: IrcServer -> IrcMessage -> IO IrcServer
pong server msg =
  if code == "PING"
    then do
      write server $ "PONG :" `B.append` pingMsg
      return server
    else return server
    
  where h       = fromJust $ sSock server
        pingMsg = mMsg msg
        code    = mCode msg

trackChanges :: IrcServer -> IrcMessage -> IO IrcServer
trackChanges server msg
  | code == "JOIN" = do
    let nick = fromJust $ mNick msg
        chan  = mMsg msg
    if nick == sNickname server
      then return server { sChannels = chan:(sChannels server) }
      else return server
  | code == "NICK" = do
    let nick    = fromJust $ mNick msg
        newNick = mMsg msg
    if nick == sNickname server
      then return server { sNickname = newNick }
      else return server 
  | code == "KICK" = do 
    let nick = (fromJust $ mOther msg) !! 0
        chan = fromJust $ mChan msg
    if nick == sNickname server
      then return server { sChannels = delete chan (sChannels server) }
      else return server
  | code == "PART" = do 
    let nick = fromJust $ mNick msg
        chan = mMsg msg
    if nick == sNickname server
      then return server { sChannels = delete chan (sChannels server) }
      else return server
  | otherwise = return server
  
  where code = mCode msg

-- Internal normal events
ctcpHandler :: EventFunc
ctcpHandler mServ iMsg
  | msg == "\x01VERSION\x01" = do
    server <- readMVar mServ
    
    chan <- getDest mServ iMsg
    sendCmd mServ
      (MNotice chan ("\x01VERSION " `B.append`
        B.pack (sCTCPVersion server) `B.append` "\x01"))
  | msg == "\x01TIME\x01" = do
    server <- readMVar mServ
    
    time <- sCTCPTime server
    chan <- getDest mServ iMsg
    sendCmd mServ
      (MNotice chan ("\x01TIME " `B.append`
        (B.pack time) `B.append` "\x01"))
  | "\x01PING " `B.isPrefixOf` msg = do
    server <- readMVar mServ
    
    chan <- getDest mServ iMsg
    sendCmd mServ
      (MNotice chan msg)

  | otherwise = return ()
  where msg = mMsg iMsg

-- Event code
events :: MIrc -> IrcEvent -> IrcMessage -> IO ()
events mServ event msg = do
  server <- readMVar mServ
  let comp   = (`eqEvent` event)
      events = Map.filter comp (sEvents server)
      eventCall = (\obj -> (eventFunc $ snd obj) mServ msg)

  mapM_ eventCall (Map.toList events)


callEvents :: MIrc -> IrcMessage -> IO ()
callEvents mServ msg
  | mCode msg == "PRIVMSG"     =
    events mServ (Privmsg undefined) msg
    
  | mCode msg == "PING"        =
    events mServ (Ping undefined) msg

  | mCode msg == "JOIN"        =
    events mServ (Join undefined) msg
  
  | mCode msg == "PART"        =
    events mServ (Part undefined) msg

  | mCode msg == "MODE"        =
    events mServ (Mode undefined) msg

  | mCode msg == "TOPIC"       =
    events mServ (Topic undefined) msg

  | mCode msg == "INVITE"      =
    events mServ (Invite undefined) msg

  | mCode msg == "KICK"        =
    events mServ (Kick undefined) msg

  | mCode msg == "QUIT"        =
    events mServ (Quit undefined) msg

  | mCode msg == "NICK"        =
    events mServ (Nick undefined) msg

  | mCode msg == "NOTICE"      =
    events mServ (Notice undefined) msg

  | B.all isNumber (mCode msg) =
    events mServ (Numeric undefined) msg
  
  | otherwise                = return ()
    

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
sendRaw :: MIrc -> B.ByteString -> IO ()
sendRaw mServ msg = do
  server <- readMVar mServ
  write server msg

-- |Sends a message to a channel

-- |Please note: As of now this function doesn't provide flood control.
-- So be careful with \\n.
sendMsg :: MIrc 
           -> B.ByteString -- ^ Channel
           -> B.ByteString -- ^ Message
           -> IO ()
sendMsg mServ chan msg =
  mapM_ s lins
  where lins = B.lines msg
        s m = sendCmd mServ (MPrivmsg chan m)
        
        
sendCmd :: MIrc
           -> Command -- Command to send
           -> IO ()
sendCmd mServ cmd = sendRaw mServ (showCommand cmd)

addEvent :: MIrc -> IrcEvent -> IO Unique
addEvent mIrc event = do
  s <- readMVar mIrc

  u <- newUnique
  writeChan (sCmdChan s) (SIrcAddEvent (u, event))
  return u


changeEvents :: MIrc -> [IrcEvent] -> IO ()
changeEvents mIrc events = do
  s <- readMVar mIrc

  uniqueEvents <- genUniqueMap events
  writeChan (sCmdChan s) (SIrcChangeEvents uniqueEvents)

remEvent :: MIrc -> Unique -> IO ()
remEvent mIrc uniq = do
  s <- readMVar mIrc

  writeChan (sCmdChan s) (SIrcRemoveEvent uniq)

debugWrite :: IrcServer -> B.ByteString -> IO ()
debugWrite s msg =
  (when (sDebug s) $ B.putStrLn msg)

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
  , cCTCPVersion = "SimpleIRC v0.2"
  , cCTCPTime    = fmap (formatTime defaultTimeLocale "%c") getZonedTime
  }
  
-- Utils

-- |Gets the destination, i.e if the IrcMessage was sent
--  directly to you returns the senders nick otherwise the channel.
getDest :: MIrc -> IrcMessage -> IO B.ByteString
getDest mIrc m = do
  s <- readMVar mIrc
  
  if sNickname s == chan
    then return (fromJust $ mNick m)
    else return chan
  
  where chan = fromJust $ mChan m
  
-- MIrc Accessors
-- |Returns a list of channels currently joined.
getChannels :: MIrc -> IO [B.ByteString]
getChannels mIrc = do
  s <- readMVar mIrc
  
  return $ sChannels s

-- |Returns the current nickname.
getNickname :: MIrc -> IO B.ByteString
getNickname mIrc = do
  s <- readMVar mIrc
  
  return $ sNickname s
  
-- |Returns the address
getAddress :: MIrc -> IO B.ByteString
getAddress mIrc = do
  s <- readMVar mIrc
  
  return $ sAddr s  

-- |Returns the address
getPort :: MIrc -> IO Int
getPort mIrc = do
  s <- readMVar mIrc
  
  return $ sPort s

-- |Returns the User name
getUsername :: MIrc -> IO B.ByteString
getUsername mIrc = do
  s <- readMVar mIrc
  
  return $ sUsername s  

-- |Returns the Real name
getRealname :: MIrc -> IO B.ByteString
getRealname mIrc = do
  s <- readMVar mIrc
  
  return $ sRealname s  

