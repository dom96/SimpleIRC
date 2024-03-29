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
{-# LANGUAGE OverloadedStrings, CPP, PatternGuards #-}
module Network.SimpleIRC.Core
  (
    -- * Types
    MIrc
  , EventFunc
  , IrcConfig(..)
  , IrcEvent(..)
  , SaslMechanism(..)

    -- * Functions
  , connect
  , disconnect
  , reconnect
  , sendRaw
  , sendMsg
  , sendCmd
  , addEvent
  , changeEvents
  , remEvent
  , mkDefaultConfig

   -- * Accessors
  , getChannels
  , getNickname
  , getAddress
  , getPort
  , getUsername
  , getRealname
#ifdef TEST
  , IrcServer(..)
  , listenLoop
#endif
  ) where

import Network.Connection
import Network.Socket (HostName, PortNumber)

import Data.Maybe
import Data.List (delete)
import Data.Char (isNumber)
import Control.Monad
import Control.Concurrent
import Network.SimpleIRC.Messages
import Network.SimpleIRC.Sasl
import Data.Unique
import Control.Exception (try, SomeException)
import System.Timeout
import Data.Time
#if ! MIN_VERSION_time(1,5,0)
import System.Locale
#endif
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable

internalEvents :: [IrcServer -> IrcMessage -> IO IrcServer]
internalEvents     = [joinChans, pong, trackChanges, handleSasl]
internalNormEvents :: [IrcEvent]
internalNormEvents = [Privmsg ctcpHandler]

type MIrc = MVar IrcServer

data IrcConfig = IrcConfig
  { cAddr     :: String   -- ^ Server address to connect to
  , cPort     :: Int      -- ^ Server port to connect to
  , cSecure   :: Bool     -- ^ Use secure transport
  , cNick     :: String   -- ^ Nickname
  , cPass     :: Maybe String -- ^ Optional server password
  , cSasl     :: Maybe SaslMechanism -- ^ sasl
  , cUsername :: String   -- ^ Username
  , cRealname :: String   -- ^ Realname
  , cChannels :: [String]   -- ^ List of channels to join on connect
  , cEvents   :: [IrcEvent] -- ^ Events to bind
  , cCTCPVersion :: String  -- ^ What to send on CTCP VERSION
  , cCTCPTime    :: IO String  -- ^ What to send on CTCP TIME
  , cPingTimeoutInterval :: Int -- The time between server messages that causes ping timeout
  }

data SIrcCommand =
    SIrcAddEvent (Unique, IrcEvent)
  | SIrcChangeEvents (Map.Map Unique IrcEvent)
  | SIrcRemoveEvent Unique

data IrcServer = IrcServer
  { sAddr         :: B.ByteString
  , sPort         :: Int
  , sSecure       :: Bool
  , sNickname     :: B.ByteString
  , sPassword     :: Maybe B.ByteString
  , sSasl         :: Maybe SaslState
  , sUsername     :: B.ByteString
  , sRealname     :: B.ByteString
  , sChannels     :: [B.ByteString]
  , sEvents       :: Map.Map Unique IrcEvent
  , sSock         :: Maybe Connection
  , sListenThread :: Maybe ThreadId
  , sCmdThread    :: Maybe ThreadId
  , sCmdChan      :: Chan SIrcCommand
  , sDebug        :: Bool
  -- Other info
  , sCTCPVersion  :: String
  , sCTCPTime     :: IO String
  , sPingTimeoutInterval :: Int
  , sFloodControlTimestamp :: UTCTime
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

data SaslState =
    -- | Should always move from NotStarted to Running, never backwards
    SaslNotStarted SaslMechanism
    -- | Because messages are sent in multiple chunks, the bytestring is
    -- the message (base-64) seen "so far", before the Await can be run
  | SaslRunning B.ByteString SaslAwait

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

connect' :: HostName -> PortNumber -> Bool -> IO Connection
connect' host port secure = do
  ctx <- initConnectionContext
  conn <- connectTo ctx $ ConnectionParams host port (tlsSettings secure) Nothing
  return conn
  where
    tlsSettings True = Just $ TLSSettingsSimple True True False
    tlsSettings False = Nothing

-- |Connects to a server
connect :: IrcConfig       -- ^ Configuration
           -> Bool         -- ^ Run in a new thread
           -> Bool         -- ^ Print debug messages
           -> IO (Either IOError MIrc) -- ^ IrcServer instance
connect config threaded debug = try $ do
  (when debug $
    B.putStrLn $ "Connecting to " `B.append` B.pack (cAddr config))

  conn <- connect' (cAddr config) (fromIntegral $ cPort config) (cSecure config)

  cmdChan <- newChan

  server <- toServer config conn cmdChan debug
  -- Initialize connection with the server
  _ <- greetServer server

  -- Create a new MVar
  res <- newMVar server

  -- Start the loops, listen and exec cmds
  if threaded
    then do listenId <- forkIO (listenLoop res)
            _ <- forkIO (execCmdsLoop res)
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

  write s $ "QUIT :" `B.append` quitMsg
  connectionClose (fromJust $ sSock s)

-- |Reconnects to the server.
reconnect :: MIrc -> IO (Either IOError MIrc)
reconnect mIrc = try $ do
  server <- readMVar mIrc

  conn <- connect' (B.unpack $ sAddr server) (fromIntegral $ sPort server) (sSecure server)

  modifyMVar_ mIrc (\s -> return $ s {sSock = Just conn})

  -- Initialize connection with the server
  _ <- withMVar mIrc greetServer

  -- Restart the listen loop.
  listenId <- forkIO (listenLoop mIrc)
  cmdId <- forkIO (execCmdsLoop mIrc)
  modifyMVar_ mIrc (\s -> return $ s {sListenThread = Just listenId,
                        sCmdThread = Just cmdId})
  return mIrc

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
genUniqueMap evts = do
  uEvents <- mapM genUnique evts
  return $ Map.fromList uEvents

toServer :: IrcConfig -> Connection -> Chan SIrcCommand -> Bool -> IO IrcServer
toServer config conn cmdChan debug = do
  uniqueEvents <- genUniqueMap $ internalNormEvents ++ cEvents config
  now <- getCurrentTime

  return $ IrcServer (B.pack $ cAddr config) (cPort config) (cSecure config)
              (B.pack $ cNick config) (B.pack `fmap` cPass config)
              (SaslNotStarted `fmap` cSasl config) (B.pack $ cUsername config)
              (B.pack $ cRealname config) (map B.pack $ cChannels config)
              uniqueEvents (Just conn) Nothing Nothing cmdChan debug
              (cCTCPVersion config) (cCTCPTime config)
              (cPingTimeoutInterval config) now

greetServer :: IrcServer -> IO IrcServer
greetServer server = do
  when (isJust sasl) $ write server "CAP REQ :sasl"
  case mpass of
    Nothing -> return ()
    Just pass -> write server $ "PASS " `B.append` pass
  write server $ "NICK " `B.append` nick
  write server $ "USER " `B.append` user `B.append` " " `B.append`
      user `B.append` " " `B.append` addr `B.append` " :" `B.append` real

  return server
  where nick = sNickname server
        mpass = sPassword server
        user = sUsername server
        real = sRealname server
        addr = sAddr server
        sasl = sSasl server

execCmdsLoop :: MIrc -> IO ()
execCmdsLoop mIrc = do
  server <- readMVar mIrc
  cmd <- readChan $ sCmdChan server
  case cmd of (SIrcAddEvent uEvent)     -> do
                _ <- swapMVar mIrc (server {sEvents =
                  (uncurry Map.insert uEvent) (sEvents server)})
                execCmdsLoop mIrc
              (SIrcChangeEvents evts) -> do
                _ <- swapMVar mIrc (server {sEvents = evts})
                execCmdsLoop mIrc
              (SIrcRemoveEvent key)     -> do
                _ <- swapMVar mIrc (server {sEvents =
                  Map.delete key (sEvents server)})
                execCmdsLoop mIrc


listenLoop :: MIrc -> IO ()
listenLoop s = do
  server <- readMVar s

  let c = fromJust $ sSock server

  -- RFC 2812, max message line length
  lineOrCleanup <- timeout (sPingTimeoutInterval server) (try $ connectionGetLine 512 c :: IO (Either SomeException B.ByteString))

  case lineOrCleanup of
    Nothing -> do
      debugWrite server $ "Timeout reached"
      cleanup server
    Just (Left ex) -> do
      debugWrite server $ B.pack $ "Exception caught: " ++ show ex
      cleanup server

    Just (Right line) -> do
      server1 <- takeMVar s

      -- Print the received line.
      debugWrite server1 $ (B.pack ">> ") `B.append` line
      let parsed = parse line

      -- Call the internal events
      newServ <- foldM (\sr f -> f sr parsed) server1 internalEvents

      putMVar s newServ -- Put the MVar back.

      -- Call the events
      callEvents s parsed

      -- Call the RawMsg Events.
      events s (RawMsg undefined) parsed


      listenLoop s
  where
    cleanup server = do
      modifyMVar_ s (\serv -> return $ serv {sSock = Nothing})
      Foldable.mapM_ (callDisconnectFunction s) (sEvents server)
    callDisconnectFunction mIrc (Disconnect f) = f mIrc
    callDisconnectFunction _ _ = return ()

-- Internal Events - They can edit the server
joinChans :: IrcServer -> IrcMessage -> IO IrcServer
joinChans server msg =
  if code == "001"
    then do mapM_ (\chan -> write server $ "JOIN " `B.append` chan) (sChannels server)
            return server {sChannels = []}
    else return server
  where code = mCode msg

pong :: IrcServer -> IrcMessage -> IO IrcServer
pong server msg =
  if code == "PING"
    then do
      write server $ "PONG :" `B.append` pingMsg
      return server
    else return server

  where pingMsg = mMsg msg
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

handleSasl :: IrcServer -> IrcMessage -> IO IrcServer
handleSasl s0 msg = answerSasl =<< checkSasl =<< beginSasl s0
  where
    code = mCode msg
    other = mOther msg
    contents = mMsg msg
    runSend server s = case s of
      SaslSend mg nxt -> do
        mapM_ (mapM_ (write server . ("AUTHENTICATE " `B.append`)) . encodeAuthMsg) mg
        return server { sSasl = Just (SaslRunning B.empty nxt) }
    beginSasl server = case (sasl, code, other) of
        (Just (SaslNotStarted (SaslMechanism nm mc)), "CAP", Just ("ACK":_)) -> do
          write server $ "AUTHENTICATE " `B.append` B.pack nm
          return server { sSasl = Just (SaslRunning B.empty mc) }
        _ -> return server
      where sasl = sSasl server
    checkSasl server = case sasl of
        Just _
          | code == "903" || code == "907" || code == "906" -> do
              write server "CAP END"
              return server { sSasl = Nothing }
          | code == "904" -> do
              debugWrite server "SASL Authentication Failed"
              return server
        _ -> return server
      where sasl = sSasl server
    answerSasl server = case sasl of
        Just (SaslRunning soFar (SaslAwaitResp f))
          | code == "AUTHENTICATE" ->
              case decodeAuthMsg soFar contents of
                Left waitMore -> return server
                  { sSasl = Just (SaslRunning waitMore (SaslAwaitResp f)) }
                Right Nothing -> do
                  write server $ "AUTHENTICATE *"
                  debugWrite server
                    "SASL Authentication Aborted: AUTHENTICATE command over 400 bytes received"
                  return server
                Right (Just fullMsg) -> case f fullMsg of
                  Left e  -> do
                    write server $ "AUTHENTICATE *"
                    debugWrite server $ "SASL Authentication Aborted: " <> B.pack e
                    return server
                  Right x -> runSend server x
          | otherwise -> return server
        -- this is handled by checkSasl
        Just (SaslRunning _ SaslAwaitDone) -> return server
        _ -> return server
      where sasl      = sSasl server

-- Internal normal events
ctcpHandler :: EventFunc
ctcpHandler mServ iMsg
  | msg == "\x01VERSION\x01" = do
    server <- readMVar mServ

    sendCmd mServ
      (MNotice origin ("\x01VERSION " `B.append`
        B.pack (sCTCPVersion server) `B.append` "\x01"))

  | msg == "\x01TIME\x01" = do
    server <- readMVar mServ

    time <- sCTCPTime server
    sendCmd mServ
      (MNotice origin ("\x01TIME " `B.append`
        (B.pack time) `B.append` "\x01"))
  | "\x01PING " `B.isPrefixOf` msg = do

    sendCmd mServ
      (MNotice origin msg)

  | otherwise = return ()
  where msg    = mMsg iMsg
        origin = fromJust $ mOrigin iMsg
-- Event code
events :: MIrc -> IrcEvent -> IrcMessage -> IO ()
events mServ event msg = do
  server <- readMVar mServ
  let comp   = (`eqEvent` event)
      evts = Map.filter comp (sEvents server)
      eventCall = (\obj -> (eventFunc $ snd obj) mServ msg)

  mapM_ eventCall (Map.toList evts)


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


eqEvent :: IrcEvent -> IrcEvent -> Bool
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
eventFunc (Disconnect _) = error "SimpleIRC: unexpected event"

-- |Sends a raw command to the server
sendRaw :: MIrc -> B.ByteString -> IO ()
sendRaw mServ msg = do
  server <- readMVar mServ
  write server msg

-- |Sends a message to a channel

-- |Implements flood control according to RFC 2813, chapter 5.8
sendMsg :: MIrc
           -> B.ByteString -- ^ Channel
           -> B.ByteString -- ^ Message
           -> IO ()
sendMsg mServ chan msg =
  mapM_ s lins
  where lins = B.lines msg
        s m = do
          now <- getCurrentTime
          stamp <- (getFloodControlTimestamp mServ)
          let latest = addUTCTime 2 $ max now stamp
              diff = diffUTCTime latest now
          setFloodControlTimestamp mServ latest
          when (diff > 10) (threadDelay $ 1000000 * (round diff - 10))
          sendCmd mServ (MPrivmsg chan m)


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
changeEvents mIrc evts = do
  s <- readMVar mIrc

  uniqueEvents <- genUniqueMap evts
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
  connectionPut conn msg'
  where
    conn = fromJust $ sSock s
    msg' = msg `B.append` "\r\n"

mkDefaultConfig :: String -> String -> IrcConfig
mkDefaultConfig addr nick = IrcConfig
  { cAddr     = addr
  , cPort     = 6667
  , cSecure   = False
  , cNick     = nick
  , cPass     = Nothing
  , cSasl     = Nothing
  , cUsername = "simpleirc"
  , cRealname = "SimpleIRC Bot"
  , cChannels = []
  , cEvents   = []
  , cCTCPVersion = "SimpleIRC v0.3"
  , cCTCPTime    = fmap (formatTime defaultTimeLocale "%c") getZonedTime
  , cPingTimeoutInterval = 350 * 10^(6::Int)
  }

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

-- |Returns the timestamp of the last sent message, possibly with flood control penalty
getFloodControlTimestamp :: MIrc -> IO UTCTime
getFloodControlTimestamp mIrc = do
  s <- readMVar mIrc

  return $ sFloodControlTimestamp s

-- |Updates the value of the flood control timestamp
setFloodControlTimestamp :: MIrc -> UTCTime -> IO ()
setFloodControlTimestamp mIrc stamp =
  modifyMVar_ mIrc (\i -> return i { sFloodControlTimestamp = stamp })

