module Network.SimpleIRC.Types
  ( 
    -- * Datatypes
    IrcConfig(..)
  , IrcServer(..)
  , IrcEvent(..)
  , EventFunc
  , IrcMessage(..)
  ) where
import qualified Data.ByteString.Char8 as B
import Control.Concurrent (ThreadId)
import System.IO (Handle)

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
  | Join EventFunc    -- ^ JOIN
  | Part EventFunc    -- ^ PART
  | Mode EventFunc    -- ^ MODE
  | Topic EventFunc   -- ^ TOPIC
  | Invite EventFunc  -- ^ INVITE
  | Kick EventFunc    -- ^ KICK
  | Quit EventFunc    -- ^ QUIT
  | Nick EventFunc    -- ^ NICK
  | RawMsg EventFunc  -- ^ This event gets called on every message received
  | Disconnect (IrcServer -> IO ())
  
  
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
  show (RawMsg  _) = "IrcEvent - RawMsg"
  show (Disconnect  _) = "IrcEvent - Disconnect"
  
data IrcMessage = IrcMessage
  { mNick   :: Maybe B.ByteString
  , mHost   :: Maybe B.ByteString
  , mServer :: Maybe B.ByteString
  , mCode   :: Maybe B.ByteString
  , mMsg    :: Maybe B.ByteString
  , mChan   :: Maybe B.ByteString
  , mOther  :: Maybe [B.ByteString]
  , mRaw    :: B.ByteString
  } deriving Show
