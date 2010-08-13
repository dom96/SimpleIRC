import Network
import System.IO
import Data.Maybe
import Data.Char
import Data.IORef
import Messages

-- TODO: Add a module ... (..) where
data IrcServer = IrcServer
  { sAddr     :: String
  , sPort     :: Int
  , sNickname :: String
  , sUsername :: String
  , sRealname :: String
  , sChannels :: [String]
  , sEvents   :: [IrcEvent]
  , sSock     :: Maybe Handle
  } deriving Show

type EventFunc = (IrcServer -> IrcMessage -> IO ())

data IrcEvent = Privmsg EventFunc
  | Numeric EventFunc
  | Ping EventFunc
  
instance Show IrcEvent where
  show (Privmsg _) = "IrcEvent - Privmsg"
  show (Numeric _) = "IrcEvent - Numeric"

instance Eq IrcEvent where
  (Privmsg _) == (Privmsg _) = True
  (Numeric _) == (Numeric _) = True
  _ == _                     = False

-- let serv = IrcServer "irc.freenode.net" 6667 "haskellTestBot" "test" "test 1" ["#()", "##XAMPP"] [] Nothing

connect :: IrcServer -> IO IrcServer
connect server = do
  cServ <- cServIO
  h <- connectTo (sAddr server) (PortNumber $ fromIntegral $ sPort server)
  hSetBuffering h NoBuffering
  writeIORef cServ (server { sSock = Just h })
  
  -- Add internal events
  modifyIORef cServ (flip addEvent (Numeric joinChans))
  modifyIORef cServ (flip addEvent (Ping pong))
  modifyIORef cServ (flip addEvent (Privmsg privmsgTest))
  
  -- Initialize connection with the server
  modifyIORefIO cServ greetServer
  
  -- Start listening
  modifyIORefIO cServ listenLoop 
  
  readIORef cServ -- Return the IrcServer
  where cServIO = newIORef server -- We need this to edit the server
  
disconnect :: IrcServer -> String -> IO IrcServer
disconnect server quitMsg = do
  write h $ "QUIT :" ++ quitMsg
  return server
  where h = fromJust $ sSock server

greetServer :: IrcServer -> IO IrcServer
greetServer server = do
  write h $ "NICK " ++ nick
  write h $ "USER " ++ user ++ " " ++ user ++ " " ++ addr ++ " :" ++ real
  
  return server
  where nick = sNickname server
        user = sUsername server
        real = sRealname server
        addr = sAddr server
        h    = fromJust $ sSock server

listenLoop :: IrcServer -> IO IrcServer
listenLoop server = do
  line <- hGetLine h
  putStrLn $ ">> " ++ line
  
  callEvents server (parse line)
  
  listenLoop server
  where h = fromJust $ sSock server

-- Internal Events
joinChans :: EventFunc
joinChans server msg = do
  if code == "001"
    then do mapM (\chan -> write h $ "JOIN " ++ chan) (sChannels server)
            return ()
    else return ()
  where h    = fromJust $ sSock server
        code = (fromJust $ mCode msg)

pong :: EventFunc
pong server msg = do
  putStrLn "In pong function"
  write h $ "PONG :" ++ pingMsg
  where h       = fromJust $ sSock server
        pingMsg = fromJust $ mMsg msg
        code    = fromJust $ mCode msg

privmsgTest :: EventFunc
privmsgTest server msg = do
  putStrLn $ show $ privmsg
  putStrLn $ show $ privmsg == "|test"
  if privmsg == "|test"
    then write h $ "PRIVMSG " ++ chan ++ " :Works!"
    else return ()
  where privmsg = fromJust $ mMsg msg
        chan    = fromJust $ mChan msg
        h       = fromJust $ sSock server

-- TODO: Spread this function out.
callEvents :: IrcServer -> IrcMessage -> IO IrcServer
callEvents server msg
  | fromJust (mCode msg) == "PRIVMSG"   = do
    let eventCall = (\(Privmsg f) -> f server msg)
    let comp      = (\a -> a == (Privmsg undefined))
    let events = filter comp (sEvents server)
    mapM eventCall events
    putStrLn "Called PRIVMSG"
    return server
    
  | fromJust (mCode msg) == "PING"      = do
    let eventCall = (\(Ping f) -> f server msg)
    let comp      = (\a -> a == (Ping undefined))
    let events = filter comp (sEvents server)
    mapM eventCall events
    putStrLn "Called PING"
    return server

  | all isNumber (fromJust $ mCode msg) = do
    let eventCall = (\(Numeric f) -> f server msg)
    let comp      = (\a -> a == (Numeric undefined))
    let events = filter comp (sEvents server)
    mapM eventCall events
    return server
  
  | otherwise                = do return server

addEvent :: IrcServer -> IrcEvent -> IrcServer
addEvent server event = server { sEvents = event:(sEvents server) }

-- I Don't have a better name idea.
modifyIORefIO :: IORef a -> (a -> IO a) -> IO ()
modifyIORefIO ref f = do
  obj <- readIORef ref
  ret <- f obj
  writeIORef ref ret

write :: Handle -> String -> IO ()
write h msg = do
  putStrLn $ "<< " ++ msg ++ "\\r\\n"
  hPutStr h (msg ++ "\r\n")
