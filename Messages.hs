module Messages (IrcMessage(..), parse) where
import Data.Maybe
data IrcMessage = IrcMessage
  { mNick   :: Maybe String
  , mHost   :: Maybe String
  , mServer :: Maybe String
  , mCode   :: Maybe String
  , mMsg    :: Maybe String
  , mChan   :: Maybe String
  , mOther  :: Maybe [String]
  } deriving Show

-- PING :asimov.freenode.net
-- :haskellTestBot!~test@host86-177-151-242.range86-177.btcentralplus.com JOIN :#()

-- :dom96!~dom96@unaffiliated/dom96 PRIVMSG #() :it lives!
-- :haskellTestBot MODE haskellTestBot :+i
-- :asimov.freenode.net 376 haskellTestBot :End of /MOTD command.

-- :asimov.freenode.net 332 haskellTestBot #() :Parenthesis

-- :asimov.freenode.net 333 haskellTestBot #() Raynes!~macr0@unaffiliated/raynes 1281221819


parse :: String -> IrcMessage
parse txt = 
  case length split of 2 -> parse2 split
                       3 -> parse3 split
                       4 -> parse4 split 
                       5 -> parse5 split
                       otherwise -> parseOther split
                       
  where split = smartSplit (takeCarriageRet txt)

parse4 :: [String] -> IrcMessage
parse4 (first:code:chan:msg:_) = 
  let (nick, host, server) = parseFirst first
  in IrcMessage nick host server (Just code)
       (Just $ dropColon msg) (Just chan) Nothing

-- Nick, Host, Server
parseFirst :: String -> (Maybe String, Maybe String, Maybe String)
parseFirst first = 
  if '!' `elem` first
    then let (nick, host) = break (== '!') (dropColon first)
         in (Just nick, Just host, Nothing)
    else (Nothing, Nothing, Just $ dropColon first) 

dropColon (':':xs) = xs
dropColon xs = xs

parse2 :: [String] -> IrcMessage
parse2 (code:msg:_) =
  IrcMessage Nothing Nothing Nothing (Just code)
    (Just $ dropColon msg) Nothing Nothing
    
parse3 :: [String] -> IrcMessage
parse3 (first:code:msg:_) =
  let (nick, host, server) = parseFirst first
  in IrcMessage nick host server (Just code) (Just msg) Nothing Nothing
  
parse5 :: [String] -> IrcMessage
parse5 (server:code:nick:chan:msg:_) =
  IrcMessage (Just nick) Nothing (Just server) (Just code)
    (Just $ dropColon msg) (Just chan) Nothing

parseOther :: [String] -> IrcMessage
parseOther (server:code:nick:chan:other) =
  IrcMessage (Just nick) Nothing (Just server) (Just code)
    Nothing (Just chan) (Just other)

smartSplit :: String -> [String]
smartSplit txt
  | ':' `elem` (dropColon txt) =
    let (first, msg) = break (== ':') (dropColon txt)
    in (words $ takeLast first) ++ [msg]
  | otherwise = words $ txt

takeLast :: String -> String
takeLast xs = take (length xs - 1) xs

takeCarriageRet :: String -> String
takeCarriageRet xs = 
  if drop (length xs - 1) xs == "\r"
    then takeLast xs
    else xs
