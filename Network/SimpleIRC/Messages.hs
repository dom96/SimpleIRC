-- |
-- Module : Network.SimpleIRC.Core
-- Copyright : (c) Dominik Picheta 2010
-- License : BSD3
--
-- Maintainer : morfeusz8@gmail.com
-- Stability : provisional
-- Portability : portable
--
-- Messages (parsing) module
--
{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Network.SimpleIRC.Messages
  ( IrcMessage(..)
  , Command(..)
  , parse
  , showCommand
  )
where
import qualified Data.ByteString.Char8 as B
import Control.Arrow hiding (first)
import Data.Typeable

-- PING :asimov.freenode.net
-- :haskellTestBot!~test@host86-177-151-242.range86-177.btcentralplus.com JOIN :#()

-- :dom96!~dom96@unaffiliated/dom96 PRIVMSG #() :it lives!
-- :haskellTestBot MODE haskellTestBot :+i
-- :asimov.freenode.net 376 haskellTestBot :End of /MOTD command.

-- :asimov.freenode.net 332 haskellTestBot #() :Parenthesis

-- :asimov.freenode.net 333 haskellTestBot #() Raynes!~macr0@unaffiliated/raynes 1281221819

data Command =
    MPrivmsg B.ByteString B.ByteString                      -- ^ PRIVMSG #chan :msg
  | MJoin    B.ByteString (Maybe B.ByteString)              -- ^ JOIN #chan key
  | MPart    B.ByteString B.ByteString                      -- ^ PART #chan :msg
  | MMode    B.ByteString B.ByteString (Maybe B.ByteString) -- ^ MODE #chan +o user
  | MTopic   B.ByteString (Maybe B.ByteString)              -- ^ TOPIC #chan :topic
  | MInvite  B.ByteString B.ByteString                      -- ^ INVITE user #chan
  | MKick    B.ByteString B.ByteString B.ByteString         -- ^ KICK #chan user :msg
  | MQuit    B.ByteString                                   -- ^ QUIT :msg
  | MNick    B.ByteString                                   -- ^ NICK newnick
  | MNotice  B.ByteString B.ByteString                      -- ^ NOTICE usr/#chan :msg
  | MAction  B.ByteString B.ByteString                      -- ^ PRIVMSG usr/#chan :ACTION msg
  deriving (Eq, Read, Show)

data IrcMessage = IrcMessage
  { mNick   :: Maybe B.ByteString
  , mUser   :: Maybe B.ByteString
  , mHost   :: Maybe B.ByteString
  , mServer :: Maybe B.ByteString
  , mCode   :: B.ByteString
  , mMsg    :: B.ByteString
  , mChan   :: Maybe B.ByteString
  , mOrigin :: Maybe B.ByteString   -- ^ Origin of the message, this is mNick if a message was sent directly to the bot, otherwise if it got sent to the channel it's mChan.
  , mOther  :: Maybe [B.ByteString]
  , mRaw    :: B.ByteString
  } deriving (Show, Typeable)

-- |Parse a raw IRC message
parse :: B.ByteString -> IrcMessage
parse txt =
  case split of
    [code, msg]                     -> parse2 code msg noCarriage
    [first, code, msg]              -> parse3 first code msg noCarriage
    [first, code, chan, msg]        -> parse4 first code chan msg noCarriage
    [first, code, chan, other, msg] -> parse5 first code chan other msg noCarriage
    server:code:nick:chan:other     -> parseOther server code nick chan other noCarriage
    _                               -> error "SimpleIRC: unexpected message format"

  where noCarriage = takeCarriageRet txt
        split      = smartSplit noCarriage

-- Nick, Host, Server
parseFirst :: B.ByteString -> (Maybe B.ByteString, Maybe B.ByteString, Maybe B.ByteString, Maybe B.ByteString)
parseFirst first =
  if '!' `B.elem` first
    then let (nick, user_host) = B.break (== '!') (dropColon first)
         in if '@' `B.elem` user_host
               then let (user, host) = second B.tail $ B.break (== '@') $ B.tail user_host
                    in (Just nick, Just user, Just host, Nothing)
               else (Just nick, Nothing, Just user_host, Nothing)
    else (Nothing, Nothing, Nothing, Just $ dropColon first)

getOrigin :: Maybe B.ByteString -> B.ByteString -> B.ByteString
getOrigin (Just nick) chan =
  if "#" `B.isPrefixOf` chan || "&" `B.isPrefixOf` chan || "+" `B.isPrefixOf` chan
      || "!" `B.isPrefixOf` chan
    then chan
    else nick
getOrigin Nothing chan = chan

parse2 :: B.ByteString -> B.ByteString -> B.ByteString -> IrcMessage
parse2 code msg =
  IrcMessage Nothing Nothing Nothing Nothing code
    (dropColon msg) Nothing Nothing Nothing

parse3 :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> IrcMessage
parse3 first code msg =
  let (nick, user, host, server) = parseFirst first
  in IrcMessage nick user host server code (dropColon msg) Nothing Nothing Nothing

parse4 :: B.ByteString
       -> B.ByteString
       -> B.ByteString
       -> B.ByteString
       -> B.ByteString
       -> IrcMessage
parse4 first code chan msg =
  let (nick, user, host, server) = parseFirst first
  in IrcMessage nick user host server code
       (dropColon msg) (Just chan) (Just $ getOrigin nick chan) Nothing

parse5 :: B.ByteString
       -> B.ByteString
       -> B.ByteString
       -> B.ByteString
       -> B.ByteString
       -> B.ByteString
       -> IrcMessage
parse5 first code chan other msg =
  let (nick, user, host, server) = parseFirst first
  in IrcMessage nick user host server code
    (dropColon msg) (Just chan) (Just $ getOrigin nick chan) (Just [other])

parseOther :: B.ByteString
           -> B.ByteString
           -> B.ByteString
           -> B.ByteString
           -> [B.ByteString]
           -> B.ByteString
           -> IrcMessage
parseOther server code nick chan other =
  IrcMessage (Just nick) Nothing Nothing (Just server) code
    (B.unwords other) (Just chan) (Just $ getOrigin (Just nick) chan) (Just other)

smartSplit :: B.ByteString -> [B.ByteString]
smartSplit txt =
  case B.breakSubstring (B.pack " :") (dropColon txt) of
    (x,y) | B.null y ->
              B.words txt
          | otherwise ->
              let (_, msg) = B.break (== ':') y
              in B.words x ++ [msg]

takeLast :: B.ByteString -> B.ByteString
takeLast xs = B.take (B.length xs - 1) xs

takeCarriageRet :: B.ByteString -> B.ByteString
takeCarriageRet xs =
  if B.drop (B.length xs - 1) xs == B.pack "\r"
    then takeLast xs
    else xs

dropColon :: B.ByteString -> B.ByteString
dropColon xs =
  if B.take 1 xs == B.pack ":"
    then B.drop 1 xs
    else xs

showCommand :: Command -> B.ByteString
showCommand (MPrivmsg chan msg)             = "PRIVMSG " `B.append` chan `B.append`
                                              " :" `B.append` msg
showCommand (MJoin    chan (Just key))      = "JOIN " `B.append` chan `B.append`
                                              " " `B.append` key
showCommand (MJoin    chan Nothing)         = "JOIN " `B.append` chan
showCommand (MPart    chan msg)             = "PART " `B.append` chan `B.append`
                                              " :" `B.append` msg
showCommand (MMode    chan mode (Just usr)) = "MODE " `B.append` chan `B.append`
                                              " " `B.append` mode `B.append`
                                              " " `B.append` usr
showCommand (MMode    chan mode Nothing)    = "MODE " `B.append` chan `B.append`
                                              " " `B.append` mode
showCommand (MTopic   chan (Just msg))      = "TOPIC " `B.append` chan `B.append`
                                              " :" `B.append` msg
showCommand (MTopic   chan Nothing)         = "TOPIC " `B.append` chan
showCommand (MInvite  usr chan)             = "INVITE " `B.append` usr `B.append`
                                              " " `B.append` chan
showCommand (MKick    chan usr msg)         = "KICK " `B.append` chan `B.append`
                                              " " `B.append` usr `B.append`
                                              " :" `B.append` msg
showCommand (MQuit    msg)                  = "QUIT :" `B.append` msg
showCommand (MNick    nick)                 = "NICK " `B.append` nick
showCommand (MNotice  chan msg)             = "NOTICE " `B.append` chan `B.append`
                                              " :" `B.append` msg
showCommand (MAction  chan msg)             = showCommand $ MPrivmsg chan
                                              ("\x01ACTION " `B.append` msg
                                              `B.append` "\x01")

