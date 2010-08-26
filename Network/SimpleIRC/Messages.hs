-- |
-- Module : Network.SimpleIRC.Core
-- Copyright : (c) Dominik Picheta 2010
-- License : BSD3
--
-- Maintainer : morfeusz8@gmail.com
-- Stability : Alpha
-- Portability : portable
--
-- Messages module
--
{-# LANGUAGE OverloadedStrings #-}
module Network.SimpleIRC.Messages 
  ( Command(..)
  , parse
  , showCommand  
  ) 
where
import Data.Maybe
import Network.SimpleIRC.Types
import qualified Data.ByteString.Char8 as B

-- PING :asimov.freenode.net
-- :haskellTestBot!~test@host86-177-151-242.range86-177.btcentralplus.com JOIN :#()

-- :dom96!~dom96@unaffiliated/dom96 PRIVMSG #() :it lives!
-- :haskellTestBot MODE haskellTestBot :+i
-- :asimov.freenode.net 376 haskellTestBot :End of /MOTD command.

-- :asimov.freenode.net 332 haskellTestBot #() :Parenthesis

-- :asimov.freenode.net 333 haskellTestBot #() Raynes!~macr0@unaffiliated/raynes 1281221819

data Command = Command
  | MPrivmsg B.ByteString B.ByteString              -- ^ PRIVMSG #chan :msg
  | MJoin    B.ByteString (Maybe B.ByteString)      -- ^ JOIN #chan key
  | MPart    B.ByteString B.ByteString              -- ^ PART #chan :msg
  | MMode    B.ByteString B.ByteString 
      (Maybe B.ByteString)                          -- ^ MODE #chan +o user
  | MTopic   B.ByteString (Maybe B.ByteString)      -- ^ TOPIC #chan :topic
  | MInvite  B.ByteString B.ByteString              -- ^ INVITE user #chan
  | MKick    B.ByteString B.ByteString B.ByteString -- ^ KICK #chan user :msg
  | MQuit    B.ByteString                           -- ^ QUIT :msg
  | MNick    B.ByteString                           -- ^ NICK newnick
  | MNotice  B.ByteString B.ByteString              -- ^ NOTICE usr/#chan :msg
  | MAction  B.ByteString B.ByteString              -- ^ PRIVMSG usr/#chan :ACTION msg
  deriving (Eq, Read, Show)

-- |Parse a raw IRC message
parse :: B.ByteString -> IrcMessage
parse txt = 
  case length split of 2 -> (parse2 split) txt
                       3 -> (parse3 split) txt
                       4 -> (parse4 split) txt 
                       5 -> (parse5 split) txt
                       otherwise -> (parseOther split) txt
                       
  where split = smartSplit (takeCarriageRet txt)

parse4 :: [B.ByteString] -> (B.ByteString -> IrcMessage)
parse4 (first:code:chan:msg:_) = 
  let (nick, host, server) = parseFirst first
  in IrcMessage nick host server (code)
       (dropColon msg) (Just chan) Nothing

-- Nick, Host, Server
parseFirst :: B.ByteString -> (Maybe B.ByteString, Maybe B.ByteString, Maybe B.ByteString)
parseFirst first = 
  if '!' `B.elem` first
    then let (nick, host) = B.break (== '!') (dropColon first)
         in (Just nick, Just host, Nothing)
    else (Nothing, Nothing, Just $ dropColon first) 

dropColon :: B.ByteString -> B.ByteString
dropColon xs =
  if B.take 1 xs == (B.pack ":")
    then B.drop 1 xs
    else xs

parse2 :: [B.ByteString] -> (B.ByteString -> IrcMessage)
parse2 (code:msg:_) =
  IrcMessage Nothing Nothing Nothing (code)
    (dropColon msg) Nothing Nothing
    
parse3 :: [B.ByteString] -> (B.ByteString -> IrcMessage)
parse3 (first:code:msg:_) =
  let (nick, host, server) = parseFirst first
  in IrcMessage nick host server (code) (dropColon msg) Nothing Nothing
  
parse5 :: [B.ByteString] -> (B.ByteString -> IrcMessage)
parse5 (server:code:nick:chan:msg:_) =
  IrcMessage (Just nick) Nothing (Just server) (code)
    (dropColon msg) (Just chan) Nothing

parseOther :: [B.ByteString] -> (B.ByteString -> IrcMessage)
parseOther (server:code:nick:chan:other) =
  IrcMessage (Just nick) Nothing (Just server) (code)
    (B.unwords other) (Just chan) (Just other)

smartSplit :: B.ByteString -> [B.ByteString]
smartSplit txt
  | ':' `B.elem` (dropColon txt) =
    let (first, msg) = B.break (== ':') (dropColon txt)
    in (B.words $ takeLast first) ++ [msg]
  | otherwise = B.words $ txt

takeLast :: B.ByteString -> B.ByteString
takeLast xs = B.take (B.length xs - 1) xs

takeCarriageRet :: B.ByteString -> B.ByteString
takeCarriageRet xs = 
  if B.drop (B.length xs - 1) xs == (B.pack "\r")
    then takeLast xs
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
                                              ("\x01 ACTION " `B.append` msg
                                              `B.append` "\x01")
