-- |
-- Module : Network.SimpleIRC.Core
-- Copyright : (c) Dominik Picheta 2010
-- License : BSD3
--
-- Maintainer : morfeusz8@gmail.com
-- Stability : Alpha
-- Portability : portable
--
-- Utils module
--

module Network.SimpleIRC.Utils
  ( 
    -- * Functions
    getChan
  ) where
import Network.SimpleIRC.Types
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
  
-- |If the IrcMessage was sent directly to you returns the nick else the channel.
getChan :: IrcServer -> IrcMessage -> B.ByteString
getChan s m =
  if sNickname s == chan
    then (fromJust $ mNick m)
    else chan
  
  where chan = fromJust $ mChan m
