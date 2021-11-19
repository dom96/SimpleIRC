-- |
-- Module : Network.SimpleIRC.Sasl
-- Copyright : (c) Dominik Picheta 2010
-- License : BSD3
--
-- Maintainer : morfeusz8@gmail.com
-- Stability : provisional
-- Portability : portable
--
-- An extensible way to implement Sasl authentication
{-# LANGUAGE OverloadedStrings, CPP #-}
module Network.SimpleIRC.Sasl
  ( SaslSend(..)
  , SaslAwait(..)
  , SaslMechanism(..)
  , SaslPlainArgs(..)
  , saslPlain
  , encodeAuthMsg
  , decodeAuthMsg
  ) where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B
import qualified Data.List.NonEmpty as NE

data SaslMechanism = SaslMechanism
    { saslMechanismName :: String
    , saslMechanismProc :: SaslAwait
    }

-- | Send a message, then await a response.  message provided in plaintext
-- and will be encoded in base64 en-route
data SaslSend = SaslSend (Maybe B.ByteString) SaslAwait

-- | Await a response, then proceed.  Response will be pre-decoded from
-- base64
data SaslAwait =
    SaslAwaitResp (B.ByteString -> Either String SaslSend)
  | SaslAwaitDone

data SaslPlainArgs = SaslPlainArgs
    { saslPlainAuthz :: Maybe String    -- ^ authorization id (usually blank)
    , saslPlainAuthn :: String          -- ^ authentication id (name)
    , saslPlainPass  :: String
    }

saslPlain :: SaslPlainArgs -> SaslMechanism
saslPlain (SaslPlainArgs authz authn pass) = SaslMechanism
    { saslMechanismName = "PLAIN"
    , saslMechanismProc = SaslAwaitResp $ \_ -> Right $
        SaslSend (Just authMsg) SaslAwaitDone
    }
  where
    authMsg = B.pack $ concat
      [ concat authz, "\0", authn, "\0", pass ]

splitMessage :: B.ByteString -> NonEmpty B.ByteString
splitMessage bs = case B.length bs of
    400 -> x `NE.cons` splitMessage y
    0   -> "+" :| []
    _   -> x :| []
  where
    (x, y) = B.splitAt 400 bs

encodeAuthMsg :: B.ByteString -> NonEmpty B.ByteString
encodeAuthMsg = splitMessage . B64.encodeBase64'

-- | ignores needed padding in base64
decodeAuthMsg
    :: B.ByteString                                 -- ^ currently seen so far
    -> B.ByteString                                 -- ^ newly seen
    -> Either B.ByteString (Maybe B.ByteString)     -- ^ 'Left' if more input needed
decodeAuthMsg soFar = fmap (fmap B64.decodeBase64Lenient) . unsplitMsg soFar

unsplitMsg
    :: B.ByteString                                 -- ^ currently seen so far
    -> B.ByteString                                 -- ^ newly seen
    -> Either B.ByteString (Maybe B.ByteString)     -- ^ 'Left' if more input needed
unsplitMsg soFar newMsg = case compare (B.length newMsg) 400 of
  LT
    | newMsg == "+" -> Right . Just $ soFar
    | otherwise     -> Right . Just $ soFar `B.append` newMsg
  EQ -> Left $ soFar `B.append` newMsg
  GT -> Right Nothing
