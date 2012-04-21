{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CoreSpec (main, spec) where

import           Control.Concurrent
import           Test.HUnit
import           Test.Hspec.Monadic
import           Test.Hspec.HUnit()
import qualified Data.Knob as K
import qualified Data.Map as Map
import           System.IO
import qualified Data.ByteString.Char8 as B
import           Data.Unique

import           Network.SimpleIRC.Core

appendMVar mList x = do
  modifyMVar_ mList (\l -> return (x:l))

mockMirc = do
  k <- K.newKnob ""
  h <- K.newFileHandle k "test connection" ReadWriteMode
  u1 <- newUnique
  u2 <- newUnique
  u3 <- newUnique
  resultList <- newMVar []
  mIrc <- newMVar $ IrcServer
    { sAddr         = B.pack ""
    , sPort         = 0
    , sNickname     = B.pack ""
    , sPassword     = Nothing
    , sUsername     = B.pack ""
    , sRealname     = B.pack ""
    , sChannels     = []
    , sEvents       = Map.fromList [ (u1, Disconnect $ \_   -> appendMVar resultList True)
                                   , (u2, Privmsg    $ \_ _ -> appendMVar resultList False)
                                   , (u3, Disconnect $ \_   -> appendMVar resultList True)
                                   ]
    , sSock         = Just h
    , sListenThread = Nothing
    , sCmdThread    = Nothing
    , sCmdChan      = undefined
    , sDebug        = False
    -- Other info
    , sCTCPVersion  = ""
    , sCTCPTime     = return ""
    , sPingTimeoutInterval = 10
    }
  return (resultList, mIrc)

main = hspecX spec

spec :: Specs
spec = do
  describe "listenLoop" $ do
    it "calls the function of all disconnect events on disconnect" $ do
      (mResultList, mIrc) <- mockMirc
      listenLoop mIrc
      resultList <- takeMVar mResultList
      assertEqual "exactly both disconnect events have added their value to the result list" [True, True] resultList
