{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Haste.App                 (liftIO, mkConfig, remote,
                                            runApp, runClient,
                                            withElem)
#ifndef __HASTE__

import           Control.Monad             (forM_, forever)
import           Control.Monad.Trans.Class (lift)

import           Server.UdpServer          (startUdpServer)
import           NetBeans                  (netBeans)

#endif

import           Control.Applicative       ((<$>), (<*>))
import           Control.Concurrent        (forkIO)
import           Control.Concurrent.MVar   (MVar (..), newEmptyMVar, putMVar,
                                            takeMVar)
import           Data.List.Split           (chunksOf)
import           Haste.App                 (MonadIO, forkServerIO)
import           Haste.JSON                (encodeJSON)
import           Haste.Prim                (fromJSStr)
import           Haste.Serialize           (toJSON)


maxStringLength = 2048

#ifdef __HASTE__

actionInVim = undefined
nb          = undefined

#else

actionInVim :: MonadIO m => MVar Action -> String -> m ()
actionInVim actionMVar action = liftIO $ do
  putMVar actionMVar $ OpenFile action
  putStrLn $ "got action: " ++ action

requestPump :: MVar Request -> MVar String -> IO ()
requestPump reqs reqChunks = forever $ do
  reqS <- (fromJSStr . encodeJSON . toJSON) <$> takeMVar reqs
  forM_ (chunksOf maxStringLength reqS) $ putMVar reqChunks
  putMVar reqChunks ""

nb :: MVar String -> MVar Action -> IO ()
nb = netBeans

#endif

main :: IO ()
main = do
  vimActions  <- newEmptyMVar
  vimEvents   <- newEmptyMVar
#ifndef __HASTE__
  forkIO $ requestPump reqs reqChunks
#endif

  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    forkServerIO $ liftIO $ nb vimEvents vimActions
    vimEndpoint <- remote (actionInVim vimActions)

    runClient $ withElem "requests" $ render vimEndpoint

