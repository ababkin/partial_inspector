{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}


import           Haste.App                 (liftIO, mkConfig, remote,
                                            runApp, runClient,
                                            alert, MonadIO, forkServerIO, getChildren, Elem, onEvent, Event(..), Client, (<.>), onServer, Remote, Server)
import           Haste.DOM                 (documentBody, getAttr)

#ifndef __HASTE__

{- import           Control.Monad.Trans.Class (lift) -}

import           NetBeans                  (netBeans)

#endif

import           Control.Applicative       ((<$>), (<*>))
import           Control.Monad             ((=<<), mapM, mapM_, when, forever)
import           Control.Concurrent        (forkIO)
import           Control.Concurrent.MVar   (MVar (..), newEmptyMVar,
                                            takeMVar, putMVar)
import Data.List (takeWhile)

import           Types.Action              (Action(..))

{- foreign import ccall unregisterAllClickCallbacks :: String -> IO () -}


#ifdef __HASTE__

actionInVim = undefined
nb          = undefined



#else

actionInVim :: MonadIO m => MVar Action -> String -> m ()
actionInVim actionMVar action = liftIO $ putMVar actionMVar $ OpenFile action

nb :: MVar String -> MVar Action -> IO ()
nb = netBeans

#endif


main :: IO ()
main = do
  vimActions  <- newEmptyMVar
  vimEvents   <- newEmptyMVar

  runApp (mkConfig "ws://localhost:24601" 24601) $ do
    forkServerIO $ liftIO $ nb vimEvents vimActions
    forkServerIO $ liftIO $ forever $ do
      event <- takeMVar vimEvents
      putStrLn $ "vim event: " ++ event
    vimEndpoint <- remote (actionInVim vimActions)

    runClient $ do
      addHandlersToChildren vimEndpoint documentBody

      where
        addHandlersToChildren :: Remote (String -> Server ()) -> Elem -> Client ()
        addHandlersToChildren vimEndpoint el = do
          addHandlers vimEndpoint el
          mapM_ (addHandlersToChildren vimEndpoint) =<< getChildren el

        addHandlers :: Remote (String -> Server ()) -> Elem -> Client ()
        addHandlers vimEndpoint el = do
          path <- getAttr el "data-trace"
          case path of
            "" -> return ()
            _ -> el `onEvent` OnClick $ \_ _ -> do
              traceable <- isTraceable el
              when traceable $
                {- onServer $ vimEndpoint <.> (map (\c -> if c == ':' then '|' else c) $ "app/views" ++ path) -}
                onServer $ vimEndpoint <.> (takeWhile (/=':') $ "app/views" ++ path)

          where 
            isTraceable :: Elem -> Client Bool
            isTraceable el = do
              childrenTraceable <- (any id <$> ( (mapM isTraceable) =<< (getChildren el) ))
              if childrenTraceable
                then return False
                else do
                  path <- getAttr el "data-trace"
                  case path of
                    "" -> return False
                    _  -> return True
    

