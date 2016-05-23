{-# LANGUAGE ScopedTypeVariables #-}

module IPC (
    runListener
  , sendToListener
  ) where

import Control.Monad (when)
import Control.Exception.Base (bracket)
import Data.Serialize (Serialize, runGet, get, Get, runPut, put)

import Network.Socket (socket, bind, listen, accept, connect, close)
import Network.Socket (Socket, Family(..), SocketType(..), SockAddr(..))
import Network.Socket.ByteString (send, recv)

import System.Posix.Files (fileExist, removeLink)

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, writeChan)

import qualified Data.ByteString.Char8 as BS8(length)

import Data.Int (Int32)


runListener :: forall a. (Serialize a) =>  String -> Chan a -> IO ()
runListener socketPath chan = bracket createSocket destroySocket acceptLoop where

  createSocket :: IO Socket
  createSocket = do
    fileExist socketPath >>= \exists -> when exists $ removeLink socketPath
    sock <- socket AF_UNIX Stream 0 
    bind sock (SockAddrUnix socketPath)
    listen sock 1
    return sock 

  destroySocket :: Socket -> IO ()
  destroySocket sock = do
    close sock 

  acceptLoop :: Socket -> IO ()
  acceptLoop sock = do
    (sock', _) <- accept sock
    _ <- forkIO $ processMessage sock'
    acceptLoop sock

  processMessage :: Socket -> IO ()
  processMessage sock = (runGet (get::Get Int32)) <$> recv sock 4 >>= either
      putStrLn
      (\l -> (runGet (get::Get a)) <$> recv sock (fromIntegral l :: Int) >>= either
         putStrLn
         (writeChan chan))


sendToListener :: (Serialize a) => String -> a -> IO ()
sendToListener socketPath msg = bracket connectToSocket close sendMessage where

  connectToSocket :: IO Socket
  connectToSocket = do 
    sock <- socket AF_UNIX Stream 0
    connect sock (SockAddrUnix socketPath)
    return sock

  sendMessage :: Socket -> IO ()
  sendMessage sock = do 
    let s = runPut $ put msg
    let l = BS8.length $ s

    send sock (runPut $ put (fromIntegral l :: Int32)) >>= \l' -> when (l' /= 4) $
      putStrLn "failed to send message"

    send sock s >>= \l' -> when (l' /= l) $
      putStrLn "failed to send message"
     

