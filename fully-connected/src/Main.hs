{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Concurrent as C
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS

-- peers = [ "127.0.0.1:3000", "127.0.0.1:3001", "127.0.0.1:3002" ]

-- "The proper programming model is one Socket is handled by a single thread"
-- one thread per Socket
-- `connect :: Socket -> SockAddr -> IO ()`: each client calls this on each of the peers
-- `listen :: Socket -> Int -> IO ()`: each client should listen for connections to itself

-- Parse 

-- Echo server

main :: IO ()
main = withSocketsDo $ do
  -- get the network address for the server
  address <- resolve "3000"
  -- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
  -- bracket is used when you want to acquire a resource, do some work with it, 
  -- and then release the resource.
  -- If an exception is raised, bracket will release the resource and then raise
  -- the exception again.
  -- The first argument is the computation to acquire the resource.
  -- The second argument is the computation to release the resource.
  -- The third argument is the the computation to run in between acquiring and 
  -- releasing the resource.
  -- In this case, the resource is a socket.
  E.bracket (open address) close loop
  where
    -- resolve resolves a string to an S.AddrInfo
    resolve :: String -> IO S.AddrInfo
    resolve port = do 
      let hints = defaultHints
                    { addrFlags = [S.AI_PASSIVE]
                    , addrSocketType = S.Stream
                    } 
      (address : _) <- getAddrInfo (Just hints) Nothing (Just port) -- resolves to one or more addresses with the "best" first
      return address
    
    -- start the server
    open :: S.AddrInfo -> IO S.Socket
    open address = do
      -- create socket
      socket <- S.socket (addrFamily address) (addrSocketType address) (addrProtocol address)
      S.setSocketOption socket S.ReuseAddr 1
      -- bind the socket to the server's address
      S.bind socket (S.addrAddress address)
      let fd = fdSocket socket
      setCloseOnExecIfNeeded fd
      S.listen socket 10
      return socket

    loop :: S.Socket -> IO ()
    loop sock = forever $ do
      -- start accepting connections to the server
      (connection, peer) <- S.accept socket
      S.putStrLn $ "Connection from " ++ (show peer)
      void $ forkFinally (talk connection) (\_ -> close connection)

    
    talk :: S.Socket -> IO ()
    talk connection = do
      -- receive the message from the client
      msg <- recv connection 1024
      -- if the message is empty, do nothing
      unless (BS.null msg) $ do
        -- send the message back to the client
        S.sendAll connection msg
        talk connection

-- data Topology a = Topology [Peer a] 

-- data Peer a = Peer a 

-- createTopology :: String -> [String] -> Topology a
-- createTopology = undefined

-- destroyTopology :: Topology a -> IO ()
-- destroyTopology = undefined

-- addPeer :: Peer a -> Topology a -> Topology a
-- addPeer = undefined

-- removePeer :: Peer a -> Topology a -> Topology a
-- removePeer = undefined

-- getSocket :: Peer a -> Peer a
-- getSocket = undefined

-- getPeers :: Topology a -> [Peer a]
-- getPeers :: undefined
