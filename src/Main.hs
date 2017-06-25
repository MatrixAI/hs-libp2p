module Main where

import qualified Network.LibP2P.Multistream as MS
import qualified Network.LibP2P.Host.Basic  as BH
import           Network.LibP2P.Host.Basic  (Host)
import qualified Data.Multiaddr         as MA

-- imports typeclass instances of Key, PrivKey, PubKey
import       Crypto.LibP2P          
import       Control.Monad.Random       

import           Network.LibP2P.Network       (writeStream, readStream)
import qualified Network.LibP2P.Peer        as P
import qualified Network.LibP2P.Swarm       as SW
import qualified Network.LibP2P.Peerstore   as PS 

main :: IO ()
main = do
    listenF <- (read :: Int) 
    target <- (read :: Text) 
    secIO <- (read :: Bool)
    seed <- (read :: Int)

    -- Make a host that listens on the given multiaddress
    ha <- makeBasicHost listenF target secIO seed

    -- Set a stream handler on host A. /echo/1.0.0 is
    -- a user-defined protocol name.
    let echoHost = addHandler ha echo where
        echo :: Handler
        echo = newHandler "/echo/1.0.0"

        -- fork a new thread IO with the handler to listen for connections
    if target == "" 
          then do
              --forkIO echoHost

    -- The following code extracts target's peer ID from the
    -- given multiaddress
    let ipfsaddr = MA.newMultiAddr target
    let pid = P.toPeerId $ MA.decode ipfsaddr

    -- Decapsulate the /ipfs/<peerID> part from the target
    -- /ip4/<a.b.c.d>/ipfs/<peer> becomes /ip4/<a.b.c.d>
    let mAddr = MA.decapsulate ipfsaddr ("ipfs/" ++ show pid) 
    let targetAddr = case mAddr of
                       Left e -> error "Couldn't get a peerID from the addr"
                       Right addr -> addr

    -- We have a peer ID and a targetAddr so we add it to the peerstore
    -- so LibP2P knows how to contact it
    PS.addAddr (peerStore ha) peerid targetAddr (PS.timeToLive)

    -- make a new stream from host B to host A
    -- it should be handled on host A by the handler we set above because
    -- we use the same /echo/1.0.0 protocol
    s <- BH.makeStream echoHost peerid, "/echo/1.0.0"

    writeStream s $ MS.encode "Hello, world!\n"

    reply <- readStream s

-- makeBasicHost creates a LibP2P host with a random peer ID listening on the
-- given multiaddress. It will use secio if secio is true.
makeBasicHost :: Int -> Bool -> Integer -> IO Host
makeBasicHost listenPort secio randseed = do 
    -- Provide a cryptographically secure source of randomness
    -- needs to be an instance of MonadRandom for use with cryptonite
    
    -- Generate a key pair for this host. We will use it at least
    -- to obtain a valid host ID.
    -- use an instance of Key a , PrivKey a, PubKey a for the crypto
    -- priv, pub <-

    -- Obtain Peer ID from public key
    let pid = P.keyToPeerId pub

    -- Create a multiaddress
    let addr = case toMultiaddr "/ip4/127.0.0.1/tcp/" ++ show listenPort of
                 Left e -> error e 
                 Right a -> a

    -- Create a peerstore
    ps <- PS.newPeerStore

    -- If using secio, we add the keys to the peerstore
    -- for this peer ID.
    when secIO $ 
        PS.addPrivKey priv ps 
        PS.addPubKey pub ps

    -- Set up stream multiplexer
    tpt <- MS.makeBlankTransport
    MS.addTransport "/yamux/1.0.0" yamux.DefaultTransport

    -- Create swarm (implements libP2P Network)
    swarm <- SW.makeNewSwarm addr pid ps tpt
    
    netw <- SW.network swarm

    basicHost <- BH.newHost netw

    -- Build host multiaddress
    let hostAddr = case toMultiaddr ("/ipfs/" ++ show $ BH.id basicHost) of
                     Left e -> error e
                     Right a -> a

    -- Now we can build a full multiaddress to reach this host
    -- by encapsulating both addresses:
    fullAddr = MA.encapsulate addr hostAddr
    -- TODO: need to include some logging functions

    return basicHost

-- doEcho reads a line of data a stream and writes it back
doEcho :: BH.Stream -> IO ()
doEcho = do
    message <- BH.readStream s
    writeStream s message
