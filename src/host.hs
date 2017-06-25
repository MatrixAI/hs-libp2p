module Network.LibP2P.Host.Basic where

import           Data.Maybe                            (fromMaybe)
import           Network.LibP2P                        (Network) 
import           Network.LibP2P.Multistream.Muxer      (MultistreamMuxer)
import qualified Network.LibP2P.Multistream.Muxer   as MS
import qualified Network.LibP2P.Host.NetworkManager as NA 
-- BasicHost is the basic implementation of the host.Host interface. This
-- particular host implementation:
--  * uses a protocol muxer to mux per-protocol streams
--  * uses an identity service to send + receive node information
--  * uses a nat service to establish NAT port mappings
    

data BasicHost
{
    network    :: Network
    mux        :: MS.MultistreamMuxer
    ids        :: ID.IDService
    natmgr     :: NA.NatManager
    addrs      :: Multiaddr -> Multiaddr
    negtimeout :: Int -- TODO: should be using a haskell time library
    -- Additional parameters for negotiation
    -- proc goprocess.Process
    -- bwc metrics.Reporter

data HostOptions
{
    hostMux       :: Maybe MultistreamMuxer
    hostIDService :: Maybe ID.IDService
    hostAddr      :: Maybe (Multiaddr -> Multiaddr)
    hostNatMgr    :: NA.NatManager
    hostNegTimeout :: Int 
    --hostBandwidthReporter  :: 
}

defaultNegTimeOut = 60

defaultAddrFactory :: Multiaddr -> Multiaddr
defaultAddrFactory = id

-- NewHost constructs a new *BasicHost and activates it by attaching its stream and connection handlers to the given inet.Network.
newHost :: Network -> HostOptions -> IO BasicHost
newHost n ho = do
    let mux        = fromMaybe MS.newMultistreamMuxer $ hostMux ho 
    let ids        = fromMaybe ID.newIDService $ hostIDService ho
    let negtimeout = fromMaybe defaultNegTimeout $ hostNegTimeOut ho
    let natmgr     = hostNatMgr ho
    let addrs      = fromMaybe defaultAddrFactory $ hostAddr ho

    let newNet = do
        addConnHandler newConnHandler n
        setStreamHandler newStreamHandler n

    return BasicHost newNet mux ids natmgr addrs

newStreamHandler :: BasicHost -> Stream -> IO BasicHost
newStreamHandler bh s = do 

newConnHandler :: BasicHost -> Connection -> IO BasicHost
newConnHandler bh c = do
    let updatedPeerstore = do
        return $ PS.setProtocols $ remotePeer c
        -- TODO


-- newConnHandler is the remote-opened conn handler for inet.Network
func (h *BasicHost) newConnHandler(c inet.Conn) {
    -- Clear protocols on connecting to new peer to avoid issues caused
    -- by misremembering protocols between reconnects
    h.Peerstore().SetProtocols(c.RemotePeer())
    h.ids.IdentifyConn(c)
}
