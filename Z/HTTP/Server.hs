module Z.HTTP.Server where

import           Z.Data.HTTP.Request
import           Z.IO.Network
import           Z.IO

type ServerLoop = (UVStream -> IO ()) -> IO ()
data HTTPServerConfig = HTTPServerConfig
    { httpSendBufSiz :: Int
    , httpRecvBufSiz :: Int
    }

defaultHTTPServerConfig :: HTTPServerConfig
defaultHTTPServerConfig = HTTPServerConfig defaultChunkSize defaultChunkSize

runHTTPServer' :: ServerLoop
               -> HTTPServerConfig
               -> (Request -> IO ())
               -> IO ()
runHTTPServer' loop conf@HTTPServerConfig{..} worker = loop $ \ uvs -> do
    remoteAddr <- getTCPPeerName uvs
    bi <- newBufferedInput' httpSendBufSiz uvs
    bo <- newBufferedOutput' httpSendBufSiz uvs
    req <- readRequest remoteAddr False bi
    return ()
