module Z.Data.HTTP.Client.Request where

import Z.Data.HTTP.Request
import qualified Z.Data.Vector as V
import Z.IO (Source)
import Z.IO.Network

type Host = (HostName, Maybe PortNumber)

initialRequest :: Request
initialRequest = Request
    { requestHost = V.empty
    , requestSecure = False
    , requestRemote = SocketAddrIPv4 ipv4Any 80 -- default http port number

    , requestMethod = GET
    , requestVersion = Version 1 0
    , requestPathRaw = V.empty
    , requestPathQuery = ([], V.empty)

    , requestHeaders = V.empty

    , requestBody = Left V.empty
    }
