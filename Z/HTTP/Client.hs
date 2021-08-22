module Z.HTTP.Client where

import Z.IO.Network
import Z.IO.Buffered
import Z.IO (withResource)
import Z.Data.HTTP.Request (Method (..))
import Z.Data.Text (Text)
import Z.Data.Parser (Parser)
import Z.Data.CBytes (fromBytes, buildCBytes)
import GHC.Word
import qualified Z.Data.Parser as P
import qualified Z.Data.ASCII  as C
import qualified Z.Data.Vector as V
import qualified Z.Data.Builder as B

type Path = V.Bytes

data Request = Request
    { reqMethod :: !Method
    , reqPath :: !Path
    , reqHost :: V.Bytes
    , reqHeaders :: [(V.Bytes, V.Bytes)]
    }

defaultRequest :: Request
defaultRequest = Request
    { reqMethod = GET
    , reqPath = V.empty 
    , reqHost = V.empty
    , reqHeaders = []
    }

type Host = (HostName, Maybe PortNumber)

-- This should parse "www.google.com:80" but not "http://www.google.com:80"
parseHost' :: Parser Host
parseHost' = do
    hostName <- P.takeTill (== C.COLON)
    w <- P.peek
    if w == C.COLON
        then P.skipWord8 >> ((,) (fromBytes hostName) <$> (Just . PortNumber <$> P.int))
        else pure (fromBytes hostName, Nothing)

----------------------------

-- http-client
-- Builder model
-- https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md#request-building

-- * Record
-- Simple
-- https://hackage.haskell.org/package/http-conduit-2.3.8/docs/Network-HTTP-Conduit.html

resolveDNS :: Host -> IO AddrInfo
resolveDNS (hostName, Just portNumber) = head <$> getAddrInfo Nothing hostName (buildCBytes . B.int $ portNumber)
resolveDNS (hostName, Nothing) = head <$> getAddrInfo Nothing hostName "http"

pattern CRLF :: V.Bytes
pattern CRLF = "\r\n"

requestToBytes :: Request -> V.Bytes
requestToBytes req = mconcat [method, " ", path, " ", version, CRLF, headers, CRLF]
  where
    method :: V.Bytes = "GET" -- TODO: find a way to serialise HTTP method from enum
    path :: V.Bytes = reqPath req
    version :: V.Bytes = "HTTP/1.1"
    headers :: V.Bytes = ""

data Response = Response
    { responseVersion :: HttpVersion
    , responseCode    :: Word16 -- smallest unit that can contain 3 digits int
    , responseMessage :: V.Bytes
    , responseHeaders :: [(V.Bytes, V.Bytes)]
    } deriving (Show)

-- TODO: user defined chunksize?
sendRequest :: Request -> IO V.Bytes
sendRequest req = do
    addr <- resolveDNS (fromBytes $ reqHost req, Nothing)
    withResource (initTCPClient defaultTCPClientConfig { tcpRemoteAddr = addrAddress addr }) $ \tcp -> do
        (i, o) <- newBufferedIO tcp
        writeBuffer' o (requestToBytes req)
        readBuffer i

data HttpVersion = HttpVersion Int Int deriving (Show)

httpParser :: Parser Response
httpParser = do
    P.bytes "HTTP/"
    maj <- P.digit
    P.word8 C.DOT
    min <- P.digit
    P.skipSpaces
    httpCode <- P.uint
    P.skipSpaces
    httpMsg <- P.takeWhile (/= C.CARRIAGE_RETURN)
    P.bytes CRLF
    !headers <- headersLoop []
    return $ Response (HttpVersion maj min) httpCode httpMsg (reverse headers)

  where
    headersLoop :: [(V.Bytes, V.Bytes)] -> Parser [(V.Bytes, V.Bytes)]
    headersLoop acc = do
        w <- P.peek
        case w of
            C.CARRIAGE_RETURN -> do
                P.bytes CRLF
                return acc
            _ -> do
                headerKey <- P.takeWhile (/= C.COLON)
                P.word8 C.COLON
                P.skipSpaces
                headerVal <- P.takeWhile (/= C.CARRIAGE_RETURN)
                P.bytes CRLF
                headersLoop $ (headerKey, headerVal) : acc -- Don't forget to reverse

