{-# LANGUAGE StrictData      #-}  -- Give an error instead of warnings in case
                                  -- user forgets to initialize some fields
{-# LANGUAGE RecordWildCards #-}

module Z.HTTP.Client where

import GHC.Word (Word16)
import Data.Foldable (foldr)
import Data.Functor.Identity (Identity)
import Data.Function ((&))
import Z.IO.Network
    ( getAddrInfo
    , defaultTCPClientConfig
    , initTCPClient
    , AddrInfo(addrAddress)
    , HostName
    , PortNumber(..)
    , TCPClientConfig(tcpRemoteAddr)
    )
import Z.IO.Buffered (newBufferedIO, readBuffer, writeBuffer')
import Z.IO (withResource)
import Z.Data.HTTP.Request (Method (..), Version(..))
import Z.Data.Text (Text)
import Z.Data.Parser (Parser)
import Z.Data.CBytes (fromBytes, buildCBytes, toBytes)
import qualified Z.Data.Parser  as P
import qualified Z.Data.ASCII   as C
import qualified Z.Data.Vector  as V
import qualified Z.Data.Builder as B
import qualified Z.Data.Text    as T

import Z.Data.Vector.FlatMap (FlatMap)
import qualified Z.Data.Vector.FlatMap as FlatMap

type Path = V.Bytes

data Request = Request
    { reqMethod :: Method
    , reqPath :: Path
    , reqHost :: V.Bytes
    , reqVersion :: Version
    , reqHeaders :: [(V.Bytes, V.Bytes)]
    }

type Headers = FlatMap V.Bytes V.Bytes

emptyHeaders :: Headers
emptyHeaders = FlatMap.empty

data Response = Response
    { responseVersion :: Version
    , responseCode    :: Word16  -- smallest unit that can contain 3 digits int
    , responseMessage :: V.Bytes
    , responseHeaders :: Headers
    } deriving Show

defaultRequest :: Request
defaultRequest = Request
    { reqMethod = GET
    , reqPath = "/"
    , reqHost = V.empty
    , reqVersion = Version 1 1
    , reqHeaders = []
    }

type Host = (HostName, Maybe PortNumber)

resolveDNS :: Host -> IO AddrInfo
resolveDNS (hostName, Just portNumber) = head <$> getAddrInfo Nothing hostName (buildCBytes . B.int $ portNumber)
resolveDNS (hostName, Nothing) = head <$> getAddrInfo Nothing hostName "http"

pattern CRLF :: V.Bytes
pattern CRLF = "\r\n"

-- build lazily
buildHeaders :: [(V.Bytes, V.Bytes)] -> B.Builder ()
buildHeaders = foldr buildHeader ""
  where
    buildHeader :: (V.Bytes, V.Bytes) -> B.Builder () -> B.Builder ()
    buildHeader (headerKey, headerVal) b = B.append b $ do
        B.bytes headerKey
        B.word8 C.COLON
        B.bytes headerVal
        B.bytes CRLF

requestToBytes :: Request -> V.Bytes
requestToBytes req = B.build $ do
    B.bytes method
    B.encodePrim C.SPACE
    B.bytes path
    B.encodePrim C.SPACE
    B.bytes version
    B.bytes CRLF
    headers
    B.bytes CRLF
  where
    method  :: V.Bytes = T.toUTF8Bytes (reqMethod req)
    path    :: V.Bytes = reqPath req
    version :: V.Bytes = T.toUTF8Bytes (reqVersion req)
    headers = buildHeaders $ ("Host", reqHost req) : reqHeaders req

-- TODO: user defined chunksize?
sendRequest :: Request -> IO Response
sendRequest req = do
    addr <- resolveDNS (fromBytes $ reqHost req, Nothing)
    withResource (initTCPClient defaultTCPClientConfig { tcpRemoteAddr = addrAddress addr }) $ \tcp -> do
        (i, o) <- newBufferedIO tcp
        writeBuffer' o (requestToBytes req)
        buf <- readBuffer i
        case P.parse' httpParser buf of
            Left _ -> undefined
            Right res -> pure res

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
    !headers <- headersLoop emptyHeaders
    return $ Response (Version maj min) httpCode httpMsg headers

  where
    headersLoop :: Headers -> Parser Headers
    headersLoop acc = do
        w <- P.peek
        case w of
            C.CARRIAGE_RETURN -> do
                P.bytes CRLF
                return acc
            _ -> do
                key <- P.takeWhile (/= C.COLON)
                P.word8 C.COLON
                P.skipSpaces
                val <- P.takeWhile (/= C.CARRIAGE_RETURN)
                P.bytes CRLF
                headersLoop $ FlatMap.insert key val acc

-- This should parse "www.google.com:80" but not "http://www.google.com:80"
parseHost :: Parser Host
parseHost = do
    hostName <- P.takeTill (== C.COLON)
    w <- P.peek
    if w == C.COLON
        then P.skipWord8 >> ((,) (fromBytes hostName) <$> (Just . PortNumber <$> P.int))
        else pure (fromBytes hostName, Nothing)

fromHost :: Text -> Request
fromHost host = let host' = T.toUTF8Bytes host in
    case P.parse' parseHost (T.toUTF8Bytes host) of
        Left err -> error (show err)
        Right (hostName, Nothing) -> defaultRequest { reqHost = toBytes hostName }
        Right (hostName, Just port) -> defaultRequest { reqHost = toBytes hostName <> ":" <> T.toUTF8Bytes port }

fromIpAddr :: Text -> Request
fromIpAddr = undefined

-- | f :: Request
-- | f = fromHost "www.google.com"
-- |     & setPath "/url"
-- |     & setHeadears [("X-Powered-By", "Z-HTTP-Client")]

setMethod :: Method -> Request -> Request
setMethod method req@Request{..} = req { reqMethod = method }

setPath :: V.Bytes -> Request -> Request
setPath path req@Request{..} = req { reqPath = path }

setVersion :: Version -> Request -> Request
setVersion version req@Request{..} = req { reqVersion = version }

setHeadears :: [(V.Bytes, V.Bytes)] -> Request -> Request
setHeadears headers req@Request{..} = req { reqHeaders = headers } 
