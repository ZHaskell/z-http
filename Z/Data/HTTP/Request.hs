module Z.Data.HTTP.Request where

import           Control.Monad
import           Data.IORef
import qualified Data.CaseInsensitive   as CI
import qualified Z.Data.Builder         as B
import qualified Z.Data.Parser          as P
import qualified Z.Data.Text            as T
import qualified Z.Data.Vector          as V
import qualified Z.Data.Vector.Base     as V
import qualified Z.Data.Vector.Search   as V
import           Z.Data.ASCII
import           Z.Data.PrimRef
import           Z.IO
import           Z.IO.Network


data HTTPException
    = BadHeaderLine V.Bytes
    | EmptyOrBadContentLength
    | NoHostHeader
    | ClientExpectedShutDown
  deriving Show

instance Exception HTTPException

-- | HTTP standard method (as defined by RFC 2616, and PATCH which is defined
--   by RFC 5789).
data Method
    = GET
    | POST
    | HEAD
    | PUT
    | DELETE
    | TRACE
    | CONNECT
    | OPTIONS
    | PATCH
    | CUSTOM_METHOD V.Bytes
  deriving (Eq, Ord)

instance T.Print Method where
    toUTF8BuilderP _ GET      = "GET"
    toUTF8BuilderP _ POST     =  "POST"
    toUTF8BuilderP _ HEAD     =  "HEAD"
    toUTF8BuilderP _ PUT      =  "PUT"
    toUTF8BuilderP _ DELETE   =  "DELETE"
    toUTF8BuilderP _ TRACE    =  "TRACE"
    toUTF8BuilderP _ CONNECT  =  "CONNECT"
    toUTF8BuilderP _ OPTIONS  =  "OPTIONS"
    toUTF8BuilderP _ PATCH    =  "PATCH"
    toUTF8BuilderP _ (CUSTOM_METHOD bs) = B.bytes bs


data Version = Version {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving (Eq, Ord)

instance T.Print Version where
    toUTF8BuilderP _ (Version maj min) = do
        "HTTP"
        B.int maj
        B.encodePrim DOT
        B.int min

data Request = Request
    { requestHost           :: V.Bytes
    , requestSecure         :: Bool
    , requestRemote         :: SocketAddr

    , requestMethod         :: !Method
    , requestVersion        :: !Version
    , requestPathRaw        :: !V.Bytes
    , requestPathQuery      :: ([V.Bytes], V.Vector (V.Bytes, V.Bytes))

    , requestHeaders        :: V.Vector (V.Bytes, V.Bytes)

    , requestBody           :: Either V.Bytes (Source V.Bytes)
    }


requestLineParser :: P.Parser (Method, V.Bytes, Version)
requestLineParser = do
    -- method
    mbs <- P.takeWhile1 (/= SPACE)
    let !method = case mbs of
            "GET"     -> GET
            "POST"    -> POST
            "HEAD"    -> HEAD
            "PUT"     -> PUT
            "DELETE"  -> DELETE
            "TRACE"   -> TRACE
            "CONNECT" -> CONNECT
            "OPTIONS" -> OPTIONS
            "PATCH"   -> PATCH
            _         -> CUSTOM_METHOD mbs
    P.skipWord8

    -- path
    !rawpath <- P.takeWhile1 (/= SPACE)
    P.skipWord8

    -- version
    vbs <- P.bytes "HTTP/"
    majv <- P.digit
    P.word8 DOT
    minv <- P.digit
    let !version = Version majv minv

    -- request line end
    P.word8 CARRIAGE_RETURN
    P.word8 NEWLINE

    pure (method, rawpath, version)

readRequest :: HasCallStack
            => SocketAddr
            -> Bool
            -> BufferedInput
            -> IO (Bool, Request)   -- ^ (keep-alive, request)
readRequest remoteAddr secure bi = do
    -- some special headers
    contentLenRef <- newCounter 0
    transferEncodingRef <- newIORef V.empty
    hostRef <- newIORef V.empty
    connectionRef <- newIORef False

    (method, rawpath, version) <- readParser requestLineParser bi
    printStdLn (method, rawpath, version)

    headers <- readHeaderLines contentLenRef transferEncodingRef hostRef connectionRef

    host <- readIORef hostRef
    when (V.null host) $ throwIO NoHostHeader

    contentLen <- readPrimIORef contentLenRef
    transferEncoding <- readIORef transferEncodingRef
    keepAlive <- readIORef connectionRef

    body <-
        if CI.foldCase transferEncoding == "chunked"
        then sourceChunkedEncoding bi
        else if contentLen > 0
            then Left <$> readExactly contentLen bi
            else if contentLen == 0
                then return (Left V.empty)
                else throwIO EmptyOrBadContentLength

    pure (keepAlive, Request host secure remoteAddr method version
        rawpath (parsePathQuery rawpath) headers body)

  where
    parsePathQuery x = undefined
    sourceChunkedEncoding bi = pure $ Right undefined

    readHeaderLines contentLenRef transferEncodingRef hostRef connectionRef =
        let loop !i acc = do
                mhdr <- readLine bi
                case mhdr of
                    Just hdr@(V.PrimVector arr s l) ->
                        if l == 0
                        then return $! V.packRN i acc
                        else do
                            let (!n, _) = V.findByte COLON hdr
                            if n == l
                            then throwIO (BadHeaderLine hdr)
                            else do
                                let !hdrK = CI.foldCase (V.PrimVector arr s n)
                                    !hdrV = V.PrimVector arr (s+n+1) (l-n-1)


                                when (hdrK == "content-length") $
                                    case P.parse' P.uint hdrV of
                                        Right l -> writePrimIORef contentLenRef l
                                        _ -> throwIO (BadHeaderLine hdr)

                                when (hdrK == "transfer-encoding") $
                                    writeIORef transferEncodingRef hdrV

                                when (hdrK == "host") $
                                    writeIORef hostRef hdrV

                                when (hdrK == "connection") $
                                    writeIORef connectionRef $! (hdrV == "keep-alive")

                                loop (i+1) ((hdrK, hdrV) : acc)
                    _ -> throwIO ClientExpectedShutDown
        in loop 0 []

