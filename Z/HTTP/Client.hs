module Z.HTTP.Client where

import Z.Data.HTTP.Request
import Z.Data.HTTP.Client.Request
import Z.IO.Network

import Z.Data.Text (Text)
import Z.Data.Parser (Parser)
import Z.Data.CBytes

import qualified Z.Data.Parser as P
import qualified Z.Data.ASCII  as C

fromUrl :: Text -> Request
fromUrl = undefined

-- fromHost :: Text -> Either P.ParseError Host
-- fromHost host = P.parse' parseHost (T)

-- This should parse "www.google.com:80" but not "http://www.google.com:80"
parseHost :: Parser Host
parseHost = do
    hostName <- P.takeTill (== C.COLON)
    w <- P.peek
    if w == C.COLON
        then P.skipWord8 >> ((,) (fromBytes hostName) <$> (Just . PortNumber <$> P.int))
        else pure (fromBytes hostName, Nothing)
