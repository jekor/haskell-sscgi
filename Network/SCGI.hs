-- Copyright 2013 Chris Forno

module Network.SCGI (SCGIT, SCGI, runRequest, header, allHeaders, body, method, path, setHeader, responseHeader, Headers, Body, Status, Response(..), negotiate) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Arrow (first)
import Control.Exception (SomeException)
import Control.Monad (liftM, liftM2)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, MonadReader, asks)
import Control.Monad.State (StateT, runStateT, MonadState, modify, gets)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Attoparsec.ByteString.Char8 (Parser, IResult(..), parseOnly, parseWith, char, string, skipSpace, decimal, take, takeTill, inClass, rational)
import Data.Attoparsec.Combinator (many1, sepBy, option)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy.Char8 ()
import Data.Char (toUpper)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Function (on)
import Data.List (sortBy, find, maximumBy)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Lazy as M
import qualified System.FilePath.Glob as G
import System.IO (Handle)

import Prelude hiding (take)

type Headers = M.Map (CI B.ByteString) B.ByteString
type Body = BL.ByteString
type Status = BL.ByteString
data Response = Response Status Body

newtype SCGIT m a = SCGIT (ReaderT (Headers, Body) (StateT Headers m) a)
  deriving (Monad, MonadState Headers, MonadReader (Headers, Body), MonadIO, MonadCatch, MonadThrow)

type SCGI = SCGIT IO

instance MonadTrans SCGIT where
  lift = SCGIT . lift . lift

runSCGIT :: MonadIO m => Headers -> Body -> SCGIT m Response -> m (Response, Headers)
runSCGIT headers body' (SCGIT r) = runStateT (runReaderT r (headers, body')) M.empty

-- |Look up a request header.
header :: Monad m
       => B.ByteString -- ^ the header name (key)
       -> SCGIT m (Maybe B.ByteString) -- ^ the header value if found
header name = asks (M.lookup (CI.mk name) . fst)

-- |Return all request headers as a list in the format they were received from the web server.
allHeaders :: Monad m => SCGIT m [(B.ByteString, B.ByteString)] -- ^ an association list of header: value pairs
allHeaders = asks (map (first CI.original) . M.toList . fst)

-- |Return the request body.
body :: Monad m
     => SCGIT m (BL.ByteString)
body = asks snd

-- |Get the request method (GET, POST, etc.). You could look the header up
-- yourself, but this normalizes the method name to uppercase.
method :: Monad m => SCGIT m (Maybe B.ByteString) -- ^ the method if found
method = liftM (B8.map toUpper) `liftM` header "REQUEST_METHOD"

-- |Get the requested path. According to the spec, this can be complex, and
-- actual CGI implementations diverge from the spec. I've found this to work,
-- even though it doesn't seem correct or intuitive.
path :: Monad m => SCGIT m (Maybe B.ByteString) -- ^ the path if found
path = do
  path1 <- header "SCRIPT_NAME"
  path2 <- header "PATH_INFO"
  return $ liftM2 B.append path1 path2

-- |Set a response header.
setHeader :: Monad m
          => B.ByteString -- ^ the header name (key)
          -> B.ByteString -- ^ the header value
          -> SCGIT m ()
setHeader name value = modify (M.insert (CI.mk name) value)

-- |Look up a response header (one set during this request). This is useful when you need to check if a header has been set already (in case you want to modify it, for example).
responseHeader :: Monad m
               => B.ByteString -- ^ the header name (key)
               -> SCGIT m (Maybe B.ByteString)
responseHeader name = gets (M.lookup (CI.mk name))

-- |Run a request in the SCGI monad.
runRequest :: (MonadIO m, MonadCatch m)
           => Handle -- ^ the handle connected to the web server (from 'accept')
           -> SCGIT m Response -- ^ the action to run in the SCGI monad
           -> m () -- ^ nothing is returned, the result of the action is written back to the server
runRequest h f = do
  -- Note: This could potentially read any amount of data into memory.
  -- For now, I'm leaving it up to the SCGI implementation in the server to block large header payloads.
  --
  -- First, parse the netstring containing the headers. If we tried to avoid this step the syntax for
  -- the headers would be ambiguous.
  result <- liftIO $ parseWith (B.hGetSome h 4096) netstringParser ""
  case result of
    Done rest headerString ->
      case parseOnly (many1 headerParser) headerString of
        Left e -> error e
        Right headers -> do
          -- CONTENT_LENGTH is required by the SCGI spec. Without it, we
          -- wouldn't know when we'd reached the end of the request.
          --
          -- The header Map uses case-insensitive keys.
          let headerMap = M.fromList $ map (first CI.mk) headers
              len' = B8.readInt $ M.findWithDefault (error "CONTENT_LENGTH missing from request") "CONTENT_LENGTH" headerMap
          case len' of
            Just (len, _) -> do
              -- We have probably read past the end of the header. Take the
              -- rest of the unparsed string and what remains to be read
              -- (determined from the CONTENT_LENGTH) and make that the body.
              let c = fromIntegral (len - B.length rest)
              body' <- liftIO $ (BL.fromChunks [rest] `BL.append`) `liftM` (if c > 0 then BL.hGet h c else return "")
              (Response status body'', headers') <- catch (runSCGIT headerMap body' f) handleException
              -- Every SCGI response must include a status line first.
              liftIO $ BL.hPutStr h $ BL.concat ["Status: ", status, "\r\n"]
              -- Output the headers returned by the SCGI action.
              liftIO $ mapM_ (\(k, v) -> B.hPutStr h $ B.concat [CI.original k, ": ", v, "\r\n"]) $ M.toList headers'
              liftIO $ BL.hPutStr h "\r\n"
              -- Finally, output the body.
              liftIO $ BL.hPutStr h body''
            _ -> error "Failed to parse CONTENT_LENGTH."
    _ -> error "Failed to parse SCGI request."
 where handleException :: MonadIO m => SomeException -> m (Response, Headers)
       handleException e = return ( Response "500 Internal Server Error" (BLU.fromString $ show e)
                                  , M.fromList [("Content-Type", "text/plain; charset=utf-8")] )

-- http://cr.yp.to/proto/netstrings.txt
netstringParser :: Parser B.ByteString
netstringParser = do
  count <- decimal <* char ':'
  take count <* char ','

headerParser :: Parser (B.ByteString, B.ByteString)
headerParser = (,) <$> cStringParser <*> cStringParser

cStringParser :: Parser B.ByteString
cStringParser = takeTill (== '\NUL') <* char '\NUL'

negotiate :: Monad m => [B.ByteString] -> SCGIT m [B.ByteString]
negotiate representations = do
  accept <- header "HTTP_ACCEPT"
  case accept of
    Nothing -> return representations
    Just acc -> return $ best $ matches representations acc

-- Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
-- Available: text/html, text/plain
-- acceptParser = [("text/html", 1.0), ("application/xhtml+xml", 1.0), ("application/xml", 0.9), ("*/*", 0.8)]
-- matches = [("text/html", 1.0), ("text/plain", 0.8)]
-- best . matches = [(text/html, 1.0)]

-- Accept: */*
-- Available: text/html, text/plain
-- acceptParser = [("*/*", 1.0)]
-- matches = [("text/html", 1.0), ("text/plain", 0.8)]
-- best . matches = [("text/html", 1.0), ("text/plain", 0.8)]

type Quality = Double

-- e.g. text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
acceptParser :: Parser [(B.ByteString, Quality)]
acceptParser = ((,) <$> (skipSpace *> takeTill (inClass ";, "))
                    <*> (option 1.0 (skipSpace *> char ';' *> skipSpace *> string "q=" *> rational)))
               `sepBy` (skipSpace *> char ',' <* skipSpace)

-- TODO: Is it possible for a representation to be returned more than once with different qualities?
-- | Find all representations that match the client's Accept header.
matches :: [B.ByteString] -- ^ the available representations for this resource
        -> B.ByteString -- ^ the Accept header value from the client
        -> [(B.ByteString, Quality)] -- ^ a associative list of matches as (representation, quality) pairs (where representation is a member of the list of available representations).
matches available accept =
  case parseOnly acceptParser accept of
    Left _ -> [] -- TODO: Log an error?
    -- For now, only negotiate the content type.
    Right acceptable -> mapMaybe (`match` ordered) available
      where ordered = reverse $ sortBy (compare `on` snd) acceptable
            match :: B.ByteString -> [(B.ByteString, Quality)] -> Maybe (B.ByteString, Quality)
            match rep reps = case find (\(r, _) -> G.match (G.compile $ B8.unpack r) (B8.unpack rep)) reps of
                               Nothing -> Nothing
                               Just (_, q) -> Just (rep, q)

-- | Find the best matches (all of the highest quality) from a list of resource representation matches.
best :: [(B.ByteString, Quality)] -- ^ a list of valid matches as (representation, quality) pairs
     -> [B.ByteString] -- ^ the best matches from the list
best [] = []
best ms = let highest = snd $ maximumBy (compare `on` snd) ms in
              map fst [ x | x <- ms, snd x == highest ]
