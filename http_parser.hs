import Network.Socket
import Text.Parsec
import Text.Parsec.String
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import System.IO
import Data.Maybe (fromMaybe)
import Control.Exception (catch, IOException)

data Method = GET | POST | Other String deriving Show
data Request = Request Method String [(String, String)] deriving Show

parseRequest :: Parser Request
parseRequest = do
  method <- parseMethod
  space
  uri <- manyTill anyChar (char ' ')
  string "HTTP/" >> manyTill anyChar (char '\r' <* char '\n')
  headers <- manyTill parseHeader (try (string "\r\n"))
  return $ Request method uri headers

parseMethod :: Parser Method
parseMethod = do
  m <- manyTill anyChar (lookAhead (char ' '))
  return $ case m of
    "GET" -> GET
    "POST" -> POST
    _ -> Other m

parseHeader :: Parser (String, String)
parseHeader = do
  name <- manyTill anyChar (char ':')
  space
  value <- manyTill anyChar (char '\r' <* char '\n')
  return (name, value)

parseHttpRequest :: String -> Either ParseError Request
parseHttpRequest = parse parseRequest ""

handleClient :: Socket -> IO ()
handleClient client = do
  h <- socketToHandle client ReadWriteMode
  hSetBuffering h LineBuffering

  requestData <- readUntilDoubleCRLF h
  putStrLn $ "Received from client: \n" ++ requestData

  case parseHttpRequest requestData of
    Left err -> do
      hPutStrLn h "HTTP/1.1 400 Bad Request"
      hPutStrLn h "Content-Type: text/plain"
      hPutStrLn h ""
      hPutStrLn h $ "Parse error: " ++ show err
    Right (Request method uri headers) -> do
      let responseBody = "Method: " ++ show method ++ "\nURI: " ++ uri ++
                         "\nHeaders: " ++ show headers
      hPutStrLn h "HTTP/1.1 200 OK"
      hPutStrLn h "Content-Type: text/plain"
      hPutStrLn h ("Content-Length: " ++ show (length responseBody))
      hPutStrLn h ""
      hPutStrLn h responseBody

  hClose h
  close client

readUntilDoubleCRLF :: Handle -> IO String
readUntilDoubleCRLF h = go ""
  where
    go acc = do
      eof <- hIsEOF h
      if eof
        then return acc
        else do
          line <- hGetLine h
          if line == ""
            then return acc
            else go (acc ++ line ++ "\r\n")

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 8080 0)
  listen sock 5
  putStrLn "Server listening on port 8080..."

  forever $ do
    (client, addr) <- accept sock
    putStrLn $ "Accepted connection from " ++ show addr
    forkIO $ handleClient client `catch` (\e -> do
        putStrLn $ "Error handling client " ++ show addr ++ ": " ++ show (e :: IOException)
        close client)
