{-# LANGUAGE GeneralizedNewtypeDeriving, ImplicitParams, TypeFamilies #-}

import Quadtree

import Data.Decimal
import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Char(toLower, toUpper)
import Data.List(intercalate)

import Control.Concurrent
import Control.Exception
import Network hiding(Service)
import qualified Data.ByteString.Char8 as B
import System.IO

import Debug.Trace -- XXX

class Server s where
  type Request s
  type Response s

  toRequest :: s -> B.ByteString -> Request s
  fromResponse :: s -> Response s -> B.ByteString

  worker :: s -> Request s -> IO (Response s)

newtype Service = Service ()

type Label = String

data ServiceRequest = Put Point Label
                    | Retrieve Bounds
                    | Delete Label
                    | Unrecognized

data ServiceResponse = Ok
                     | Error String
                     | Table [(Point, Label)]

instance Server Service where
  type Request Service = ServiceRequest
  type Response Service = ServiceResponse

  toRequest _ str = case parse requestParser "" (B.unpack str) of
                      Left err -> Unrecognized
                      Right req -> req
    where requestParser =
            let decimal :: Parser Decimal
                decimal = do integerPart <- many1 digit
                             fractionalPart <- option "" $
                                                 do char '.'
                                                    liftM ((:) '.') $ many1 digit
                             return $ read (integerPart ++ fractionalPart)
                decimalList = decimal `sepBy` (spaces >> char ',' >> spaces)
                caseInsensitiveString =
                  liftM (map toUpper) . sequence . map upperOrLower
                  where upperOrLower c = char (toLower c) <|> char (toUpper c)
                command name body = caseInsensitiveString name >> spaces >> body
                label :: Parser Label
                label = many (alphaNum <|> char '_')

                put, retrieve, delete :: Parser ServiceRequest
                put = command "PUT" $
                        do string "("
                           [x, y] <- decimalList
                           string ")"
                           spaces
                           l <- label
                           return $ Put (x, y) l
                retrieve = command "RETRIEVE" $
                             do string "{"
                                [x, y, width, height] <- decimalList
                                string "}"
                                return $ Retrieve ((x, y), (width, height))
                delete = command "DELETE" $ liftM Delete label
            in choice [put, retrieve, delete]

  fromResponse _ Ok = B.pack "OK"
  fromResponse _ (Error str) = B.pack $ "ERROR " ++ str
  -- TODO: use bytestring primitives here to increase performance
  fromResponse _ (Table entries) = B.pack $ intercalate "\n" $ map showEntry entries
    where showEntry ((x, y), l) = intercalate "\t" [show x, show y, l]
  
  worker s _ = return Ok

server :: Server s => s -> Int -> IO ()
server srv port =
  do sock <- listenOn (PortNumber $ fromIntegral port)
     (forever $ loop sock) `finally` sClose sock
  where loop :: Socket -> IO ThreadId
        loop sock = do (h, _, _) <- accept sock
                       forkIO $ do hSetBuffering h NoBuffering
                                   (forever $ handle h) `finally` hClose h
        handle :: Handle -> IO ()
        handle h = do str <- B.hGetLine h
                      res <- worker srv (toRequest srv str)
                      B.hPutStrLn h (fromResponse srv res)

main :: IO ()
main = server (Service ()) 8000

