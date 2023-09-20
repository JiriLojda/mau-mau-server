module Main (main) where

import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import Server (runMauMauServer)
import System.IO (stdout)

ip :: String
ip = "127.0.0.1"

port :: Int
port = 9160

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn ("Starting Mau Mau server on: " ++ ip ++ ":" ++ show port)
  runMauMauServer "127.0.0.1" 9160
