module Main (main) where

import Server (runMauMauServer)

ip :: String
ip = "127.0.0.1"

port :: Int
port = 9160

main :: IO ()
main = do
  putStrLn ("Starting Mau Mau server on: " ++ ip ++ ":" ++ show port)
  runMauMauServer "127.0.0.1" 9160
