module Main where

import Control.Monad.Reader
import Control.Monad.Except
import Debug.Trace
import Text.Printf

data Config = Config { port :: Int
                     , host :: String
                     } deriving (Show)

data Error = Invalid String | NotFound deriving (Show)

type App a = ReaderT Config (ExceptT Error IO) a



testFn :: Int -> App String
testFn 0 = do
  p <- asks port
  h <- asks host
  throwError $ Invalid "0 not supported"
  return $ printf "%s:%d" h p

testFn _ = do
  p <- asks port
  h <- asks host
  return $ printf "%s:%d" h p


main :: IO ()
main = do 
  s1 <- runExceptT $ runReaderT (testFn 0) conf
  putStrLn (show s1)
  s2 <- runExceptT $ runReaderT (testFn 1) conf
  putStrLn (show s2)

  where
    conf = Config 123 "localhost"
  