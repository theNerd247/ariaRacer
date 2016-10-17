{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TQueue
import Data.Array
import Control.Monad

import Thread.Pool

app :: Int -> IO ()
app n = do
  threadDelay 500
  putStrLn $! "Foo: " ++ (show n)
  threadDelay 500

main :: IO ()
main = do
  putStrLn "Starting pool"
  p <- startPool 3 :: IO (ThreadPool ())
  let runApp n = addJob p (app n)
  {-let js = jobs p-}
  forM_ [1..1000] runApp
  putStrLn "Threads started"
  getResult p
  forever $ return ()
