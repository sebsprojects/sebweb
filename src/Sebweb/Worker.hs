module Sebweb.Worker (
  dailyWorker
) where

import Control.Monad
import Control.Concurrent
import Data.Time
import qualified Data.Text as T

import Sebweb.Log
import Sebweb.LogI

dailyWorker :: ILogQueue -> T.Text -> Int -> IO () -> IO ()
dailyWorker ilq name startt task = do
  now <- getCurrentTime
  logEnqueue ilq $ ILogData now ILInfo ITWorker $
    name <> ": init; scheduled first task in " <>
    (T.pack $ show $ startt + secondsUntilMidnight now) <> " seconds"
  threadDelayUntilSecAfterMidnight (startt * 1000000)
  _ <- forever $ do
    task
    threadDelayUntilSecAfterMidnight startt
  return ()

threadDelayUntilSecAfterMidnight :: Int -> IO ()
threadDelayUntilSecAfterMidnight s = do
  now <- getCurrentTime
  threadDelay $ (s + secondsUntilMidnight now) * 1000000

secondsUntilMidnight :: UTCTime -> Int
secondsUntilMidnight now = floor $ 60 * 60 * 24 - utctDayTime now
