module Worker (
    runWorker
  , WorkerCallbacks(..)
  ) where


import Control.Concurrent.Chan (Chan, readChan, writeChan)
import Control.Concurrent (ThreadId, killThread, forkIO, threadDelay)
import Control.Monad (when)
import Data.Time.Clock (UTCTime, getCurrentTime)

import Common
import Settings

       
data State = State {
    status    :: PomodoroStatus
  , pomodoros :: Int
  , tid       :: Maybe ThreadId
  , startTime :: Maybe UTCTime
  , name     :: Maybe String
  }
 
data WorkerCallbacks = WorkerCallbacks {
    onChangeStatus :: PomodoroStatus -> IO ()
  , onPomodoroEnd  :: Int -> Maybe String -> UTCTime -> UTCTime -> IO ()
  } 

runWorker :: Chan Message -> Settings -> WorkerCallbacks  -> IO ()
runWorker chan settings cbs = readLoop $ State {
    status    = Inactive
  , pomodoros = 0
  , tid       = Nothing
  , startTime = Nothing
  , name     = Nothing
  } where

  readLoop :: State -> IO ()
  readLoop state = readChan chan >>= updateState state >>= readLoop

  updateState :: State -> Message -> IO State
  updateState state (Message status'' name'') = case ((status state) == status'') of
    True  -> return state
    False -> do
      putStrLn $ (show $ status state) ++ " -> " ++ (show status'')
      maybe (return ()) killThread (tid state)

      onChangeStatus cbs status''

      when ((status state, status'') == (Work, Relax)) $ ($ startTime state) $ maybe
          (return ())
          (\t -> getCurrentTime >>= (onPomodoroEnd cbs (pomodoros state) (name state) t))

      let status' = status''

      let pomodoros' = case (status state, status') of
                         (Work, Relax) -> (pomodoros state) + 1
                         (_   , _)     -> (pomodoros state)

      tid' <- case status' of
        Work -> Just <$> (forkIO $ statusDelay (workInterval settings) Relax)

        Relax -> let
          nextInterval = ($ settings) $ case pomodoros' `mod` (pomodorosPerRound settings) of
                                     0 -> longerRelaxInterval
                                     _ -> relaxInterval
          nextStatus = case (autoRestart settings) of
                         True  -> Work
                         False -> Inactive
          in Just <$> (forkIO $ statusDelay nextInterval nextStatus)

        Inactive -> return Nothing

      startTime' <- case (status state, status') of
        (_   , Work)     -> Just <$> getCurrentTime
        (_   , Inactive) -> return $ Nothing
        (_   , _)        -> return $ startTime state

      let name' = case (status state, status') of
                     (_, Work) -> name''
                     (_, _)    -> name state

      return $ State {
          status    = status''
        , pomodoros = pomodoros' 
        , tid       = tid'
        , startTime = startTime'
        , name     = name'
        }

  statusDelay m s = threadDelay (m * 1000000) >> writeChan chan (Message s Nothing)

