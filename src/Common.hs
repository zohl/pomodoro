{-# LANGUAGE DeriveGeneric #-}

module Common (
    PomodoroStatus(Work, Relax, Inactive)
  , Message(..)
  , interpret
  , waitUntil
  , waitUntilTimeout
  ) where

import Control.Concurrent (threadDelay)
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Control.Monad (when)


data PomodoroStatus = Work | Relax | Inactive
  deriving (Show, Eq, Generic)
instance Serialize PomodoroStatus


data Message = Message PomodoroStatus (Maybe String)
  deriving (Show, Eq, Generic)

instance Serialize Message

interpret :: PomodoroStatus -> String
interpret Work     = "Stay focused!"
interpret Relax    = "Take a break"
interpret Inactive = "The timer is inactive"


waitUntil :: Int -> IO Bool -> IO ()
waitUntil d f = f >>= \success -> when (not success) $ (threadDelay d >> waitUntil d f)
  
waitUntilTimeout :: Int -> Int -> IO Bool -> IO (Maybe ())
waitUntilTimeout t d f
  | t < 0     = return $ Nothing
  | otherwise = f >>= \success -> case success of
      True  -> return $ Just ()
      False -> threadDelay d >> waitUntilTimeout (t-d) d f


