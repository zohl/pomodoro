{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad (when)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Char (toLower)

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (writeChan, newChan)
import System.Posix.Process (forkProcess)

import System.Directory (
    getAppUserDataDirectory
  , createDirectoryIfMissing
  , getAppUserDataDirectory)
import System.Posix.Files (fileExist, removeLink)

import Control.Exception.Base (handle, IOException)
import System.Environment (getArgs, withArgs)
import System.FilePath (joinPath)
import Text.Heredoc

import Common
import Worker
import IPC
import GUI
import Settings


main :: IO ()
main = getAppUserDataDirectory "pomodoro"
   >>= createDirectoryIfMissing True
   >>  getArgs
   >>= dispatch where
     dispatch []    = runApp

     dispatch (cmd:cmds) = case cmd of
       "--start"  -> case cmds of
         [name] -> sendMessage Work (Just name)
         []     -> sendMessage Work Nothing
         _      -> showUsage 

       "--stop"   -> sendMessage Inactive Nothing
       "--help"   -> showUsage
       _          -> showUsage

     showUsage = putStrLn $ [str|
     |Usage:
     |  pomodoro                 Run pomodoro
     |
     |  pomodoro --start [name]  Start the timer and (if specified) tag it with name
     |  pomodoro --stop          Stop the timer
     |                           (Both commands will run pomodoro if it is not already running.)
     |
     |  pomodoro --help          Show this message
     |
     |Configuration:
     |  Edit ~/.pomodoro/settings.conf for desired behaviour.
     |  Avaliable options are:
     |    workInterval        :: Int
     |    relaxInterval       :: Int
     |    longerRelaxInterval :: Int
     |    pomodorosPerRound   :: Int
     |    autoRestart         :: Bool
     |    timestampFormat     :: String
     |    enablePopups        :: Bool
     |    enableSounds        :: Bool
     |    askPomodoroName     :: Bool
     |]

withBaseDir :: FilePath -> IO FilePath
withBaseDir path = (joinPath . (:[path])) <$> getAppUserDataDirectory "pomodoro"


runApp :: IO ()
runApp = do
  [logPath, settingsPath, socketPath] <- sequence $ withBaseDir <$> [
      "activity.log"
    , "settings.conf"
    , "pomodoro.sock"
    ]

  settings <- fileExist settingsPath >>= (\exists -> case exists of 
    False -> do
      writeFile settingsPath $ show defaultSettings
      return $ defaultSettings
    True  -> parseSettings defaultSettings <$> readFile settingsPath)

  chan <- newChan
  _ <- forkIO $ runListener socketPath chan

  runGUI settings $ GUICallbacks {
      onIconClick = (writeChan chan) . (Message Work)
    , onMenuStart = (writeChan chan) . (Message Work)
    , onMenuStop  = writeChan chan $ Message Inactive Nothing

    , onInit = \cfs -> do
        _ <- forkIO $ runWorker chan settings $ WorkerCallbacks {
               onChangeStatus = \status -> do
                 (iconUpdate cfs) status
                 when (enablePopups settings) $
                   (popupNotification cfs) (map toLower $ show status) (interpret status)
                 when (enableSounds settings) $ (soundNotification cfs)

             , onPomodoroEnd = \n name startTime endTime -> appendFile logPath $ concat [
                   "Pomodoro #", (show n)
                 , maybe "" ((" (" ++) . (++ ")")) name
                 , ": "
                 , formatTime defaultTimeLocale (timestampFormat settings) startTime
                 , " -- "
                 , formatTime defaultTimeLocale (timestampFormat settings) endTime
                 , "\n"
                 ]
          }
        return ()
    }


sendMessage :: PomodoroStatus -> Maybe String -> IO ()
sendMessage st name = withBaseDir "pomodoro.sock" >>= sendMessage' 2 where
 
  sendMessage' :: Int -> FilePath -> IO ()
  sendMessage' 0 _    = fail "cannot connect to the application"
  sendMessage' n path = (flip handle)
    (sendToListener path (Message st name)) $ \(e::IOException) -> do
      putStrLn $ show e
      fileExist path >>= \exists -> when exists $ removeLink path
      _ <- forkProcess $ withArgs [] runApp
      waitUntilTimeout 2000000 100000 (fileExist path) >>= maybe
        (fail "cannot start application")
        (\_ -> sendMessage' (n-1) path)
    
  

