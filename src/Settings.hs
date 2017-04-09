module Settings (
    Settings(..)
  , defaultSettings
  , parseSettings
  ) where

-- | Representation of ~\/.pomodoro\/settings.conf file
data Settings = Settings {
    workInterval        :: Int
    -- ^ Duration of work interval (in seconds)

  , relaxInterval       :: Int
    -- ^ Duration of relax interval (in seconds)

  , longerRelaxInterval :: Int
    -- ^ Duration of relax interval after each round (in seconds)
                           
  , pomodorosPerRound   :: Int
    -- ^ How many intervals in a round                       

  , autoRestart         :: Bool
    -- ^ If True, will automatically start new interval

  , timestampFormat     :: String
    -- ^ Time format as in 'Data.Time.Format.formatTime'

  , enablePopups        :: Bool
    -- ^ If True, will show popup messages
                           
  , enableSounds        :: Bool
    -- ^ If True, will play sound every time the status changes

  , askPomodoroName     :: Bool
    -- ^ If True, will ask a name for a new interval 
    -- (this will not affect command line interface).

  , soundProgram        :: String
    -- ^ Name of the program that will play the "bell" sound on each
    -- pomodoro, defaults to mpg321.
  } deriving (Eq)


defaultSettings :: Settings
defaultSettings = Settings {
    workInterval        = 25 * 60
  , relaxInterval       = 5  * 60 
  , longerRelaxInterval = 15 * 60 
  , pomodorosPerRound   = 4
  , autoRestart         = False
  , timestampFormat     = "[%0Y-%m-%d %H:%M:%S]"
  , enablePopups        = True
  , enableSounds        = False
  , askPomodoroName     = False
  , soundProgram        = "mpg321"
  }


instance Show Settings where
  show settings = unlines [
      formatField "workInterval"        workInterval
    , formatField "relaxInterval"       relaxInterval
    , formatField "longerRelaxInterval" longerRelaxInterval
    , formatField "pomodorosPerRound"   pomodorosPerRound
    , formatField "autoRestart"         autoRestart
    , formatField "timestampFormat"     timestampFormat
    , formatField "enablePopups"        enablePopups
    , formatField "enableSounds"        enableSounds
    , formatField "askPomodoroName"     askPomodoroName
    , formatField "soundProgram"        soundProgram
    ] where
        formatField :: (Show a) => String -> (Settings -> a) -> String
        formatField key value = key ++ " = " ++ (show $ value settings)


parseSettings :: Settings -> String -> Settings
parseSettings defaults contents = foldl parseLine defaults configLines where

  configLines = filter ((> 0) . length) (lines contents)

  trim :: String -> String
  trim = trim' . trim' where trim' = reverse . (dropWhile (== ' '))

  liftT2 :: (a -> b) -> (a, a) -> (b, b)
  liftT2 f (x, y) = (f x, f y)

  parseLine :: Settings -> String -> Settings
  parseLine defaults' line = let
    (key, value) = liftT2 (trim . drop 1) $ break (== '=') (' ':line)
    in case key of
      "workInterval"        -> defaults' { workInterval        = (read value :: Int) }
      "relaxInterval"       -> defaults' { relaxInterval       = (read value :: Int) }
      "longerRelaxInterval" -> defaults' { longerRelaxInterval = (read value :: Int) }
      "pomodorosPerRound"   -> defaults' { pomodorosPerRound   = (read value :: Int) }
      "autoRestart"         -> defaults' { autoRestart         = (read value :: Bool) }
      "timestampFormat"     -> defaults' { timestampFormat     = (read value :: String) }
      "enablePopups"        -> defaults' { enablePopups        = (read value :: Bool) }
      "enableSounds"        -> defaults' { enableSounds        = (read value :: Bool) }
      "askPomodoroName"     -> defaults' { askPomodoroName     = (read value :: Bool) }
      "soundProgram"        -> defaults' { soundProgram        = (read value :: String) }
      _                     -> defaults'


