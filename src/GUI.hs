{-# LANGUAGE TupleSections #-}

module GUI (
    runGUI
  , GUICallbacks(..)
  , GUICallforths(..)
  ) where

import Graphics.UI.WX hiding (sound)
import Graphics.UI.WXCore

import Data.Maybe (fromJust)
import Libnotify ((<>), summary, body, timeout, Timeout(..), display_)
import System.Directory (findExecutable)
import System.Environment (getExecutablePath)
import System.FilePath (normalise, joinPath, takeDirectory)
import System.Process (spawnProcess)

import Common
import Settings
import Paths_pomodoro


data Resources = Resources {
    icons        :: [(PomodoroStatus, Icon ())]
  }

initResources :: IO Resources
initResources =
  -- Get singular icon
  let getIcon status = getDataFileName ("res/" ++ show status ++ ".png")
                       >>= flip iconCreateFromFile sizeNull
                       >>= \i -> return (status, i)
  in do icons <- traverse getIcon [Work, Relax, Inactive]
        return $ Resources icons

setIcon :: Resources -> TaskBarIcon a -> PomodoroStatus -> IO ()
setIcon res tbi status' = do
  _ <- taskBarIconSetIcon tbi (fromJust $ lookup status' (icons res)) (show status')
  return ()


data GUICallforths = GUICallforths {
    iconUpdate        :: PomodoroStatus -> IO ()
  , popupNotification :: String -> String -> IO ()
  , soundNotification :: IO ()
  }

data GUICallbacks = GUICallbacks {
    onIconClick :: Maybe String -> IO ()
  , onMenuStart :: Maybe String -> IO ()
  , onMenuStop  :: IO ()
  , onInit      :: GUICallforths -> IO ()
  }
  

showPopup :: String -> String -> IO ()
showPopup summary' body' = display_ $
     (summary $ "Pomodoro: " ++ summary')
  <> (body body')
  <> (timeout $ Custom 8) 


playSound :: String -> IO ()
playSound sound = findExecutable "mpg321" >>= maybe
  (putStrLn "cannot find mpg321")
  (\player -> spawnProcess player [sound] >> return ())
  

runGUI :: Settings -> GUICallbacks -> IO () 
runGUI settings cbs = start $ initGUI where

  initGUI :: IO ()
  initGUI = do
    base <- getDataDir

    res  <- initResources
    tbi  <- taskBarIconCreate

    setIcon res tbi Inactive

    (dialogFrame, withNameDialog) <- initNameDialog
    evtHandlerOnTaskBarIconEvent tbi (onTaskBarEvt tbi dialogFrame withNameDialog)
    
    onInit cbs $ GUICallforths {
        iconUpdate        = setIcon res tbi
      , popupNotification = showPopup
      , soundNotification = playSound (base++"/Bell.mp3")
    }


  initNameDialog :: IO (Frame (), (Maybe String -> IO ()) -> IO ())
  initNameDialog = do
    f <- frameEx (wxCAPTION .-. wxSTAY_ON_TOP) [
        visible := False
      , text := "Enter pomodoro name"
      ] objectNull

    ok <- button f [text := "OK"]

    cancel <- button f [text := "Cancel", on command := do
                           putStrLn "Cancel"
                           set f [visible := False]]

    txt <- textEntry f [ text := "" ]
    
    set f [layout := floatCenter $ margin 10 $ hfill $ column 5 $ [
                  hfill $ widget txt
                , hfill $ row 5 $ [hspace 100, floatRight $ widget cancel, widget ok]]]

    return $ (f,) $ \p -> case (askPomodoroName settings) of
      False -> p $ Nothing
      True  -> do
        set ok [on command := do
                       name <- get txt text 
                       set f [visible := False]
                       p $ case (length name) of
                         0 -> Nothing
                         _ -> Just name]
        set f [visible := True]


  onTaskBarEvt :: TaskBarIcon a -> Frame b -> ((Maybe String -> IO ()) -> IO ()) -> EventTaskBarIcon -> IO ()
  onTaskBarEvt _   _ cb TaskBarIconLeftDown  = cb $ onIconClick cbs
  onTaskBarEvt tbi f cb TaskBarIconRightDown = do
    popupMenu <- menuPane []
    _ <- menuItem popupMenu [text := "Start", on command := cb $ onMenuStart cbs] 
    _ <- menuItem popupMenu [text := "Stop" , on command := onMenuStop cbs]
    _ <- menuItem popupMenu [text := "Exit" , on command := taskBarIconDelete tbi >> close f]
    _ <- taskBarIconPopupMenu tbi popupMenu
    return ()
  
  onTaskBarEvt _ _ _ _ = return ()



  
