{-# LANGUAGE TemplateHaskell #-}
module Example.Editor where

import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Lens
import Data.Maybe
import System.Exit
import qualified Data.Set as S
import Data.Glapp


data Editor = Editor { _editorWindowCount :: Int
                     , _editorKeys        :: S.Set Key
                     } deriving (Show)
makeLenses ''Editor


data EditorWindow = EditorWindow { _windowKeys :: S.Set Key
                                 , _windowBuffer :: String
                                 } deriving (Show, Eq, Ord)
makeLenses ''EditorWindow


myWindowInput :: EditorWindow -> InputEvent -> EditorWindow
myWindowInput (EditorWindow keys buff) (CharEvent c) = EditorWindow keys $ c:buff
myWindowInput (EditorWindow keys buff) (KeyEvent k _ ks mod)
    | ks == KeyState'Pressed = EditorWindow (S.insert k keys) buff
    | ks == KeyState'Released = EditorWindow (S.delete k keys) buff
myWindowInput s _ = s


myWindowRender :: EditorWindow -> IO ()
myWindowRender = const $ return ()


myWindowStep :: EditorWindow -> IO EditorWindow
myWindowStep = return


myWindow :: UserWindow EditorWindow
myWindow = UserWindow { _windowData = EditorWindow S.empty ""
                      , _windowRender = myWindowRender
                      , _windowInput = myWindowInput
                      , _windowStep = myWindowStep
                      }


myAppStep :: Editor -> [UserWindow EditorWindow] -> IO Editor
myAppStep e ws = do
    print e
    return $ flip execState e $ do
        -- Gather all the keys down on the windows.
        editorKeys .= foldl (\acc w -> acc `S.union` (w^.windowData.windowKeys)) S.empty ws
        -- Update the window count to the number of user windows.
        editorWindowCount .= length ws


myNewWindows :: Editor -> [NewWindow EditorWindow]
myNewWindows (Editor 0 _) = [(UserWindowConfig (800,800) (100,100) "Editor0", myWindow)]
myNewWindows (Editor i keys) =
    if S.fromList [Key'LeftSuper,Key'N] `S.isSubsetOf` keys
      then [(UserWindowConfig (800,800) (100,100) ("Editor" ++ show i), myWindow)]
      else []
myNewWindows _ = []


shouldMyAppQuit :: Editor -> Bool
shouldMyAppQuit (Editor 0 _) = True
shouldMyAppQuit _ = False


myApp :: UserApp Editor EditorWindow
myApp = UserApp { _userAppData = Editor 0 S.empty
                , _userAppStep = myAppStep
                , _userAppShouldQuit = shouldMyAppQuit
                , _userAppNewWindows = myNewWindows
                }


