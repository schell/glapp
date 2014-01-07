{-# LANGUAGE TemplateHaskell #-}
module App.Types where

import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Lens
import Data.Maybe


data InputEvent = CharEvent Char
                | WindowSizeEvent Int Int
                | KeyEvent Key Int KeyState ModifierKeys
                | MouseButtonEvent MouseButton MouseButtonState ModifierKeys
                | CursorMoveEvent Double Double
                | CursorEnterEvent CursorState
                | ScrollEvent Double Double


data UserApp a = UserApp { _userData :: a
                         , _userShouldClose :: a -> Bool
                         , _userRender :: a -> IO ()
                         , _userInput :: a -> InputEvent -> a
                         }
makeLenses ''UserApp


data AppWindow a = AppWindow { _awWindow      :: Window
                             , _awUserAppMVar :: MVar (UserApp a)
                             }
makeLenses ''AppWindow


data App a b = App { _appState   :: a
                   , _appWindows :: [AppWindow b]
                   }
makeLenses ''App


