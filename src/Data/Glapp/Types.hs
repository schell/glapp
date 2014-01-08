{-# LANGUAGE TemplateHaskell #-}
module Data.Glapp.Types where

import Graphics.UI.GLFW
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Lens
import Data.Maybe


data Id = Id { _unId :: Int } deriving (Show, Eq, Ord)

instance Enum Id where
    toEnum = Id
    fromEnum = _unId


data InputEvent = CharEvent Char
                | WindowSizeEvent Int Int
                | KeyEvent Key Int KeyState ModifierKeys
                | MouseButtonEvent MouseButton MouseButtonState ModifierKeys
                | CursorMoveEvent Double Double
                | CursorEnterEvent CursorState
                | ScrollEvent Double Double


data UserWindow a = UserWindow { _windowData :: a
                               --, _windowShouldClose :: a -> Bool
                               , _windowRender :: a -> IO ()
                               , _windowStep :: a -> IO a
                               , _windowInput :: a -> InputEvent -> a
                               }
makeLenses ''UserWindow


type UserWindowId a = (Id, UserWindow a)


data UserWindowConfig = UserWindowConfig { _windowSize  :: (Int, Int)
                                         , _windowPos   :: (Int, Int)
                                         , _windowTitle :: String
                                         } deriving (Show, Eq, Ord)


type NewWindow a = (UserWindowConfig, UserWindow a)


data UserApp a b = UserApp { _userAppData       :: a
                           , _userAppShouldQuit :: a -> Bool
                           , _userAppStep       :: a -> [UserWindow b] -> IO a
                           , _userAppNewWindows :: a -> [NewWindow b]
                           }
makeLenses ''UserApp

