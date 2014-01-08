{-# LANGUAGE TemplateHaskell #-}
module Data.App.Internal where

import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Lens
import Data.Maybe
import Data.App.Types
import qualified Data.Set as S


data AppWindow a = AppWindow { _appWindowId       :: Id
                             , _appWindow         :: Window
                             , _appUserWindowMVar :: MVar (UserWindow a)
                             } deriving (Eq)
makeLenses ''AppWindow


instance Ord (AppWindow a) where
    compare (AppWindow a _ _) (AppWindow b _ _) = compare a b


data App a b = App { _userApp      :: UserApp a b
                   , _windows      :: S.Set (AppWindow b)
                   , _nextWindowId :: Id
                   }
makeLenses ''App

