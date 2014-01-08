module Main where

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
import Example.Editor

main :: IO ()
main = iterateApp myApp

    --dir  <- fmap (++ "assets/") getCurrentDirectory
    --text <- initTextRenderer dir

