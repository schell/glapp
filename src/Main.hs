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
import App.Types


data EditorWindow = EditorWindow { _editorKeysDown :: S.Set Key 
                                 , _editorBuffer :: String
                                 } deriving (Show, Eq, Ord)


newAppWindowThen :: (Window -> IO (UserApp a)) -> IO (Maybe (AppWindow a))
newAppWindowThen f = do
    mWin <- createWindow 800 800 "Editor" Nothing Nothing
    case mWin of
        Nothing  -> return Nothing
        Just win -> do
            s    <- f win
            mvar <- newMVar s

            setCharCallback win $ Just $ \_ c ->
                input mvar $ CharEvent c

            setWindowSizeCallback win $ Just $ \win' w h -> do
                input mvar $ WindowSizeEvent w h
                render mvar win' 

            setKeyCallback win $ Just $ \win' k i ks mod ->
                input mvar $ KeyEvent k i ks mod

            setMouseButtonCallback win $ Just $ \win' mb mbs mod ->
                input mvar $ MouseButtonEvent mb mbs mod

            setCursorPosCallback win $ Just $ \win' x y ->
                input mvar $ CursorMoveEvent x y

            setCursorEnterCallback win $ Just $ \win' cs ->
                input mvar $ CursorEnterEvent cs

            setScrollCallback win $ Just $ \win' x y ->
                input mvar $ ScrollEvent x y

            return $ Just $ AppWindow win mvar


input :: MVar (UserApp a) -> InputEvent -> IO ()
input mvar e = do
    uApp <- takeMVar mvar
    let datum = (uApp ^. userInput) (uApp ^. userData) e
    putMVar mvar $ uApp & userData .~ datum


render :: MVar (UserApp a) -> Window -> IO ()
render mvar win = do
    makeContextCurrent $ Just win
    uApp <- readMVar mvar
    clear [ColorBuffer, DepthBuffer]
    uApp ^. userRender $ uApp ^. userData
    swapBuffers win


appUserInput :: EditorWindow -> InputEvent -> EditorWindow
appUserInput (EditorWindow keys buff) (CharEvent c) = EditorWindow keys $ c:buff 
appUserInput (EditorWindow keys buff) (KeyEvent k _ ks mod)
    | ks == KeyState'Pressed = EditorWindow (S.insert k keys) buff
    | ks == KeyState'Released = EditorWindow (S.delete k keys) buff
appUserInput s _ = s


appUserRender :: EditorWindow -> IO ()
appUserRender = print 


appUserShouldClose :: EditorWindow -> Bool
appUserShouldClose (EditorWindow keys buff) = S.fromList [Key'Escape,Key'LeftSuper] `S.isSubsetOf` keys

main :: IO ()
main = do
    True <- Graphics.UI.GLFW.init
    mWS <- newAppWindowThen $ \win -> do
        setWindowPos win 100 100
        return UserApp { _userData = EditorWindow S.empty "" 
                       , _userShouldClose = appUserShouldClose 
                       , _userRender = appUserRender 
                       , _userInput = appUserInput
                       }

    when (isJust mWS) $ do
        let app = App { _appState = False
                      , _appWindows = [fromJust mWS]
                      }

        forever $ do
            pollEvents
            forM_ (app ^. appWindows) $ \(AppWindow win mvar) -> do
                render mvar win 
                uApp <- readMVar mvar
                when (uApp ^. userShouldClose $ uApp ^. userData)
                    exitSuccess

    --dir  <- fmap (++ "assets/") getCurrentDirectory
    --text <- initTextRenderer dir


