module Data.App (
    module T,
    iterateApp
) where

import           Graphics.UI.GLFW
import           Graphics.Rendering.OpenGL
import           Control.Monad
import           Control.Monad.State
import           Control.Concurrent
import           Control.Lens
import           Data.Maybe
import           System.Exit
import           Data.App.Types as T
import           Data.App.Internal
import qualified Data.Set as S
import Debug.Trace



input :: MVar (UserWindow a) -> InputEvent -> IO ()
input mvar e = do
    uApp <- takeMVar mvar
    let datum = (uApp ^. windowInput) (uApp ^. windowData) e
    putMVar mvar $ uApp & windowData .~ datum


render :: MVar (UserWindow a) -> Window -> IO ()
render mvar win = do
    makeContextCurrent $ Just win
    uWin <- readMVar mvar
    clear [ColorBuffer, DepthBuffer]
    uWin ^. windowRender $ uWin ^. windowData
    swapBuffers win
    makeContextCurrent Nothing


iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v
    | p v = return v
    | otherwise = f v >>= iterateUntilM p f


getUserAppShouldQuit :: App a b -> Bool
getUserAppShouldQuit app = shouldQuit getUserApp
    where shouldQuit = app ^. userApp . userAppShouldQuit
          getUserApp = app ^. userApp . userAppData


getUserWindowIds :: App a b -> IO [UserWindowId b]
getUserWindowIds app = do
    uWins <- forM (map _appUserWindowMVar $ S.toList $ app ^. windows) readMVar
    let ids = S.map _appWindowId (app ^. windows)
    return $ zip (S.toList ids) uWins


stepWindows :: App a b -> IO (App a b)
stepWindows app = do
    forM_ (S.toList $ app ^. windows) $ \(AppWindow i win mvar) -> do
        uWin  <- takeMVar mvar
        udata <- uWin ^. windowStep $ uWin ^. windowData
        putMVar mvar $ uWin & windowData .~ udata
    return app


makeAppWindow :: Id -> NewWindow a -> IO (Maybe (AppWindow a))
makeAppWindow id' (UserWindowConfig s p t, uWin) = do
    mWin <- (uncurry createWindow) s t Nothing Nothing
    case mWin of
       Nothing  -> return Nothing
       Just win -> do
            (uncurry $ setWindowPos win) p

            mvar <- newMVar uWin

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

            return $ Just $ AppWindow id' win mvar


createWindows :: Show a => App a b -> IO (App a b)
createWindows app = do
    -- Get our user windows.
    uWinIds <- getUserWindowIds app

    -- Get the config for our windows.
    let uWins      = map snd uWinIds
        news       = app ^. userApp.userAppNewWindows $ app ^. userApp.userAppData
        ids        = take (length news) $ iterate succ $ app ^. nextWindowId
        zipped     = zip ids news

    -- Create the windows.
    appWins <- fmap catMaybes $ forM zipped (uncurry makeAppWindow)

    return $ (flip execState) app $
        when (length appWins > 0) $
            do nextWindowId .= (succ $ last ids)
               windows %= (S.union $ S.fromList appWins)


destroyWindows :: App a b -> IO (App a b)
destroyWindows app = do
    -- Run through all the windows and find which ones should
    -- close.
    uWins <- getUserWindowIds app
    closedAppWins <- fmap catMaybes $ forM (S.toList $ app ^. windows) $
        \a@(AppWindow _ win _) -> do
            shouldClose <- windowShouldClose win
            if shouldClose then do
                -- Close them and return them.
                destroyWindow win
                return $ Just a
              else return Nothing

    -- Remove them from our list.
    return $ app & windows %~ (`S.difference` S.fromList closedAppWins)


renderWindows :: App a b -> IO (App a b)
renderWindows app = do
    -- Run through all the windows and step/render them.
    forM_ (S.toList $ app ^. windows) $ \(AppWindow _ win mvar) ->
        render mvar win
    return app


stepUserApp :: App a b -> IO (App a b)
stepUserApp app = do
    let uApp  = app ^. userApp
    uWins <- fmap (map snd) $ getUserWindowIds app
    uad <- (uApp ^. userAppStep) (uApp ^. userAppData) uWins
    return $ app & userApp.userAppData .~ uad


stepApp :: Show a => App a b -> IO (App a b)
stepApp a = do
    pollEvents
    createWindows a >>= stepWindows >>= stepUserApp >>= destroyWindows >>= renderWindows


iterateApp :: Show a => UserApp a b -> IO ()
iterateApp u = do
    True <- Graphics.UI.GLFW.init
    let app = App { _userApp = u
                  , _windows = S.empty
                  , _nextWindowId = Id 0
                  }

    stepApp app >>= iterateUntilM getUserAppShouldQuit stepApp
    return ()


