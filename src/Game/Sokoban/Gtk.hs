{-# LANGUAGE OverloadedStrings #-}
module Game.Sokoban.Gtk where

import Game.Sokoban
import Game.Sokoban.Levels

import Control.Monad.State (MonadIO(..), execStateT, foldM, forM_, gets, when)
import Data.IORef
import qualified Data.Map as M (Map, (!), empty, fromList, insert)
import Data.Sequence
import Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import Linear.V2 (V2(..))

data GameState
   = GameState
   { stateWorld :: World
   , stateUndo :: Seq World
   }

mainWith :: Word -> IO ()
mainWith idx =
    do initGUI
       let initialGameState = GameState (loadWorld $ levels !! fromIntegral (idx - 1)) empty
       state <- newIORef initialGameState
       tiles <- loadTiles
           [ "character-boy"
           , "gem-blue"
           , "grass-block"
           , "plain-block"
           , "selector"
           , "stone-block"
           , "stone-block-tall"
           ]
       window <- windowNew
       window `on` sizeRequest $ return (Requisition 800 600)
       window `on` keyPressEvent $ handleKeyboard window state initialGameState
       window `on` exposeEvent $ drawScene window state tiles
       onDestroy window mainQuit
       widgetShowAll window
       mainGUI

handleKeyboard :: (WidgetClass window) => window -> IORef GameState -> GameState -> EventM EKey Bool
handleKeyboard window state initialGameState =
    tryEvent $
    do kv <- liftIO . keyvalName =<< eventKeyVal
       liftIO $
           do case kv of
                "u" -> undo state
                "r" -> writeIORef state initialGameState
                "q" ->
                    do st <- readIORef state
                       checkQuit <- messageDialogNew Nothing [DialogModal] MessageWarning ButtonsYesNo
                           ("Do you really want to quit?" :: String)
                       quitResponse <- dialogRun checkQuit
                       when (quitResponse == ResponseYes) $
                           do alert <- messageDialogNew Nothing [DialogModal] MessageInfo ButtonsOk $
                                  "You quit after " ++ show (steps $ stateWorld st) ++ " steps."
                              ResponseOk <- dialogRun alert
                              mainQuit
                "Left" -> updateWorld state left
                "Right" -> updateWorld state right
                "Down" -> updateWorld state down
                "Up" -> updateWorld state up
                _ -> return ()
              widgetQueueDraw window


drawScene :: (WidgetClass window, MonadIO m) => window -> IORef GameState -> M.Map String Surface -> m Bool
drawScene window state tiles =
    liftIO $
    do cr <- widgetGetDrawWindow window
       GameState world _ <- readIORef state
       renderWithDrawable cr $
           do let imgWorker = tiles M.! "character-boy"
                  imgWall = tiles M.! "stone-block"
                  imgStorage = tiles M.! "selector"
                  imgCrate = tiles M.! "gem-blue"
              C.scale 0.4 0.4
              drawImage imgWorker (worker world)
              forM_ (walls world) $ drawImage imgWall
              forM_ (storage world)  $ drawImage imgStorage
              forM_ (crates world) $ drawImage imgCrate
       -- stepLabel <- labelNew (Nothing :: Maybe String)
       -- labelSetText stepLabel $ show (steps world)
       return True
    where
      drawImage img offset =
           -- tiles are 100x85
          do Requisition width height <- liftIO $ widgetSizeRequest window
             let centerPoint (V2 x y) =
                     ( fromIntegral x * 100 + 0.125 * fromIntegral width
                     , 1.5 * fromIntegral height - fromIntegral y * 85
                     )
             uncurry (C.setSourceSurface img) $ centerPoint offset
             C.paint

loadTiles :: [String] -> IO (M.Map String Surface)
loadTiles = foldM addSurface M.empty
    where
      addSurface kvs path =
          do surface <- C.imageSurfaceCreateFromPNG ("img/" ++ path ++ ".png")
             return $ M.insert path surface kvs

updateWorld :: IORef GameState -> Move -> IO ()
updateWorld state move =
    do GameState wld prev <- readIORef state
       wld' <- flip execStateT wld $
           do applyMove move
              finished <- isFinished
              when finished $ gets steps >>= \step -> liftIO $
                  do alert <- messageDialogNew Nothing [DialogModal] MessageInfo ButtonsOk $
                         "Congratulations!\n\nYou won in " ++ show step ++ " steps."
                     ResponseOk <- dialogRun alert
                     mainQuit
       modifyIORef' state $ \s -> s { stateWorld = wld', stateUndo = wld <| prev }

undo :: IORef GameState -> IO ()
undo state =
    modifyIORef' state $ \s@(GameState wld prev) ->
        case viewl prev of
          EmptyL -> s
          (w :< ws) -> GameState w ws
