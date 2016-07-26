{-# LANGUAGE OverloadedStrings #-}
module Game.Sokoban.Gtk where

import Game.Sokoban
import Game.Sokoban.Levels

import Control.Monad.State (MonadIO(..), execStateT, foldM, forM_, gets, when)
import Control.Concurrent.MVar
import Data.Map (Map, (!), empty, fromList, insert)
import Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import Linear.V2 (V2(..))

initialGameState =
    loadWorld $ level 1

main :: IO ()
main =
    do initGUI
       state <- newMVar initialGameState
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
       window `on` keyPressEvent $ handleKeyboard window state
       window `on` exposeEvent $ drawScene window state tiles
       onDestroy window mainQuit
       widgetShowAll window
       mainGUI

handleKeyboard :: (WidgetClass window) => window -> MVar World -> EventM EKey Bool
handleKeyboard window state =
    tryEvent $
    do kv <- liftIO . keyvalName =<< eventKeyVal
       liftIO $
           do updateWorld state $ case kv of
                "Left" -> Just left
                "Right" -> Just right
                "Down" -> Just down
                "Up" -> Just up
                _ -> Nothing
              widgetQueueDraw window

drawScene :: (WidgetClass window, MonadIO m) => window -> MVar World -> Map String Surface -> m Bool
drawScene window state tiles =
    liftIO $
    do cr <- widgetGetDrawWindow window
       world <- readMVar state
       renderWithDrawable cr $
           do let imgWorker = tiles ! "character-boy"
                  imgWall = tiles ! "stone-block"
                  imgStorage = tiles ! "selector"
                  imgCrate = tiles ! "gem-blue"
              C.scale 0.4 0.4
              drawImage imgWorker (worker world)
              forM_ (walls world) $ drawImage imgWall
              forM_ (storage world)  $ drawImage imgStorage
              forM_ (crates world) $ drawImage imgCrate
       return True
    where
      drawImage img (V2 x y) =
           -- tiles are 100x85
          do Requisition width height <- liftIO $ widgetSizeRequest window
             C.setSourceSurface img (fromIntegral x * 100 + 0.5*fromIntegral width) (1.5*fromIntegral height - fromIntegral y * 85)
             C.paint

loadTiles :: [String] -> IO (Map String Surface)
loadTiles = foldM addSurface empty
    where
      addSurface kvs path =
          do surface <- C.imageSurfaceCreateFromPNG ("img/" ++ path ++ ".png")
             return $ insert path surface kvs

updateWorld :: MVar World -> Maybe Move -> IO ()
updateWorld state move =
    do wld <- readMVar state
       wld' <- flip execStateT wld $
           do mapM_ applyMove move
              finished <- isFinished
              when finished $ gets steps >>= \step -> liftIO $
                  do alert <- messageDialogNew Nothing [DialogModal] MessageInfo ButtonsOk $ "Congratulations!\n\nYou won in " ++ show step ++ " steps."
                     ResponseOk <- dialogRun alert
                     mainQuit
       modifyMVar_ state $ \s -> return wld'



