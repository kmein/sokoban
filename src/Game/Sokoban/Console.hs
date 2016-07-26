{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Game.Sokoban.Console where

import Game.Sokoban
import Game.Sokoban.Levels (level)

import Control.Monad.State (evalStateT, get, gets, liftIO, unless)
import System.IO (BufferMode(NoBuffering), hSetBuffering, hSetEcho, stdin, stdout)

displayWorld :: Sokoban IO ()
displayWorld = liftIO . print =<< get

getMove :: IO Move
getMove =
    getChar >>= \case
      'k' -> return up
      'h' -> return left
      'j' -> return down
      'l' -> return right
      _ -> getMove

gameLoop :: Sokoban IO ()
gameLoop =
    do displayWorld
       wasValid <- applyMove =<< liftIO getMove
       unless wasValid $ liftIO $ putChar '\a'
       finished <- isFinished
       if finished
          then do displayWorld
                  elapsed <- gets steps
                  liftIO $ putStrLn $ "You win after " ++ show elapsed ++ " steps!"
          else gameLoop

main :: IO ()
main =
    do hSetEcho stdin False
       hSetBuffering stdin NoBuffering
       hSetBuffering stdout NoBuffering
       evalStateT gameLoop $ loadWorld $ level 1

