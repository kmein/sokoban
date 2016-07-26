module Game.Sokoban
    ( module Game.Sokoban
    , module Game.Sokoban.Types
    ) where

import Game.Sokoban.Types
import Control.Monad.State (get, gets, modify, when, unless)
import Data.Set (delete, insert, member)

applyMove :: (Monad m) => Move -> Sokoban m Bool
applyMove move =
    do w <- get
       let newPosition = worker w + move
           newPosition' = newPosition + move
       if not $
           isWall w newPosition
           || (isCrate w newPosition && (isCrate w newPosition' || isWall w newPosition'))
       then do gameStep move
               if isCrate w newPosition
               then True <$ moveCrate newPosition newPosition'
               else return False
        else return False
    where
      moveCrate :: (Monad m) => Coordinate -> Coordinate -> Sokoban m ()
      moveCrate old new = modify $ \world ->
          world { crates = insert new $ delete old $ crates world }
      gameStep :: (Monad m) => Coordinate -> Sokoban m ()
      gameStep move = modify $ \world ->
          world
          { worker = worker world + move
          , steps = succ $ steps world
          }

isFinished :: (Monad m) => Sokoban m Bool
isFinished = (==) <$> gets crates <*> gets storage
