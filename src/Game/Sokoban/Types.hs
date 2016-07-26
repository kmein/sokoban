module Game.Sokoban.Types where

import Control.Monad.State (StateT)
import Data.Set (Set, empty, insert, member)
import Linear.V2 (V2(..))

type Move = V2 Int

up :: Move
up = V2 0 1

down :: Move
down = V2 0 (-1)

left :: Move
left = V2 (-1) 0

right :: Move
right = V2 1 0

type Coordinate = V2 Int

data World
   = World
   { walls :: Set Coordinate
   , crates :: Set Coordinate
   , storage :: Set Coordinate
   , worker :: Coordinate
   , outmost :: Coordinate
   , steps :: Int
   }

isStorage :: World -> Coordinate -> Bool
isStorage world point = point `member` storage world

isWall :: World -> Coordinate -> Bool
isWall world point = point `member` walls world

isCrate :: World -> Coordinate -> Bool
isCrate world point = point `member` crates world

isWorker :: World -> Coordinate -> Bool
isWorker world point = point == worker world

instance Show World where
    show world = unlines chars
        where
          V2 xMax yMax = outmost world
          chars = [[func (V2 x y) | x <- [0 .. xMax]] | y <- [yMax, yMax - 1 .. 0]]
          func coord
              | isCrate world coord && isStorage world coord  = '*'
              | isWorker world coord && isStorage world coord  = '+'
              | isWall world coord = '#'
              | isWorker world coord = '@'
              | isCrate world coord = 'o'
              | isStorage world coord = '.'
              | otherwise = ' '

loadWorld :: String -> World
loadWorld format = foldl addToWorld emptyWorld { outmost = outmostCoord } mapping
    where
      rows = lines format
      numRows = length rows
      coords = [[(x, y) | x <- [0 ..]] | y <- [numRows - 1, numRows - 2 .. 0]]
      -- reverse y-numbering causes 0,0 to be at the bottom left instead of top left
      mapping = concat $ zipWith zip coords rows
      outmostCoord = uncurry V2 $ maximum $ map fst mapping
      addToWorld world (coord, ch) =
          case ch of
            '@' -> world { worker = point }
            'o' -> world { crates = insert point (crates world) }
            '#' -> world { walls = insert point (walls world) }
            ' ' -> world
            '.' -> world { storage = insert point (storage world) }
            '*' -> world
                { crates = insert point (crates world)
                , storage = insert point (storage world) }
            '+' -> world
                { worker = point
                , storage = insert point (storage world) }
            c -> error $ "Not recognised: " ++ show c
          where point = uncurry V2 coord

emptyWorld :: World
emptyWorld =
    World
    { walls = empty
    , crates = empty
    , storage = empty
    , worker = V2 0 0
    , outmost = V2 0 0
    , steps = 0
    }

type Sokoban m a = StateT World m a
