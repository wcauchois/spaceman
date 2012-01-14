{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spaceman where

import Control.Monad
import Control.Monad.Reader

type Point = (Double, Double)
type Bounds = (Point   -- X, Y
              , Point) -- Width, Height

data Quadtree a = Node Bounds (Quadtree a   -- Top left
                              , Quadtree a  -- Top right
                              , Quadtree a  -- Bottom left
                              , Quadtree a) -- Bottom right
                | Leaf Bounds [(Point, a)]

data Config = Config {
  maximumCapacity :: Int
}

newtype Q a = Q { runQ :: Reader Config a } deriving (Monad, MonadReader Config)

config :: (Config -> a) -> Q a
config = asks

inside :: Point -> Bounds -> Bool
(x', y') `inside` ((x, y), (width, height)) =
  x' > x && x' < x + width && y' > y && y' < y + height

intersect :: Bounds -> Bounds -> Bounds
((x, y), (width, height)) `intersect` ((x', y'), (width', height')) =
  let resX = x `max` x'; resY = y `max` y'
  in ((resX, resY),
      ((x + width) `min` (x' + width') - resX,
       (y + height) `min` (y' + height') - resY))

empty :: Bounds -> Bool
empty (_, (width, height)) = width == 0 && height == 0
                           

subdivide :: Quadtree a -> Quadtree a
subdivide (Leaf bounds@((x, y), (width, height)) entries) =
  let halfWidth = width / 2; halfHeight = height / 2
      [topLeftBounds, topRightBounds, bottomLeftBounds, bottomRightBounds] =
        map (\offset -> ((x+fst offset, y+snd offset), (halfWidth, halfHeight))) $
          zip (concat $ map (replicate 2) [0, halfWidth]) (cycle [0, halfHeight])
  in Node bounds (constructLeaf topLeftBounds
                 , constructLeaf topRightBounds
                 , constructLeaf bottomLeftBounds
                 , constructLeaf bottomRightBounds)
  where constructLeaf bounds =
          Leaf bounds $ filter ((`inside` bounds) . fst) entries

insert :: Quadtree a -> (Point, a) -> Q (Quadtree a)
insert leaf@(Leaf bounds entries) entry@(k, v) =
  do capacity <- config maximumCapacity
     if length entries + 1 > capacity
       then insert (subdivide leaf) entry
       else return (Leaf bounds $ entry : entries)

retreiveArea :: Quadtree a -> Bounds -> Q [(Point, a)]
retreiveArea (Leaf bounds entries) area =
  let intersection = area `intersect` bounds
  in return $ filter ((`inside` intersection) . fst) entries
retreiveArea (Node bounds (topLeft, topRight, bottomLeft, bottomRight)) area
  | empty (area `intersect` bounds) = return []
  | otherwise = let children = [topLeft, topRight, bottomLeft, bottomRight]
                in liftM concat $ mapM (`retreiveArea` area) children

