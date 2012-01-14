{-# LANGUAGE ImplicitParams #-}

module Quadtree(
  Quadtree, Bounds, Point,
  insert, retreiveArea, fromBounds
) where

import Control.Monad

type Point = (Double, Double)
type Bounds = (Point   -- X, Y
              , Point) -- Width, Height

data Quadtree a = Node Bounds (Quadtree a   -- Top left
                              , Quadtree a  -- Top right
                              , Quadtree a  -- Bottom left
                              , Quadtree a) -- Bottom right
                | Leaf Bounds [(Point, a)]

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

fromBounds :: Bounds -> Quadtree a
fromBounds = (`Leaf` [])

subdivide :: Quadtree a -> Quadtree a
subdivide (Leaf bounds@((x, y), (width, height)) entries) =
  let halfWidth = width / 2; halfHeight = height / 2
      [topLeftBounds, topRightBounds, bottomLeftBounds, bottomRightBounds] =
        map (\(ofsX, ofsY) -> ((x + ofsX, y + ofsY), (halfWidth, halfHeight))) $
          zip (concat $ map (replicate 2) [0, halfWidth]) (cycle [0, halfHeight])
  in Node bounds (constructLeaf topLeftBounds
                 , constructLeaf topRightBounds
                 , constructLeaf bottomLeftBounds
                 , constructLeaf bottomRightBounds)
  where constructLeaf bounds =
          Leaf bounds $ filter ((`inside` bounds) . fst) entries

insert :: (?maximumCapacity :: Int) => Quadtree a -> (Point, a) -> Quadtree a
insert leaf@(Leaf bounds entries) entry@(k, v)
  | length entries + 1 > ?maximumCapacity = insert (subdivide leaf) entry
  | otherwise = Leaf bounds $ entry : entries

retreiveArea :: Quadtree a -> Bounds -> [(Point, a)]
retreiveArea (Leaf bounds entries) area =
  let intersection = area `intersect` bounds
  in filter ((`inside` intersection) . fst) entries
retreiveArea (Node bounds (topLeft, topRight, bottomLeft, bottomRight)) area
  | empty (area `intersect` bounds) = []
  | otherwise = let children = [topLeft, topRight, bottomLeft, bottomRight]
                in concat $ map (`retreiveArea` area) children

