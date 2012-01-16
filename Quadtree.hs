{-# LANGUAGE ImplicitParams #-}

module Quadtree(
  Quadtree, Bounds, Point, Capacity,
  insert, delete, retreiveArea, fromBounds
) where

import Control.Monad
import Data.HashSet(HashSet)
import qualified Data.HashSet as HS
import Data.Hashable

type Capacity = Int
type Point = (Double, Double)
type Bounds = (Point   -- X, Y
              , Point) -- Width, Height

data (Hashable a, Eq a) => Quadtree a =
  Node (HashSet a) Bounds (Quadtree a   -- Top left
                          , Quadtree a  -- Top right
                          , Quadtree a  -- Bottom left
                          , Quadtree a) -- Bottom right
  | Leaf Bounds [(Point, a)]

bounds :: Quadtree a -> Bounds
bounds (Node _ bounds _) = bounds
bounds (Leaf bounds _) = bounds

children :: Quadtree a -> [Quadtree a]
children (Node _ _ (topLeft, topRight, bottomLeft, bottomRight)) =
  [topLeft, topRight, bottomLeft, bottomRight]

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
  in Node (HS.fromList $ map snd entries) bounds (constructLeaf topLeftBounds
                                                 , constructLeaf topRightBounds
                                                 , constructLeaf bottomLeftBounds
                                                 , constructLeaf bottomRightBounds)
  where constructLeaf bounds =
          Leaf bounds $ filter ((`inside` bounds) . fst) entries

insert :: (?maximumCapacity :: Capacity) => (Point, a) -> Quadtree a -> Maybe (Quadtree a)
insert (Node set bounds (topLeft, topRight, bottomLeft, bottomRight)) entry@(point, label)
  | not (point `inside` bounds) = Nothing
  | otherwise = Just $ Node (HS.insert label set) bounds $
                  let tryInsert child = fromMaybe child (insert child entry)
                  in (tryInsert topLeft
                     , tryInsert topRight
                     , tryInsert bottomLeft
                     , tryInsert bottomRight)
insert (Leaf bounds entries) entry
  | not (point `inside` bounds) = Nothing
  | length entries + 1 > ?maximumCapacity = insert (subdivide leaf) entry
  | otherwise = Leaf bounds $ entry : entries

delete :: a -> Quadtree a -> Quadtree a
delete label node@(Node set bounds (topLeft, topRight, bottomLeft, bottomRight))
  | label `HS.member` set = Node (HS.delete label set) bounds (delete label topLeft,
                                                               delete label topRight,
                                                               delete label bottomLeft,
                                                               delete label bottomRight)
  | otherwise = node
delete label (Leaf bounds entries) = Leaf bounds $ filter ((/=label) . snd) entries

retreiveArea :: Bounds -> Quadtree a -> [(Point, a)]
retreiveArea area (Leaf bounds entries) =
  let intersection = area `intersect` bounds
  in filter ((`inside` intersection) . fst) entries
retreiveArea area node@(Node bounds _)
  | empty (area `intersect` bounds) = []
  | otherwise = concat $ map (`retreiveArea` area) $ children node

