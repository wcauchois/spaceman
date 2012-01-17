{-# LANGUAGE ImplicitParams #-}

module Quadtree(
  Quadtree, Bounds, Point, Capacity,
  insert, delete, retrieveArea, fromBounds,
  
  insertSimple, inside, bounds, children, subdivide, intersect, empty -- XXX
) where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Decimal
import Data.List((\\))
import Debug.Trace -- XXX

insertSimple m entry q = let ?maximumCapacity = m in insert entry q

type Capacity = Int
type Point = (Decimal, Decimal)
type Bounds = (Point   -- X, Y
              , Point) -- Width, Height

data Quadtree a =
    Node (Set a) Bounds (Quadtree a   -- Top left
                      , Quadtree a  -- Top right
                      , Quadtree a  -- Bottom left
                      , Quadtree a) -- Bottom right
  | Leaf Bounds [(Point, a)]
  deriving (Show) -- XXX

bounds :: Quadtree a -> Bounds
bounds (Node _ bounds _) = bounds
bounds (Leaf bounds _) = bounds

children :: Quadtree a -> [Quadtree a]
children (Node _ _ (topLeft, topRight, bottomLeft, bottomRight)) =
  [topLeft, topRight, bottomLeft, bottomRight]

inside :: Point -> Bounds -> Bool
(x', y') `inside` ((x, y), (width, height)) =
  x' >= x && x' <= x + width && y' >= y && y' <= y + height

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

subdivide :: (Ord a) => Quadtree a -> Quadtree a
subdivide (Leaf bounds@((x, y), (width, height)) entries) =
  Node (Set.fromList $ map snd entries) bounds children
  where portions = concatMap (uncurry replicate)
        widths = portions (width `divide` 2)
        heights = portions (height `divide` 2)
        -- TODO: turn this into an elegant map or fold or whatevers
        topLeftBounds = ((x + 0, y + 0), (widths !! 0, heights !! 0))
        topRightBounds = ((x + widths !! 0, y + 0), (widths !! 1, heights !! 0))
        bottomLeftBounds = ((x + 0, y + heights !! 0), (widths !! 0, heights !! 1))
        bottomRightBounds = ((x + widths !! 0, y + heights !! 0), (widths !! 1, heights !! 1))
        (children, []) = (`runState` entries) $
          do topLeftEntries     <- popEntries topLeftBounds
             topRightEntries    <- popEntries topRightBounds
             bottomLeftEntries  <- popEntries bottomLeftBounds
             bottomRightEntries <- popEntries bottomRightBounds
             return (Leaf topLeftBounds topLeftEntries
                    , Leaf topRightBounds topRightEntries
                    , Leaf bottomLeftBounds bottomLeftEntries
                    , Leaf bottomRightBounds bottomRightEntries)
        popEntries bounds = do entries <- get
                               let entries' = filter ((`inside` bounds) . fst) entries
                               put (entries \\ entries')
                               return entries'

insert :: (?maximumCapacity :: Capacity, Ord a)
       => (Point, a) -> Quadtree a -> Maybe (Quadtree a)
insert entry@(point, label) (Node set bounds (topLeft, topRight, bottomLeft, bottomRight))
  | not (point `inside` bounds) = Nothing
  | otherwise = Just $ Node (Set.insert label set) bounds $ (`evalState` False) $
                  do let tryInsert child = get >>= \done ->
                           case (done, insert entry child) of
                             (True, _)            -> return child
                             (False, Nothing)     -> return child
                             (False, Just child') -> put True >> return child'
                     topLeft'     <- tryInsert topLeft
                     topRight'    <- tryInsert topRight
                     bottomLeft'  <- tryInsert bottomLeft
                     bottomRight' <- tryInsert bottomRight
                     return (topLeft', topRight', bottomLeft', bottomRight')
insert entry@(point, label) leaf@(Leaf bounds entries)
  | not (point `inside` bounds) = Nothing
  | length entries + 1 > ?maximumCapacity = insert entry (subdivide leaf)
  | otherwise = Just $ Leaf bounds $ entry : entries

delete :: (Ord a) => a -> Quadtree a -> Quadtree a
delete label node@(Node set bounds (topLeft, topRight, bottomLeft, bottomRight))
  | label `Set.member` set = Node (Set.delete label set) bounds (delete label topLeft,
                                                                 delete label topRight,
                                                                 delete label bottomLeft,
                                                                 delete label bottomRight)
  | otherwise = node
delete label (Leaf bounds entries) = Leaf bounds $ filter ((/=label) . snd) entries

retrieveArea :: Bounds -> Quadtree a -> [(Point, a)]
retrieveArea area (Leaf bounds entries) =
  let intersection = area `intersect` bounds
  in filter ((`inside` intersection) . fst) entries
retrieveArea area node@(Node _ bounds _)
  | empty (area `intersect` bounds) = []
  | otherwise = concat $ map (retrieveArea area) $ children node

