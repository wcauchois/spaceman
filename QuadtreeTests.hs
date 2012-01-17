{-# LANGUAGE ImplicitParams #-}

import Quadtree
import Test.QuickCheck
import Data.Maybe
import Data.List(sort)
import Data.Decimal

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g x y = f (g x y)

container :: [Point] -> Bounds
container points = (origin, (maximum xs - originX, maximum ys - originY))
  where (xs, ys) = unzip points
        origin@(originX, originY) = (minimum xs, minimum ys)

fromPoints :: (?maximumCapacity :: Capacity) => [Point] -> Quadtree ()
fromPoints points =
  foldr (fromJust .: insert) (fromBounds $ container points) $
    map (flip (,) ()) points

prop_insertAndRetrieveAll :: NonEmptyList Point -> Positive Capacity -> Bool
prop_insertAndRetrieveAll (NonEmpty points) (Positive maximumCapacity) =
  sort (map fst (retrieveArea bounds quadtree)) == sort points
  where bounds = container points
        quadtree = let ?maximumCapacity = maximumCapacity in fromPoints points

prop_insertAndRetrieveSome :: NonEmptyList Point
                           -> (Point, (Positive Decimal, Positive Decimal))
                           -> Positive Capacity
                           -> Bool
prop_insertAndRetrieveSome (NonEmpty points)
                           ((x, y), (Positive width, Positive height))
                           (Positive maximumCapacity) =
  sort (map fst (retrieveArea bounds quadtree)) == sort points'
  where bounds = container points
        quadtree = let ?maximumCapacity = maximumCapacity in fromPoints points
        points' = filter (`inside` bounds) points

prop_insertAndDelete :: NonEmptyList Point -> Positive Capacity -> Bool
prop_insertAndDelete (NonEmpty points) (Positive maximumCapacity) =
  sort (map fst (retrieveArea bounds (delete 1 quadtree))) == sort (tail points)
  where bounds = container points
        quadtree = let ?maximumCapacity = maximumCapacity
                   in foldr (fromJust .: insert) (fromBounds bounds) $ zip points [1..]

deepCheck :: Testable prop => prop -> IO ()
deepCheck = quickCheckWith (stdArgs { maxSuccess = 1000 })

main :: IO ()
main = do deepCheck prop_insertAndRetrieveAll
          deepCheck prop_insertAndRetrieveSome
          deepCheck prop_insertAndDelete
