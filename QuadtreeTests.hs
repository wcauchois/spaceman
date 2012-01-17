{-# LANGUAGE ImplicitParams #-}

import Quadtree
import Test.QuickCheck
import Data.Maybe
import Data.List(sort)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g x y = f (g x y)

prop_insertAndRetrieveAll :: NonEmptyList Point -> Positive Capacity -> Bool
prop_insertAndRetrieveAll (NonEmpty points) (Positive maximumCapacity) =
  sort (map fst (retrieveArea container quadtree)) == sort points
  where container = let (xs, ys) = unzip points
                        origin@(originX, originY) = (minimum xs, minimum ys)
                    in (origin, (maximum xs - originX, maximum ys - originY))
        quadtree = let ?maximumCapacity = maximumCapacity
                   in foldr (fromJust .: insert) (fromBounds container) $ map (flip (,) ()) points

deepCheck :: Testable prop => prop -> IO ()
deepCheck = quickCheckWith (stdArgs { maxSuccess = 1000 })

main :: IO ()
main = return ()
