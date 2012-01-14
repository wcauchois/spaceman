{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spaceman where

import Control.Monad.Reader

type Point = (Double, Double)
type Size = Double

data Quadtree a = Quadtree Size (Quadtree a, Quadtree a, Quadtree a, Quadtree a)
                | Leaf [(Point, a)]

data Config = Config {
  maximumCapacity :: Int
}

newtype DB a = DB { runDB :: Reader Config a } deriving (Monad, MonadReader Config)

bounds :: [Point] -> Size
bounds points = let xs = map fst points
                    ys = map snd points
                in maximum xs - minimum xs `max` maximum ys - minimum ys

insert :: Quadtree a -> (Point, a) -> DB (Quadtree a)
insert (Leaf xs) x@(p, v) = do capacity <- asks maximumCapacity
                               if length xs > capacity
                                 then return $ Quadtree (Bounds xs) undefined
                                 else return $ Leaf (x : xs)
