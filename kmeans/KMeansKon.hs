module KMeansKon where

import KMeansCore

import Data.List (foldl', foldr1, minimumBy)
import Data.Ord
import Data.Function (on)
import qualified Data.Vector.Mutable as MVector
import qualified Data.Vector as Vector
import Control.Parallel.Strategies
import Control.DeepSeq

data PointSum = PointSum {-# UNPACK #-} !Int {-# UNPACK #-} !Double {-# UNPACK #-} !Double

instance NFData PointSum where
  rnf (PointSum count xs ys) = () -- all fields are strict


(.+) :: PointSum -> PointSum -> PointSum
(PointSum c1 x1 y1) .+ (PointSum c2 x2 y2) = PointSum (c1 + c2) (x1 + x2) (y1 + y2)

(.+.) :: PointSum -> Point -> PointSum
(PointSum count xSum ySum) .+. (Point x y) = PointSum (count + 1) (xSum + x) (ySum + y)

sqrtDistance :: Point -> Point -> Double
sqrtDistance (Point x1 y1) (Point x2 y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

clusterFromPointSum :: Int -> PointSum -> Cluster
clusterFromPointSum clusterId ps = Cluster clusterId center
        where
                center = Point (xSum / fromIntegral count) (ySum / fromIntegral count)
                PointSum count xSum ySum = ps

nearest :: Point -> [Cluster] -> Cluster
nearest p cs = fst $ minimumBy (compare `on` snd)
                         [ (cluster, sqrtDistance center p) | cluster@(Cluster _ center) <- cs ]

assignPointsToClusters :: Int -> [Cluster] -> [Point] -> Vector.Vector PointSum
assignPointsToClusters clustersCount clusters points =
        Vector.create $ do
                mVec <- MVector.replicate clustersCount $ PointSum 0 0 0
                let addPoint p = do
                        let Cluster cid _ = nearest p clusters
                        psi <- MVector.read mVec cid
                        MVector.write mVec cid $! (psi .+. p)
                mapM_ addPoint points
                pure mVec


sumVectors :: [Vector.Vector PointSum] -> [PointSum]
sumVectors = Vector.toList . foldr1 (Vector.zipWith (.+))

findMeans :: [[Point]] -> [Cluster] -> [Cluster]
findMeans points clusters = loop (length clusters) 0 clusters
        where
            loop :: Int -> Int -> [Cluster] -> [Cluster]
            loop clustersCount i clusters
                            | i > 80                  = clusters
                            | clusters == newClusters = clusters
                            | otherwise               = loop clustersCount (i + 1) newClusters
                            where
                                 newClusters = zipWith clusterFromPointSum [0..] $ filter (\(PointSum c _ _) -> c > 0) pss
                                 pss = sumVectors $ ((map (assignPointsToClusters clustersCount clusters) points) `using` parList rpar)

