module Math.Cloud.Cloud where

-- | A point cloud.
type Cloud a = [a]

-- | A point cloud with named points, i.e. a collection of pairs @(name, point)@.
type TaggedCloud a b = [(a,b)]

