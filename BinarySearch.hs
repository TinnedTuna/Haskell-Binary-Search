module BinarySearch (binarySearch) where

binarySearch :: (Ord a) => [a] -> a -> Bool
binarySearch [] _ = False
binarySearch [x] a = x == a
binarySearch xs a
  | a == (xs !! midpoint) = True
  | a < (xs !! midpoint) = binarySearch lower a
  | a > (xs !! midpoint) = binarySearch upper a
  where
    midpoint = floor $ (fromIntegral $ length xs) / 2
    upper = drop (midpoint + 1) xs
    lower = take midpoint xs
