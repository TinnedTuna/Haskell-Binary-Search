import BinarySearch (binarySearch)
import Data.List
import Test.QuickCheck

prop_contains :: [Int] -> Property
--prop_contains xs x = x `elem` xs ==> binarySearch (sort xs) x
prop_contains xs = True ==> let ys = sort xs in and [ binarySearch ys y | y <- ys ]

prop_not_contains :: [Int] -> Int -> Property
prop_not_contains xs x = x `notElem` xs ==> not $ binarySearch (sort xs) x

main = do {
   quickCheck prop_contains;
   quickCheck prop_not_contains; }
