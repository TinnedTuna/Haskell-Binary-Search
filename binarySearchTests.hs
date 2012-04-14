import BinarySearch (binarySearch)
import Data.List
import Test.QuickCheck

prop_contains :: [Int] -> Int -> Property
prop_contains xs x = x `elem` xs ==> binarySearch (sort xs) x

prop_not_contains :: [Int] -> Int -> Property
prop_not_contains xs x = x `notElem` xs ==> not $ binarySearch (sort xs) x

main = do {
   quickCheck prop_contains;
   quickCheck prop_not_contains; }
