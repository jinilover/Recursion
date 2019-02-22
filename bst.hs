import qualified Data.Vector as V

-- | noOfTrees for 0 consecutive number = 1, because there is no node, therefore only 1 tree, an empty tree.
-- noOfTrees for 1 consecutive number = 1
-- 1
-- noOfTrees for 2 consecutive numbers = 2
-- 1      2
--  \    /
--   2  1
-- noOfTrees for 3 consecutive numbers = 5
-- 1          1      2      3          3
--  \          \    / \    /          /
--   2          3  1   3  1          2
--    \        /           \        /
--     3      2             2      1
-- and so on.

noOfBsts :: Int -> Integer
noOfBsts n = bstV n V.! n
  where bstV 0 = V.fromList [1]
        bstV 1 = V.fromList [1, 1]
        bstV n = let v = bstV $ n - 1
                     evenNumVal = 2 * sum (map (\x -> v V.! x * v V.! (n - 1 - x)) [n-1, n-2 .. (n - 1) `div` 2 + 1])
                     finalVal = if n `mod` 2 == 0 
                                then evenNumVal 
                                else evenNumVal + square (v V.! ((n - 1) `div` 2)) in
                 v V.++ V.fromList [finalVal]
        square n = n * n
