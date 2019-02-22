type DENOM = Int
type AMOUNT = Int

combinations :: [DENOM] -> Int -> [[(DENOM, AMOUNT)]]
combinations _ 0 = [[]]
combinations [] _ = []
combinations (c : cs) total =
  [(c, amount) : list | amount <- [1 .. total `div` c], list <- combinations cs $ total - c * amount] ++
  combinations cs total
