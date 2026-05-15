import System.IO

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  input <- hGetContents handle
  let parsedInput = parse input
  let solution = solve parsedInput
  print $ "01_1: " ++ show solution
  let solution' = solve' parsedInput
  print $ "01_2: " ++ show solution'

parse :: String -> [Int]
parse = map toInt

toInt :: Char -> Int
toInt '(' = 1
toInt ')' = -1
toInt _ = 0

solve :: [Int] -> Int
solve = sum

solve' :: [Int] -> Int
solve' = solveIter' 0 0

solveIter' :: Int -> Int -> [Int] -> Int
solveIter' steps (-1) _ = steps
solveIter' steps pos (x:xs) = solveIter' (steps + 1) (pos + x) xs

