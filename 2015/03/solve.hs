import Data.List
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

parse :: String -> [(Int, Int)]
parse = map toDirection

toDirection :: Char -> (Int, Int)
toDirection '^' = (0, 1)
toDirection '>' = (1, 0)
toDirection 'v' = (0, -1)
toDirection '<' = (-1, 0)
toDirection _ = (0, 0)

solve :: [(Int, Int)] -> Int
solve xs = length $ nub $ foldl visit [(0, 0)] xs

visit :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
visit xs (x, y) = (firstx + x, firsty + y) : xs
  where
    firstx = fst first
    firsty = snd first
    first = head xs

solve' :: [(Int, Int)] -> Int
solve' xs = length $ nub $ visited
  where
    visited = foldl visit [(0, 0)] (evens xs) ++ foldl visit [(0, 0)] (odds xs)

evens :: [a] -> [a]
evens [] = []
evens [x] = [x]
evens (x:_:xs) = x : evens xs

odds :: [a] -> [a]
odds [] = []
odds [x] = [x]
odds (_:x:xs) = x : odds xs

