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

parse :: String -> [(Int, Int, Int)]
parse s = map parseDims $ lines s

parseDims :: String -> (Int, Int, Int)
parseDims s = (dims!!0, dims!!1, dims!!2)
  where
    dims = map read $ split (== 'x') s

split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
  "" -> []
  s' -> w : split p s''
    where (w, s'') = break p s'

paperArea :: (Int, Int, Int) -> Int
paperArea (l, w, h) = 2 * lw + 2 * wh + 2 * hl + minimum [lw, wh, hl]
  where
    lw = l * w
    wh = w * h
    hl = h * l

solve :: [(Int, Int, Int)] -> Int
solve xs = sum $ map paperArea xs

solve' :: [(Int, Int, Int)] -> Int
solve' xs = sum $ map ribbonLength xs

ribbonLength :: (Int, Int, Int) -> Int
ribbonLength (l, w, h) = 2 * l + 2 * w + 2 * h + l * w * h - 2 * maximum [l, w, h]

