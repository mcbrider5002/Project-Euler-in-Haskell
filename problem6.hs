sumOfSquaresDiff :: [Int] -> Int
sumOfSquaresDiff ls = (sum(ls))^2 - sum ([x^2 | x <- ls])

squareDiffs = sumOfSquaresDiff [1..100]

main = putStrLn (show squareDiffs)