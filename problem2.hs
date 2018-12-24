fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

getFibs :: Int
getFibs = sum (filter even (takeWhile (<4000000) fibs))

main = putStrLn (show getFibs)