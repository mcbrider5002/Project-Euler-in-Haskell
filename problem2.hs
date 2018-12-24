fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

getFibs :: Integer
getFibs = sum (filter even (takeWhile (<4000000) fibs))

main = putStrLn (show getFibs)