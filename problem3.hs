import Data.List (find)

intSqrt :: Integer -> Integer
intSqrt n = (floor . sqrt . fromIntegral) n

isPrime :: Integer -> Bool
isPrime n = all (\x -> n `mod` x /= 0) [2..(intSqrt n)]

findLargestPrimeFactor :: Integer -> Maybe Integer
findLargestPrimeFactor n = find (\x -> n `mod` x == 0
                                 && isPrime x) [intSqrt n,(intSqrt n)-1..2]

largestPrimeFactor :: Maybe Integer
largestPrimeFactor = findLargestPrimeFactor 600851475143