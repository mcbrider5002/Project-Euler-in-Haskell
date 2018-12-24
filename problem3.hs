import Data.List (find)

target = 600851475143

intSqrt :: Int -> Int
intSqrt n = (floor . sqrt . fromIntegral) n

---

isPrime :: Int -> Bool
isPrime n = all (\x -> n `mod` x /= 0) [2..(intSqrt n)]

-- Naive implementation
findLargestPrimeFactor :: Int -> Maybe Int
findLargestPrimeFactor n = find (\x -> n `rem` x == 0
                                 && isPrime x) [intSqrt n,(intSqrt n)-1..2]

largestPrimeFactor :: Maybe Int
largestPrimeFactor = findLargestPrimeFactor target

---

-- Implementation using unique decomposition of primes
findLargestPrimeFactorFold :: Int -> Int
findLargestPrimeFactorFold n = snd (foldl (\(a, p) c -> if a `rem` c == 0 then (a `div` c, c) else (a, p)) (n, 0) [2 .. intSqrt n])

largestPrimeFactorFold :: Int
largestPrimeFactorFold = findLargestPrimeFactorFold target

---

-- Better implementation using unique decomposition of primes
findLargestPrimeFactorRecursive :: Int -> [Int] -> Int
findLargestPrimeFactorRecursive n (x:xs)
    | n `rem` x == 0 = if n == x then x else findLargestPrimeFactorRecursive (n `div` x) xs
    | otherwise = findLargestPrimeFactorRecursive n xs
    
largestPrimeFactorRecursive :: Int
largestPrimeFactorRecursive = findLargestPrimeFactorRecursive target [2 .. intSqrt target]

main = putStrLn (show largestPrimeFactorRecursive)