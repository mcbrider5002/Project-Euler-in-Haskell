import Data.List (find)

target = 600851475143

intSqrt :: Integer -> Integer
intSqrt n = (floor . sqrt . fromIntegral) n

---

isPrime :: Integer -> Bool
isPrime n = all (\x -> n `mod` x /= 0) [2..(intSqrt n)]

-- Naive implementation
findLargestPrimeFactor :: Integer -> Maybe Integer
findLargestPrimeFactor n = find (\x -> n `rem` x == 0
                                 && isPrime x) [intSqrt n,(intSqrt n)-1..2]

largestPrimeFactor :: Maybe Integer
largestPrimeFactor = findLargestPrimeFactor target

---

-- Implementation using unique decomposition of primes
findLargestPrimeFactorFold :: Integer -> Integer
findLargestPrimeFactorFold n = snd (foldl (\(a, p) c -> if a `rem` c == 0 then (a `div` c, c) else (a, p)) (n, 0) [2 .. intSqrt n])

largestPrimeFactorFold :: Integer
largestPrimeFactorFold = findLargestPrimeFactorFold target

---

-- Better implementation using unique decomposition of primes
findLargestPrimeFactorRecursive :: Integer -> [Integer] -> Integer
findLargestPrimeFactorRecursive n (x:xs)
    | n `rem` x == 0 = if n == x then x else findLargestPrimeFactorRecursive (n `div` x) xs
    | otherwise = findLargestPrimeFactorRecursive n xs
    
largestPrimeFactorRecursive :: Integer
largestPrimeFactorRecursive = findLargestPrimeFactorRecursive target [2 .. intSqrt target]