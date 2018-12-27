import Data.List (find)
import EulerUtils (intSqrt)
import EulerPrimes (isPrime)

target = 600851475143

---

-- Naive implementation where we search all factors then filter out nonprimes and take max
findLargestPrimeFactor :: Int -> Int
findLargestPrimeFactor n = let sqrtn = intSqrt n
                               factors = [x | x <- [3, 5 .. sqrtn], n `rem` x == 0]
                           in  maximum (filter isPrime (factors ++ (map (div n) factors)))

largestPrimeFactor :: Int
largestPrimeFactor = findLargestPrimeFactor target

---

{- Implementation using unique decomposition of primes - only checks up to sqrt(n), then checks the paired factors of these numbers...
   (If the paired factor is composite, its decomposition will have been factored out - for a number to be composite it must be itself a product of several prime
    factors, but for these factors to not have already been previously exhaustively filtered out (and therefore for n to be divisible by this number) 
    then each the (>1) primes in their decomposition must themselves be greater than sqrt(n) - 
    this implies they are greater than n and for natural numbers, this is a contradiction)

   This should have better time complexity than the below algorithm's worst-case - this should be O(sqrt(n)) as opposed to O(n) (which occurs below when input is prime)
-} 
findLargestPrimeFactorFold :: Int -> Int
findLargestPrimeFactorFold n = let factorOut m d = if m `rem` d == 0 then factorOut (m `div` d) d else m
                                   (factoredn, primes) = foldl (\(a, ps) c -> if a `rem` c == 0 then (a `factorOut` c, c:ps) else (a, ps)) (n, []) [3, 5 .. intSqrt n]
                               in  if factoredn == 1 && primes /= [] -- completely factored, and wasn't 1 to start with
                                   then head primes
                                   else foldr (\p a -> if a > p && (a `rem` p == 0) then a `div` p else a) factoredn primes
                                          
largestPrimeFactorFold :: Int
largestPrimeFactorFold = findLargestPrimeFactorFold target

---

-- More concise implementation using unique decomposition of primes
findLargestPrimeFactorRecursive :: Int -> [Int] -> Int
findLargestPrimeFactorRecursive n ls@(x:xs)
    | n `rem` x == 0 = if n == x then x else findLargestPrimeFactorRecursive (n `div` x) ls
    | otherwise = findLargestPrimeFactorRecursive n xs
    
largestPrimeFactorRecursive :: Int
largestPrimeFactorRecursive = findLargestPrimeFactorRecursive target [3, 5 .. (target `div` 2)]

main = putStrLn (show largestPrimeFactorRecursive)