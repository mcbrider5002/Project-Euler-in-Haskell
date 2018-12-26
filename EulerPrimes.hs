module EulerPrimes (isPrime, generatePrimes) where

import EulerUtils (intSqrt)

{- Haskell has its own methods for dealing with primes (in Data.Numbers.Primes and Math.NumberTheory.Primes.Testing), 
   but at least for the early problems, I feel that would be cheating...
   So here are some of my own (for now, basic) implementations which I may or may not use throughout -}

--- Primality Testing ---

trialDivision :: Int -> Bool
trialDivision n = all (\x -> n `rem` x /= 0) [2..(intSqrt n)]

isPrime = trialDivision

--- Prime Generating ---

-- Finds all primes in the given range using trial division
trialPrimes :: [Int] -> [Int]
trialPrimes ls = [x | x <- reverse ls, isPrime x]

-- An implementation of the Sieve of Eratosthenes, but not a very efficient one...
sieve :: [Int] -> [Int] -> [Int]
sieve ps [] = ps
sieve ps (l:ls)
    | (isPrime l) = sieve (l:ps) ([x | x <- ls, x `rem` l /= 0])
    | otherwise = sieve ps ls
    
-- Find all primes in the range 5..n using given primality checker
findPrimes :: ([Int] -> [Int]) -> Int -> [Int]
findPrimes f n
    | n <= 1 = []
    | otherwise = ([x | x <- [5..n]])
    
naivePrimes n = (findPrimes (trialPrimes) n) ++ [3, 2]
sieveOfEratosthenes = findPrimes (sieve [3, 2])

generatePrimes = naivePrimes
