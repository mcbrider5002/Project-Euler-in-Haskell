module EulerPrimes (isPrime, generatePrimes) where

import EulerUtils (intSqrt)

{- Haskell has its own methods for dealing with primes (in Data.Numbers.Primes and Math.NumberTheory.Primes.Testing), 
   but at least for the early problems, I feel that would be cheating...
   So here are some of my own (for now, basic) implementations which I may or may not use throughout -}

--- Primality Testing ---

trialDivision :: Int -> Bool
trialDivision n = (n > 1) && (all (\x -> n `rem` x /= 0) [2..(intSqrt n)])

isPrime = trialDivision

--- Prime Generating ---

-- Generates an infinite list of primes using trial division
trialPrimes :: [Int]
trialPrimes = 2 : [x | x <- [3, 5..], isPrime x]

sieve :: [Int] -> [Int]
sieve (l:ls) = l : sieve [x | x <- ls, x `rem` l /= 0]

-- An implementation of the Sieve of Eratosthenes, but not a very efficient one...    
sieveOfEratosthenes :: [Int]
sieveOfEratosthenes = 2 : sieve [3, 5..]

generatePrimes = trialPrimes