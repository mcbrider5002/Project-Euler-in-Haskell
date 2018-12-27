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

-- Generates an infinite list of primes using trial division
trialPrimes :: [Int]
trialPrimes = 2 : [x | x <- [3, 5..], isPrime x]

-- An implementation of the Sieve of Eratosthenes, but not a very efficient one...
sieve :: [Int] -> [Int] -> [Int]
sieve ps (l:ls) = l : sieve (l:ps) [x | x <- ls, x `rem` l /= 0]
    
sieveOfEratosthenes = 2 : (sieve [] ([3, 5..]))

generatePrimes = trialPrimes