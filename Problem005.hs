{- Solution by prime factorisation by repeatedly factoring out primes for each number in the list of numbers 
   for which we are trying to find the minimal number divisible by all of them
   using a similar algorithm as problem 3 but keeping a list of found primes with multiples
   (or otherwise, the intersection of all multisets representing the prime factorisation of each number) -}
uniqueFactors :: [Int] -> [Int] -> [Int]
uniqueFactors [] factors = factors
uniqueFactors (n:ns) factors = let newFactor = foldl (\acc x -> if acc `rem` x == 0 then acc `div` x else acc) n factors
                               in  uniqueFactors ns (newFactor : factors)

smallestDivisible :: Int
smallestDivisible = product (uniqueFactors [2..20] [])

main = print smallestDivisible