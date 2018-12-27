import EulerPrimes (isPrime)

getPrime :: Int
getPrime = [x | x <- (2 : [3, 5..]), isPrime x] !! 10000

main = putStrLn (show getPrime)