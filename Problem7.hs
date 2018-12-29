import EulerPrimes (generatePrimes)

getPrime :: Int
getPrime = generatePrimes !! 10000

main = print getPrime