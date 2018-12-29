import EulerPrimes (generatePrimes)

primeSum = sum (takeWhile (<2000000) generatePrimes)

main = print primeSum