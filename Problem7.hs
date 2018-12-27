import EulerPrimes (isPrime)

getPrime :: Int
getPrime = snd (head (dropWhile ((<=10000) . fst) (zip [3..] [x | x <- [5, 7..], isPrime x])))

main = putStrLn (show getPrime)