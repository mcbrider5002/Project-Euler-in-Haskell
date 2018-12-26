module EulerUtils where

intSqrt :: Int -> Int
intSqrt n = (floor . sqrt . fromIntegral) n

-- Split an integral number into its digits
integralDigits :: (Integral a) => a -> [a]
integralDigits n = let pTens = takeWhile (<=n) (map (\x -> 10 ^ x) [0..])
                   in  map (\x -> (n `div` x) `rem` 10) pTens