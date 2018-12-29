module EulerUtils where

intSqrt :: Int -> Int
intSqrt n = (floor . sqrt . fromIntegral) n

type SplitInt = [Int]

-- Split an integral number into its digits
integralDigits :: (Integral a) => a -> [Int]
integralDigits n = let powersOfTen = takeWhile (<=n) (map (\x -> 10 ^ x) [0..])
                   in  map (fromIntegral) (map (\x -> (n `div` x) `rem` 10) powersOfTen)