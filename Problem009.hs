import EulerUtils (intSqrt, isSquare)

findTriplet :: Int -> Int -> Maybe Int
findTriplet a b 
    | a >= 333 = Nothing -- We have that a < b < c so a > 333 => a + b + c > 1000, although it's not the tightest bound
    | a + b + c > 1000 = findTriplet (a + 1) (a + 2)
    | (isSquare (a^2 + b^2)) && a + b + c == 1000 = Just (a * b * c) 
    | otherwise = findTriplet a (b + 1)
        where c = intSqrt(a^2 + b^2)

triplet = findTriplet 1 2

triplet2 :: Maybe Int
triplet2 = case [a * b * c | a <- [1..333], b <- [a..1000], let c = intSqrt(a^2 + b^2), (isSquare (a^2 + b^2)) && a + b + c == 1000] of
                    [x] -> Just x
                    _ -> Nothing
      
main = print triplet