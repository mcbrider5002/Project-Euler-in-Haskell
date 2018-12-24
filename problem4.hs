import Data.List (sortBy)

-- Checks if an Int is palindromic
isPalindrome :: Int -> Bool
isPalindrome n = let pTens = takeWhile (<=n) (map (\x -> 10 ^ x) [0..])
                     decomposed = map (\x -> (n `div` x) `rem` 10) pTens
                 in  and (zipWith (==) decomposed (reverse decomposed))
                 
---
          
-- Brute force (but concise!) solution
largestPalindrome :: Int
largestPalindrome = head (sortBy (flip compare) [x*y | x <- [999,998..100], y <- [999,998..100], isPalindrome (x * y)])

---

{- More careful solution that avoids checking redundant twice by abusing commutativity (i.e. we take the constraint x >= y)
   and breaking early when we find a new palindrome if future checks would be strictly less than our current result 
-}
palindromeRecursive :: Int -> Int -> Int -> Maybe Int
palindromeRecursive 99 _ _ = Nothing
palindromeRecursive x 99 m = palindromeRecursive (x-1) (x-1) m
palindromeRecursive x y m
    | isPalindrome (x * y) = let b = max m (x*y)
                             in  if b > (x-1) * (x-1)
                                 then Just b
                                 else palindromeRecursive (x-1) (x-1) b
    | otherwise = palindromeRecursive x (y-1) m
    
largestPalindromeRecursive :: Maybe Int
largestPalindromeRecursive = palindromeRecursive 999 999 0