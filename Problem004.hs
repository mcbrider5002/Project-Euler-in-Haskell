import EulerUtils (integralDigits)

-- Checks if an Int is palindromic
isPalindrome :: Int -> Bool
isPalindrome n = decomposed == reverse decomposed
                    where decomposed = integralDigits n

---
          
{- Solution where we avoid redundant checks by abusing commutativity (i.e. we take the constraint x >= y so we don't check 
   both e.g. (x = z, y = z - 1) and (x = z - 1, y = z) for some z)
-}
largestPalindrome :: Int
largestPalindrome = maximum [x*y | x <- [999,998..100], y <- [x,x-1..100], isPalindrome (x * y)]

---

{- Solution where we use the above optimisation and break early when we find a new palindrome if future checks would be strictly less than our current result 
-}
palindromeRecursive :: Int -> Int -> Int -> Maybe Int
palindromeRecursive 99 _ _ = Nothing
palindromeRecursive x 99 m = palindromeRecursive (x-1) (x-1) m
palindromeRecursive x y m
    | (not . isPalindrome) (x * y) = palindromeRecursive x (y-1) m
    | b <= (x-1) * (x-1)           = palindromeRecursive (x-1) (x-1) b
    | otherwise                    = Just b
        where b = max m (x*y)
    
largestPalindromeRecursive :: Maybe Int
largestPalindromeRecursive = palindromeRecursive 999 999 0

main = print largestPalindromeRecursive