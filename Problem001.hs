-- Helper
getMultiples :: Int -> [Int]
getMultiples base = [base, base *2..999]

---

-- Simple implementation
multiples :: Int
multiples = msum 3 + msum 5 - msum 15
                where msum = sum . getMultiples

---

-- Sum of arithmetic sequence using mathematical formula
arithmeticSum :: Int -> Int -> Int -> Int
arithmeticSum n a d = (n * (2 * a + (n - 1) * d)) `div` 2

-- Implementation using arithmetic sequence formula                                
multiplesArithmeticSum :: Int
multiplesArithmeticSum = msum 3 + msum 5 - msum 15
                            where ns = length . getMultiples
                                  msum n = arithmeticSum (ns n) n n

---
   
-- Implementation using list comprehension   
multiplesComprehension :: Int
multiplesComprehension = sum [x | x <- [1..999], x `rem` 3 == 0 || x `rem` 5 == 0]

---

-- Checks all implementations return same result
multiplesEquivalency :: Bool
multiplesEquivalency = (multiples == multiplesArithmeticSum)
                        && (multiples == multiplesComprehension)
                        
main = print multiples