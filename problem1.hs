import Data.List (nub)

-- Helper
getMultiples :: Int -> [Int]
getMultiples base = [base, base *2..999]

---

-- Simple implementation
multiples :: Int
multiples = sum (nub ((getMultiples 3) ++ (getMultiples 5)))

---
  
-- Sum of arithmetic sequence using mathematical formula
arithmeticSum :: Int -> Int -> Int -> Int
arithmeticSum n a d = floor (((fromIntegral n) / 2.0) 
                                * (fromIntegral (2 * a + (n - 1) * d)))

-- Implementation using arithmetic sequence formula                                
multiplesArithmeticSum :: Int
multiplesArithmeticSum = (arithmeticSum nThrees 3 3) 
                         + (arithmeticSum nFives 5 5)
                         - (arithmeticSum nFifteens 15 15)
                            where ns = length . getMultiples
                                  nThrees =  ns 3
                                  nFives = ns 5
                                  nFifteens = ns 15

---
   
-- Implementation using list comprehension   
multiplesComprehension :: Int
multiplesComprehension = sum [x | x <- [1..999], x `rem` 3 == 0 || x `rem` 5 == 0]

---

-- Checks all implementations return same result
multiplesEquivalency :: Bool
multiplesEquivalency = (multiples == multiplesArithmeticSum)
                        && (multiples == multiplesComprehension)
                        
main = putStrLn (show multiples)