import EulerUtils (SplitInt, integralDigits)

target = --To Implement with IO

target2 = 13

-- Splits a number into its digits, then returns a list of lists of m consecutive digits within that number
numberSplitter :: (Integral a) => a -> Int -> [SplitInt]
numberSplitter n m = [take m (drop i d) | i <- [0..(length d) - m]]
                        where d = integralDigits n

---

-- Despite the name, this is O(n + m) and since the numbers we're working with are relatively small (~10^13 at most) this works relatively well for us
naive :: (Integral a) => a -> Int -> Int
naive n m = maximum (map prod (numberSplitter n m))

{- We can sort the digits in each number into order (as multiplication is commutative) and then just compare to find which will have the biggest product...
   This is O(n + n(mlogm)) so probably worse than the 'naive' method... 
-}
sorter :: (Integral a) => a -> Int -> Int
sorter n m = prod (maximum (map sort (numberSplitter n m)))

getMaxProd = naive target target2
getMaxProdSorter = naive target target2

main = putStrLn (show getMaxProd)