import Data.Char (isDigit, digitToInt)
import Data.List (sort)
import EulerUtils (SplitInt, integralDigits)

--This is clumsy monadic code but I'm not sure on the clean way to write it yet
target :: IO String
target = do contents <- (readFile "Problem8.txt")
            pure $ filter (isDigit) contents

target2 = 13

-- Takes a number (as a string) and splits it into a list of m-length contiguous sequences of digits
numberSplitter :: String -> Int -> [SplitInt]
numberSplitter n m = [map (digitToInt) (take m (drop i n)) | i <- [0..(length n) - m]]

---

-- Despite the name, this is O(n*m) and since the numbers we're working with are relatively small (~10^13 at most) this works relatively well for us
naive :: String -> Int -> Int
naive n m = maximum (map product (numberSplitter n m))

{- We can sort the digits in each number into order (as multiplication is commutative) and then just compare to find which will have the biggest product...
   This is O(n + n(mlogm)) (if we use a non-radix sort) so probably worse than the 'naive' method... 
-}
sorter :: String -> Int -> Int
sorter n m = product (maximum (map sort (numberSplitter n m)))                 

getMaxProd f = do number <- target
                  pure $ f number target2
   
getMaxProdNaive = getMaxProd naive  
getMaxProdSorter = getMaxProd sorter

main = do prod <- getMaxProdNaive
          print prod