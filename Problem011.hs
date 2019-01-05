import Data.Char (digitToInt)

-- Read in grid of numbers
target :: IO [[Int]]
target = do
            ls <- lines <$> (readFile "Problem011.txt")
            let ws = map words ls
            pure $ map (map (read)) ws            
            
-- Just calculate all the products from the grid, find max
mProd :: [[Int]] -> Int
mProd g = let 
              rows  = maximum [product [(g !! y) !! (x+a) | a <- [0..3]] | x <- [0 .. xlen - 4], y <- [0 .. ylen - 1]]
              
              cols  = maximum [product [(g !! (y+a)) !! x | a <- [0..3]] | x <- [0 .. xlen - 1], y <- [0 .. ylen - 4]]
              
              ldiag = maximum [product [(g !! (y+a)) !! (x+a) | a <- [0..3]] | x <- [0 .. xlen - 4], y <- [0 .. ylen - 4]]
              
              rdiag = maximum [product [(g !! (y+a)) !! (x-a) | a <- [0..3]] | x <- [xlen-1, xlen-2 .. 4], y <- [0 .. ylen - 4]]
          
          in  max (max rows cols) (max ldiag rdiag)
                where xlen = length (g !! 0)
                      ylen = length g

ans = mProd <$> target
        
main = ans >>= print