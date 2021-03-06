module Rand where

import Prelude

-- | 0 <= val < 1e4, 0 <= gen < 1e8, seed is a big odd integer 
type Rand = {val :: Int, gen :: Int, seed :: Int}

-- | takes the 1e4 int at the middle of a 1e8 int
middle :: Int -> Int
middle nn = 
  let n0 = nn `mod` 100
      n3' = nn `mod` 1000000
      n3 = nn - ((nn-n3') `div` 1000000) * 1000000 
   in (n3-n0) `div` 100 

-- | iterates the pseudo random generation of a value 
-- | between 0 and 9999
rand :: Rand -> Rand
rand {val, gen, seed} = 
  { val: middle $ (val * val + gen) `mod` 100000000
  , gen: (gen + seed) `mod` 100000000
  , seed}

-- | generates an array of n PRN between 0 and 9999
rands :: Int -> Rand -> Array Int
rands 0 r = [r.val]
rands n r = [r.val] <> rands (n-1) (rand r)
