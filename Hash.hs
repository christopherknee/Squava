-- Borrowed from the CPSC 312 Hash.hs
module Hash where

class Hash t where
   hash :: t -> Int

arbMun = 0.5 * (sqrt(5) -1)  :: Double
numHashVals = 2^20  :: Double
fractionalPart :: Double -> Double
fractionalPart n = n- fromIntegral(floor n)

instance Hash Int where
   hash n = floor (numHashVals *fractionalPart(arbMun *fromIntegral n))

instance Hash t => Hash [t] where
   hash [] = 1741        -- any constant
   hash (h:t) = hash ( hash h + hash t)
