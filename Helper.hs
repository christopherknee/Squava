module Helper where

delete x lst = filter (/= x) lst

isValidCoord :: Int -> Int -> Bool
isValidCoord i1 i2 =
    (i1 >= 65 && i1 <= 69) && 
    (i2 >= 49 && i2 <= 53)

coordToInt :: String -> Maybe Int
coordToInt [c1,c2]
    | isValidCoord i1 i2    = Just ((i1-65) + (5*(i2-49)))
    | otherwise             = Nothing
    where
        i1 = fromEnum c1
        i2 = fromEnum c2
coordToInt _ = Nothing

intToCoord :: Int -> String
intToCoord i = [c1,c2]
    where 
        c1 = toEnum ((mod i 5) + 65) :: Char
        c2 = toEnum ((div i 5) + 49) :: Char