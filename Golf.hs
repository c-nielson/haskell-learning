{-# OPTIONS_GHC -Wall #-}

module Golf where

skips :: [a] -> [[a]]
skips [] = []
skips a = map (takeEvery a) [1..(length a)]
    where
        takeEvery xs n = case drop (n-1) xs of
            [] -> []
            x : ys -> x : takeEvery ys n

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima a = if length a >= 3 then
    foldl extractMaxima [] (map (getMaxima . dropN a) [0..length a - 3])
    else []

extractMaxima :: [Integer] -> Maybe Integer -> [Integer]
extractMaxima l m = case m of
    Just n -> l ++ [n]
    Nothing -> l

dropN :: [a] -> Int -> [a]
dropN a n = drop n a

getMaxima :: [Integer] -> Maybe Integer
getMaxima (x : y : z : _) = if x < y && y > z then Just y else Nothing
getMaxima _ = Nothing

histogram :: [Integer] -> String
histogram ns = unlines (map toString (chop (count ns))) ++ "==========\n0123456789\n"
    where
        count ns' = [length (filter (==n) ns') | n <- [0..9]]
        chop ns'' = reverse $ take (maximum ns'') $ iterate (map pred) ns''
        toString = map (\n -> if n > 0 then '*' else ' ')