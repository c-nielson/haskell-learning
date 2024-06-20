toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:zs) = x:(y * 2):doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
    | x `div` 10 == 0 = x
    | otherwise = sumDigits (toDigits x)
sumDigits (x:y:zs)
    | x `div` 10 == 0 && y `div` 10 == 0 = x + y + sumDigits zs
    | otherwise = sumDigits (toDigits x ++ toDigits y ++ zs)

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start goal helper
    | n == 1 = [(start, goal)]
    | otherwise = hanoi (n - 1) start helper goal ++ [(start, goal)] ++ hanoi (n - 1) helper goal start