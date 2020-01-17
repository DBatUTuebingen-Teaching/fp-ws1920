import Data.Maybe (maybe)
import Data.Char (ord)

-- Exc a is either an exception (Exc err) or a value (Val y)
data Exc a =
    Exc Error
  | Val a
  deriving (Show)

type Error = String

-- Instantiate the Functor-Applicative-Monad tower
instance Functor Exc where
-- ...

instance Applicative Exc where
-- ...

instance Monad Exc where
-- ...


-- A safe variant of (!!)
at :: Int -> [a] -> Exc a
at i xs | 0 <= i && i < length xs = Val (xs !! i)
        | otherwise               = Exc "list index out of bound"


numeralToDigit :: String -> Exc Char
numeralToDigit w = maybe (Exc "unknown numeral")
                         Val
                         (lookup w digits)
  where
    digits = [("null",  '0'),
              ("zero",  '0'),
              ("one",   '1'),
              ("two",   '2'),
              ("three", '3'),
              ("four",  '4'),
              ("five",  '5'),
              ("six",   '6'),
              ("seven", '7'),
              ("eight", '8'),
              ("nine",  '9')]

digitToVal :: Char -> Exc Int
digitToVal d | d `elem` ['0'..'9'] = Val (ord d - ord '0')
             | otherwise           = Exc "non-digit has no value"

chineseNumeral :: Int -> Exc Char
chineseNumeral n = at n "零一二三四五六七八九"


-- Translate English numeral n into a Chinese digit,
-- *if possible* (return an error message otherwise)
-- Variant using monadic bind operator (>>=)
chinese' :: String -> Exc Char
chinese' n = -- ...


main :: IO ()
main = print $ chinese' "five"
