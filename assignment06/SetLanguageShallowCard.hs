module SetLanguageShallowCard (IntegerSet,
                               empty,
                               insert,
                               delete,
                               member,
                               card) where

data IntegerSet = IS (Integer -> Bool) Int -- characteristic function + cardinality

-- constructors
empty :: IntegerSet
empty = IS (\_ -> False)
           0
-- empty = IS (const False) 0

insert :: IntegerSet -> Integer -> IntegerSet
insert xs@(IS f c) x = IS (\y -> x == y || f y)
                          (if member xs x then c else c + 1)

delete :: IntegerSet -> Integer -> IntegerSet
delete xs@(IS f c) x = IS (\y -> y /= x && f y)
                          (if member xs x then c - 1 else c)

-- observer
member :: IntegerSet -> Integer -> Bool
member (IS f _) x = f x
-- member (IS f _) = f

card :: IntegerSet -> Int
card (IS _ c) = c
