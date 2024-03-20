module FirstFifty where

import qualified Data.Monoid as M
import qualified Data.List as L
import qualified Data.Set as S

-- VIEW
main :: IO ()
main = printMyAnswers

-- problem [n]: [answer] is [correct | incorrect]
printMyAnswers :: IO ()
printMyAnswers = foldl (\ioAcc (pnum, answer) ->
  let nextLine = "problem " ++ show pnum ++ ": " ++ show answer ++ " is " ++ if checkAnswer pnum answer then "correct" else "incorrect"
  in ioAcc >> putStrLn nextLine) (return ()) answerList

checkAnswer :: Integer -> Integer -> Bool
checkAnswer n x = x == solution
  where solution = case n of
          1  -> 233168
          2  -> 4613732
          3  -> 6857
          4  -> 906609
          5  -> 232792560
          6  -> 25164150
          7  -> 104743
          8  -> 23514624000
          9  -> 31875000
          10 -> 142913828922
          11 -> 70600674
          12 -> 76576500
          13 -> 5537376230
          14 -> 837799
          15 -> 137846528820
          16 -> 1366
          17 -> 21124
          18 -> 1074
          19 -> 171
          20 -> 648
          21 -> 31626
          22 -> undefined
          23 -> undefined
          24 -> undefined
          25 -> undefined
          26 -> undefined
          27 -> undefined
          28 -> undefined
          29 -> undefined
          30 -> undefined
          _  -> error $ "You forgot to put problem " ++ show n ++ " in the checkAnswer solution cases."

-- SOLUTIONS
euler1 = sum . filter (combineWith (||) (3 `divides`) (5 `divides`)) $ [1..999]
euler2 = sum . filter even . takeWhile (<= 4000000) $ fibs
euler3 = highestPrimeFactor 600851475143
--   aE5 + bE4 + cE3 + cE2 + bE1 + a
-- = 100001a + 10010b + 1100c
-- = 11 * (9091a + 910b + 100c)
-- that is to say, 11 divides all 6 digit palindromes.
euler4 = maximum
  [ 11 * a * b | a <- [1..(div 999 11)]
               , b <- [1..999]
               , isPalindrome (11*a*b) ]
euler5 = smallestMultiple [1..20]
euler6 = -1
euler7 = -1
euler8 = -1
euler9 = -1
euler10 = -1
euler11 = -1
euler12 = -1
euler13 = -1
euler14 = -1
euler15 = -1
euler16 = -1
euler17 = -1
euler18 = -1
euler19 = -1
euler20 = -1
euler21 = -1

answerList :: [(Integer, Integer)]
answerList = [ (1, toInteger euler1)
             , (2, toInteger euler2)
             , (3, toInteger euler3)
             , (4, toInteger euler4)
             , (5, toInteger euler5)
             , (6, toInteger euler6)
             , (7, toInteger euler7)
             , (8, toInteger euler8)
             , (9, toInteger euler9)
             , (10, toInteger euler10)
             , (11, toInteger euler10)
             , (12, toInteger euler10)
             , (13, toInteger euler10)
             , (14, toInteger euler10)
             , (15, toInteger euler10)
             , (16, toInteger euler10)
             , (17, toInteger euler10)
             , (18, toInteger euler10)
             , (19, toInteger euler10)
             , (20, toInteger euler10)
             , (21, toInteger euler21) ]

-- HELPERS
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | n > 0  = n * factorial (n-1)
            | n < 1  = error "out of range error: factorial does not support negative numbers"

divides :: Integral a => a -> a -> Bool
divides d 0 = error "you modded by 0 in divides"
divides d n = mod n d == 0

highestPrimeFactor :: Integral t => t -> t
highestPrimeFactor n = divIncUntilPrime 2 n
  where
    divIncUntilPrime i n   | i * i > n    = n
                           | divides i n  = divIncUntilPrime 2 (div n i)
                           | otherwise    = divIncUntilPrime (i+1) n

combineWith :: (b -> c -> d) -> (a -> b) -> (a -> c) -> a -> d
combineWith zipper f g x = zipper (f x) (g x)

type DigitList = [Integer]
toDigitList :: Integer -> DigitList
toDigitList 0 = [0]
toDigitList n = reverse . toRevDigitList $ n
  where toRevDigitList 0 = []
        toRevDigitList n = mod n 10 : toRevDigitList (div n 10)

-- works fine
newtype SimpleMap a b = SimpleMap (S.Set a, a -> b)

-- works fine
instance (Show a, Show b) => Show (SimpleMap a b) where
  show map = foldl (\acc k ->
    acc ++ show k ++ " -> " ++ show (unsafeGetValue k map) ++ "\n") "" . S.toList . keys $ map

-- works fine
keys :: SimpleMap a b -> S.Set a
keys (SimpleMap (ks, _)) = ks -- keys should be sorted upon entry

-- works fine
unsafeGetValue :: a -> SimpleMap a b -> b
unsafeGetValue key (SimpleMap (_, f)) = f key

-- works fine
getValue :: Ord a => a -> SimpleMap a b -> Maybe b
getValue key smap = case (S.member key (keys smap)) of
  False -> Nothing
  True  -> Just $ unsafeGetValue key smap

-- works fine
applyToValue :: Ord a => (b -> b) -> a -> SimpleMap a b -> SimpleMap a b
applyToValue f key smap = SimpleMap (S.insert key (keys smap),
                          \k -> if k == key
                                then f $ unsafeGetValue k smap
                                else unsafeGetValue k smap)

-- works fine
insert :: Ord a => (a, b) -> SimpleMap a b -> SimpleMap a b
insert (key, value) smap = SimpleMap (S.insert key (keys smap), 
                           \k -> if k == key
                                 then value
                                 else unsafeGetValue k smap)

unsafeRemove :: Ord a => a -> SimpleMap a b -> SimpleMap a b
unsafeRemove key smap = SimpleMap (S.delete key (keys smap),
                        \k -> if k == key
                              then error "key not in SimpleMap"
                              else unsafeGetValue k smap)

-- works fine
emptyUnsafeGetValue :: a -> b
emptyUnsafeGetValue _ = error "key not in SimpleMap"

-- works fine
emptySimpleMap :: Ord a => SimpleMap a b
emptySimpleMap = SimpleMap (S.empty, emptyUnsafeGetValue)

-- works fine
concatSimpleMap :: Ord a => (b -> b -> b) -> SimpleMap a b -> SimpleMap a b -> SimpleMap a b
concatSimpleMap resolve smap1 smap2 =
  let unitedKeys = S.union (keys smap1) (keys smap2)
  in S.foldr (\key accMap -> 
    let value = case (getValue key smap1, getValue key smap2) of
                  (Just x, Just y)  -> resolve x y
                  (Just x, Nothing) -> x
                  (Nothing, Just y) -> y
    in insert (key, value) accMap) emptySimpleMap unitedKeys

primeDivisorCount :: Integer -> SimpleMap Integer Integer
primeDivisorCount n = thd $ until ((== 1) . fst) go (n, 0, emptySimpleMap)
  where
    fst (x,_,_) = x
    thd (_,_,x) = x 
    go (n, primeN, smap)
      | p `divides` n = (div n p, primeN, 
        case (S.member p (keys smap)) of
          True  -> applyToValue (+1) p smap
          False -> insert (p, 1) smap)
      | otherwise = (n, primeN + 1, smap)
      where p = primes !! fromIntegral primeN

smallestMultiple :: [Integer] -> Integer
smallestMultiple = weightedProduct . concatMaxes . map primeDivisorCount
  where
    concatMaxes :: [SimpleMap Integer Integer] -> SimpleMap Integer Integer
    concatMaxes = foldr (concatSimpleMap max) emptySimpleMap
    -- the error is in weightedProduct... who knows
    weightedProduct :: SimpleMap Integer Integer -> Integer
    weightedProduct smap | 0 == (S.size . keys $ smap) = 1
                         | otherwise =
      let key = S.elemAt 0 (keys smap)
      in key * unsafeGetValue key smap * weightedProduct (unsafeRemove key smap)

isPalindrome :: Integer -> Bool
isPalindrome n = 
  let dl = toDigitList n
  in dl == reverse dl

properDivisors :: Integer -> [Integer]
properDivisors n = [ i | i <- [1..(div n 2)], divides i n ]

primes :: [Integer]
primes = 2 : filter isPrime [3..]

isPrime :: Integer -> Bool
isPrime n = all (\x -> mod n x /= 0) $ takeWhile (<= div n 2) primes
