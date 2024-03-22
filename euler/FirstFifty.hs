module FirstFifty where

import Data.Semigroup
import Data.Monoid
import System.IO
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.String as Str

-- This module uses a convoluted, messy IO system where it feeds inputList to
-- answerList, and then folds the answers and solutions into cohesive output.
-- Although it's fragile, breakable, inefficient, and unscalable due to shoelace and bubble gum
-- data types being used, it works for now and shouldn't have to scale up until way later on 
-- ... that said, I will be redesigning the IO system from the ground up in the SecondFifty 
-- module of these Euler solutions, ideally without the SimpleMap data type.

-- VIEW
main :: IO ()
main = printAnswers

-- problem [n]: [answer] is [correct | incorrect]
printAnswers :: IO ()
printAnswers = (inputMap >>= answerList) >>= L.foldl' (\ioAcc (pnum, answer) ->
    let nextLine = "problem " ++ show pnum ++ ": " ++ show answer 
                   ++ stimesMonoid (maxLength - numLength answer) " " ++ " is " 
                   ++ if (checkAnswer pnum answer) then "correct" else "incorrect"
    in ioAcc >> putStrLn nextLine) (return ())
    where numLength = toInteger . length . show
          -- the problem lies here
          maxLength = maximum . map (\(x, y) -> numLength x + numLength y) $ al

answerList :: SimpleMap Integer String -> [(Integer, Integer)]
answerList inputMap = foldr (\(n, f) acc -> 
                                if contains n inputMap
                                then (n, f (getValue inputMap)) : acc
                                else (n, f _) : acc) []
  [ (1,  toInteger . euler1),  (2,  toInteger . euler2),  (3,  toInteger . euler3)
  , (4,  toInteger . euler4),  (5,  toInteger . euler5),  (6,  toInteger . euler6)
  , (7,  toInteger . euler7),  (8,  toInteger . euler8),  (9,  toInteger . euler9)
  , (10, toInteger . euler10), (11, toInteger . euler10), (12, toInteger . euler10)
  , (13, toInteger . euler10), (14, toInteger . euler10), (15, toInteger . euler10)
  , (16, toInteger . euler10), (17, toInteger . euler10), (18, toInteger . euler10)
  , (19, toInteger . euler10), (20, toInteger . euler10), (21, toInteger . euler21) 
  , (22, toInteger . euler10), (23, toInteger . euler10), (24, toInteger . euler10)
  , (25, toInteger . euler10), (26, toInteger . euler10), (27, toInteger . euler10) ]

inputMap :: IO (SimpleMap Integer String)
inputMap = sequence . map seqPair2 $ 
  [ (8, readFile "e8_longint.txt") ]

checkAnswer :: Integer -> Integer -> Bool
checkAnswer n x = x == solution
  where solution = case n of
          1  -> 233168;         2  -> 4613732;          3  -> 6857
          4  -> 906609;         5  -> 232792560;        6  -> 25164150
          7  -> 104743;         8  -> 23514624000;      9  -> 31875000
          10 -> 142913828922;   11 -> 70600674;         12 -> 76576500
          13 -> 5537376230;     14 -> 837799;           15 -> 137846528820
          16 -> 1366;           17 -> 21124;            18 -> 1074
          19 -> 171;            20 -> 648;              21 -> 31626
          22 -> 1;              23 -> 1;                24 -> 1        
          25 -> 1;              26 -> 1;                27 -> 1        
          28 -> 1;              29 -> 1;                30 -> 1        
          _  -> error $ "You forgot to put problem " ++ show n ++ " in the checkAnswer solution cases."

-- SOLUTIONS
euler1 _  = sum . filter (divisibleBy 3) . filter (divisibleBy 5) $ [1..999]
euler2 _  = sum . filter even . takeWhile (<= 4000000) $ fibs
euler3 _  = highestPrimeFactor 600851475143
--   a*10^5 + b*10^4 + c*10^3 + c*10^2 + b*10^1 + a
-- = 100001a + 10010b + 1100c
-- = 11 * (9091a + 910b + 100c)
-- that is to say, 11 divides all 6 digit palindromes.
euler4 _  = maximum
  [ 11 * a * b | a <- [1..(div 999 11)]
               , b <- [1..999]
               , isPalindrome (11*a*b) ]
euler5 _  = smallestMultiple [1..20]
euler6 _  = squaresDiff 100
  where squaresDiff x = (3*x^4 + 2*x^3 - 3*x^2 - 2*x) `div` 12
euler7 _  = primes !! 10000
euler8 d  = maxAdjacentProduct 13 . map read . filter C.isDigit $ d
euler9 _  = 0
euler10 _ = 0
euler11 _ = 0
euler12 _ = 0
euler13 _ = 0
euler14 _ = 0
euler15 _ = 0
euler16 _ = 0
euler17 _ = 0
euler18 _ = 0
euler19 _ = 0
euler20 _ = 0
euler21 _ = 0
euler22 _ = 0
euler23 _ = 0
euler24 _ = 0
euler25 _ = 0
euler26 _ = 0
euler27 _ = 0

-- HELPERS

maxAdjacentProduct :: Integer -> Integer
maxAdjacentProduct n = fst . foldr (\x (m, xs) ->
  case (length xs < n, x > head xs) of
    (True, _)      -> (m, x : xs ++ [x])          
    (False, True)  -> let xs' = tail xs ++ [x]    
                      in (max m xs', xs')
    (False, False) -> (m, tail xs ++ [x])) (0, [])

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | n > 0  = n * factorial (n-1)
            | n < 1  = error "out of range error: factorial does not support negative numbers"

divides :: Integral a => a -> a -> Bool
divides d 0 = error "you modded by 0 in divides"
divides d n = mod n d == 0
divisibleBy = divides

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
toDigitList = map read . map (: []) . show . abs

-- works fine
newtype SimpleMap a b = SimpleMap (S.Set a, a -> b)

-- works fine
instance (Show a, Show b) => Show (SimpleMap a b) where
  show map = foldl (\acc k ->
    acc ++ show k ++ " -> " ++ show (unsafeGetValue k map) ++ "\n") "" . S.toList . keys $ map

-- works fine
keys :: SimpleMap a b -> S.Set a
keys (SimpleMap (ks, _)) = ks -- keys should be sorted upon entry

contains :: a -> SimpleMap a b -> Bool
contains x smap = S.member x (keys smap)

-- works fine
unsafeGetValue :: a -> SimpleMap a b -> b
unsafeGetValue key (SimpleMap (_, f)) = f key

-- works fine
getValue :: Ord a => a -> SimpleMap a b -> Maybe b
getValue key smap = case (contains key smap) of
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

fromList :: [(a, b)] -> SimpleMap a b
fromList = foldr insert emptySimpleMap

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
        case (contains p smap) of
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
    weightedProduct smap = S.foldr (\key product ->
      let value = unsafeGetValue key smap
      in product * key ^ value) 1 (keys smap)

isPalindrome :: Integer -> Bool
isPalindrome n = 
  let dl = toDigitList n
  in dl == reverse dl

properDivisors :: Integer -> [Integer]
properDivisors n = [ i | i <- [1..(div n 2)], divides i n ]

primes :: [Integer]
primes = 2 : filter isPrime [3..]

seqPair2 :: (a, m b) -> m (a, b)
seqPair2 (x, my) = my >>= \y -> return (x, y)

-- O( n * pi(n) )
isPrime :: Integer -> Bool
isPrime n = all (\x -> mod n x /= 0) $ takeWhile (<= div n 2) primes
