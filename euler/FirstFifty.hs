module Main where

import Data.Semigroup
import Data.Monoid
import System.IO
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Set as S
import GHC.Float

-- This module uses a convoluted, messy effect system where it feeds inputList to
-- answerList, and then folds the answers and solutions into cohesive output.
-- Although it's fragile, breakable, inefficient, and unscalable due to shoelace and bubble gum
-- data types being used, it works for now and shouldn't have to scale up until way later on 
-- ... that said, I will be redesigning the IO system from the ground up in the SecondFifty 
-- module of these Euler solutions, ideally without the SimpleMap data type.
--
-- TODO: replace SimpleMap with Data.Map.Strict

-- DEBUG AREA

-- VIEW
main :: IO ()
main = printAnswers

-- problem [n]: [answer] is ["correct" | "incorrect"]
printAnswers :: IO ()
printAnswers = 
  do im <- inputMap
     al <- answerList im
     L.foldl' (ioShowLine al) (return ()) al
  where 
    ioShowLine al ioAcc (pnum, answer) =
      let maxLength = maximum $ map (\ (x, y) -> numLength x + numLength y) al
          nextLine = "problem " ++ show pnum ++ ": " ++ show answer 
                     ++ stimesMonoid (maxLength - numLength answer - numLength pnum) " " ++ " is " 
                     ++ if (checkAnswer pnum answer) then "correct" else "incorrect"
      in ioAcc >> putStrLn nextLine

-- takes a SimpleMap as input. returns an IO [(Integer, Integer)] as output.
--
answerList :: SimpleMap Integer String -> IO [(Integer, Integer)]
answerList inputMap = return $ foldr 
  (\ (n, f) acc -> case (contains n inputMap) of
                     True  -> (n, f (unsafeGetValue n inputMap)) : acc
                     False -> (n, f "") : acc) []
  [ (1,  toInteger . euler1),  (2,  toInteger . euler2),  (3,  toInteger . euler3)
  , (4,  toInteger . euler4),  (5,  toInteger . euler5),  (6,  toInteger . euler6)
  , (7,  toInteger . euler7),  (8,  toInteger . euler8),  (9,  toInteger . euler9)
  , (10, toInteger . euler10), (11, toInteger . euler11), (12, toInteger . euler12)
  , (13, toInteger . euler13), (14, toInteger . euler14), (15, toInteger . euler15)
  , (16, toInteger . euler16), (17, toInteger . euler17), (18, toInteger . euler18)
  , (19, toInteger . euler19), (20, toInteger . euler20), (21, toInteger . euler21) 
  , (22, toInteger . euler22), (23, toInteger . euler23), (24, toInteger . euler24)
  , (25, toInteger . euler25), (26, toInteger . euler26), (27, toInteger . euler27) ]

inputMap :: IO (SimpleMap Integer String)
inputMap = (=<<) (return . fromList) $ sequence . map seqPair2 $ 
  [ (8, readFile "data/e8_longint.txt")
  , (22, readFile "data/e22_names.txt") ]

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
euler1 _  = sum . filter (\x -> divisibleBy 3 x || divisibleBy 5 x) $ [1..999]
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
euler7 _  = primes !! 10000 -- firstNPrimes 10001 !! 10000
euler8    = maxAdjacentProduct 13 . map read . map (:[]) . filter C.isDigit -- too large, check maxAdjacentProduct
euler9  _ = 0
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
euler22   = sum . locWeightTransform 1 . fmap alphaValue . L.sort . parse
  where 
    locWeightTransform _ []     = []
    locWeightTransform n (x:xs) = n*x : locWeightTransform (n+1) xs
    alphaValue = toInteger . sum . fmap (\x -> fromEnum x - fromEnum 'A' + 1)
    parse = filter (/= []) . split ',' . filter (\c -> C.isAlpha c || (c == ','))
euler23 _ = 0
euler24 _ = 0
euler25 _ = 0
euler26 _ = 0
euler27 _ = 0

-- HELPERS
numLength :: Integer -> Integer
numLength = toInteger . length . show

maxAdjacentProduct :: Integer -> [Integer] -> Integer
maxAdjacentProduct n = fst . foldr (\x (m, xs) ->
  case (toInteger (length xs) < n, x > head xs) of
    (True, _)      -> (m, xs ++ [x])          
    (False, True)  -> let xs' = tail xs ++ [x]    
                      in (max m (product xs'), xs')
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

split :: Eq a => a -> [a] -> [[a]]
split del [] = []
split del xs = let (h,t) = span (/= del) xs
               in case t of 
                 [] -> h : []
                 _  -> h : split del (tail t)
                    

-- works fine
newtype SimpleMap a b = SimpleMap (S.Set a, a -> b)

-- works fine
instance (Show a, Show b) => Show (SimpleMap a b) where
  show map = foldl (\acc k ->
    acc ++ show k ++ " -> " ++ show (unsafeGetValue k map) ++ "\n") "" . S.toList . keys $ map

-- works fine
keys :: SimpleMap a b -> S.Set a
keys (SimpleMap (ks, _)) = ks -- keys should be sorted upon entry

contains :: Ord a => a -> SimpleMap a b -> Bool
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

fromList :: Ord a => [(a, b)] -> SimpleMap a b
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

seqPair2 :: Monad m => (a, m b) -> m (a, b)
seqPair2 (x, my) = my >>= \y -> return (x, y)

primesBelow :: Integer -> [Integer]
primesBelow n = snd $ until (\ (i, ps) -> i >= (toInteger (length ps)))
  (\ (i, ps) -> let i' = fromInteger i 
                in (i + 1, take i' ps ++ [ps !! i'] ++ filter (not . divisibleBy (ps !! i')) (drop (i' + 1) ps))) (0, [2..n])
  where until p f = go
          where
            go !x | p x          = x
                  | otherwise    = go (f x)

-- seems like there's probably some space time fuckery going on ....
firstNPrimes :: Integer -> [Integer]
firstNPrimes n |                 n < 0       = error "firstNPrimes takes only positive input"
               | 0       <= n && n < 6       = take (fromInteger n) [2, 3, 5, 7, 11]
               | 6       <= n && n < 20      = let n' = fromInteger n in
                                               take (fromInteger n) . primesBelow . floorFloat $ 
                                               n' * (log n' + log (log n'))
               | 20      <= n && n < 8009824 = let n' = fromInteger n in
                                               take (fromInteger n) . primesBelow . floorFloat $ 
                                               n' * (log n' + log (log n') - 0.5)
               | 8009824 <= n                = let n' = fromInteger n in
                                               take (fromInteger n) . primesBelow . floorFloat $ 
                                               n' * (log n' + log (log n') - 1
                                               + (log (log n') - 2) / log n' 
                                               - (log (log n') ^ 2 - 6 * log (log n') + 10.273) 
                                               / (2 * log n' ^ 2))

-- p*p because iteratively, xs has been sieved by all primes less than p,
-- and so p*p is the next composite number.
primes :: [Integer]
primes = 2 : sieve primes [3..] 
    where 
    sieve (p:pt) xs = let (h,t) = span (< p*p) xs 
                      in h ++ sieve pt [x | x <- t, rem x p > 0]

{-
primes :: [Integer]
primes = 2 : filter isPrime [3..]

isPrime :: Integer -> Bool
isPrime n = all (\x -> mod n x /= 0) $ takeWhile (<= div n 2) primes
-}
