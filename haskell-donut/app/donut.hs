module Donut where

-- all trig in radians
-- gloss gui is perfect
-- compile with -O2 flag (level 2 optimizations)
-- all functions and operators not defined in this document are documented in 
--   Hoogle, the Haskell search engine.

data Vec3 a = Vec3 a a a deriving (Show, Functor, Foldable)

zipVecWith :: (a -> b -> c) -> Vec3 a -> Vec3 b -> Vec3 c
zipVecWith f (Vec3 a1 a2 a3) (Vec3 b1 b2 b3) =
  Vec3 (f a1 b1) (f a2 b2) (f a3 b3)

instance Num a => Num (Vec3 a) where
  a + b  = zipVecWith (+) a b
  a - b  = zipVecWith (-) a b
  a * b  = zipVecWith (*) a b
  negate = fmap negate
  abs    = undefined
  signum = undefined
  fromInteger x = Vec3 (fromInteger x) 0 0

i = Vec3 1 0 0
j = Vec3 0 1 0
k = Vec3 0 0 1

dotProduct w v = sum $ zipVecWith (*) w v
crossProduct (Vec3 a1 a2 a3) (Vec3 b1 b2 b3) =
  Vec3 (a2 * b3 - a3 * b2) (a3 * b1 - a1 * b3) (a1 * b2 - a2 * b1)

norm = sqrt . sum . fmap (^2)

-- component and projection of v onto w
component  w v =  dotProduct w v / norm w
projection w v = (dotProduct w v / norm w ^ 2 *) <$> w

rotateX rθ (Vec3 x y z) = Vec3 x (y * cos rθ - z * sin rθ) (y * sin rθ + z * cos rθ)
rotateY rθ (Vec3 x y z) = Vec3 (x * cos rθ - z * sin rθ) y (x * sin rθ + z * cos rθ)
rotateZ rθ (Vec3 x y z) = Vec3 (x * cos rθ - y * sin rθ) (x * sin rθ + y * cos rθ) z

-- Rodriguez' rotation formula (Used wikipedia to find formula. Is that cheating?)
rotate' rvec rθ v 
  = fmap (cos rθ *) v 
  + fmap (sin rθ *) (crossProduct k v)
  + fmap ((dotProduct k v) * (1 - cos rθ) *) k

donut r1 r2 rvec rθ θ1 θ2 
  = fmap ( (r1 + r2 * cos θ2) * cos θ1 * ) i'
  + fmap ( (r1 + r2 * cos θ2) * sin θ1 * ) j'
  + fmap ( r2 * sin θ2 * ) k'
  where
    i' = rotate' rvec rθ i
    j' = rotate' rvec rθ j
    k' = rotate' rvec rθ k

{- FINAL PLAN FOR THE PRIMARY DATA STRUCTURE:
I can have just the bare details necessary stored in a stream, and then feed those to a 
  function that generates a structure of the kind that Gloss takes, in addition to the shadings.
-}

