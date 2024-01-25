{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Donut where

-- all trig in radians
-- gloss gui is perfect
-- compile with -O2 flag (level 2 optimizations)
-- all functions and operators not defined in this document are documented in Hoogle, the Haskell search engine.

newtype Vec3 a = Vec3 (a, a, a) deriving (Show, Functor, Foldable)
newtype Vec2 a = Vec2 (a, a)    deriving (Show, Functor, Foldable)

zipVec3With :: (a -> b -> c) -> Vec3 a -> Vec3 b -> Vec3 c
zipVec3With f (Vec3 (a1, a2, a3)) (Vec3 (b1, b2, b3)) 
  = Vec3 (f a1 b1, f a2 b2, f a3 b3)

zipVec2With :: (a -> b -> c) -> Vec2 a -> Vec2 b -> Vec2 c
zipVec2With f (Vec2 (a1, a2)) (Vec2 (b1, b2)) 
  = Vec2 (f a1 b1, f a2 b2)


instance Num a => Num (Vec3 a) where
  a + b  = zipVec3With (+) a b
  a - b  = zipVec3With (-) a b
  a * b  = zipVec3With (*) a b
  negate = fmap negate
  abs    = error "You called abs on a Vec3. Did you mean norm?"
  signum = error "You called signum on a Vec3."
  fromInteger x = error "You called fromInteger on a Vec3."

instance Num a => Num (Vec2 a) where
  a + b  = zipVec2With (+) a b
  a - b  = zipVec2With (-) a b
  a * b  = zipVec2With (*) a b
  negate = fmap negate
  abs    = error "You called abs on a Vec2. Did you mean norm?"
  signum = error "You called signum on a Vec2."
  fromInteger x = error "You called fromInteger on a Vec2."

i3 :: forall a. Floating a => Vec3 a
i3 = Vec3 (1, 0, 0)
j3 :: forall a. Floating a => Vec3 a
j3 = Vec3 (0, 1, 0)
k3 :: forall a. Floating a => Vec3 a
k3 = Vec3 (0, 0, 1)

i2 :: forall a. Floating a => Vec2 a
i2  = Vec2 (1, 0)
j2 :: forall a. Floating a => Vec2 a
j2  = Vec2 (0, 1)

dotProduct3 :: Num a => Vec3 a -> Vec3 a -> a
dotProduct3 w v = sum $ zipVec3With (*) w v

crossProduct3 :: Num a => Vec3 a -> Vec3 a -> Vec3 a
crossProduct3 (Vec3 (a1, a2, a3)) (Vec3 (b1, b2, b3)) 
  = Vec3 (a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1)

dotProduct2 :: Num a => Vec2 a -> Vec2 a -> a
dotProduct2 w v = sum $ zipVec2With (*) w v

crossProduct2 :: Num a => Vec2 a -> Vec2 a -> a
crossProduct2 (Vec2 (a1, a2)) (Vec2 (b1, b2))
  = a1 * b2 - a2 * b1

norm3 :: Vec3 Float -> Float
norm3 = sqrt . sum . fmap (^2)

norm2 :: Vec2 Float -> Float
norm2 = sqrt . sum . fmap (^2)

-- component and projection of v onto w
component3      :: Vec3 Float -> Vec3 Float -> Float
component3  w v =  dotProduct3 w v / norm3 w
projection3     :: Vec3 Float -> Vec3 Float -> Vec3 Float
projection3 w v = (dotProduct3 w v / norm3 w ^ 2 *) <$> w

rotateX :: Float -> Vec3 Float -> Vec3 Float
rotateX rθ (Vec3 (x, y, z)) = Vec3 (x, y * cos rθ - z * sin rθ, y * sin rθ + z * cos rθ)
rotateY :: Float -> Vec3 Float -> Vec3 Float
rotateY rθ (Vec3 (x, y, z)) = Vec3 (x * cos rθ - z * sin rθ, y, x * sin rθ + z * cos rθ)
rotateZ :: Float -> Vec3 Float -> Vec3 Float
rotateZ rθ (Vec3 (x, y, z)) = Vec3 (x * cos rθ - y * sin rθ, x * sin rθ + y * cos rθ, z)

-- Rodriguez' rotation formula (Used wikipedia to find formula. Is that cheating?)
rotate3 :: p -> Float -> Vec3 Float -> Vec3 Float
rotate3 rvec rθ v 
  = fmap (cos rθ *) v 
  + fmap (sin rθ *) (crossProduct3 k3 v)
  + fmap (dotProduct3 k3 v * (1 - cos rθ) *) k3

donut :: Float -> Float -> p -> Float -> Float -> Float -> Vec3 Float
donut r1 r2 rvec rθ θ1 θ2 
  = fmap ( (r1 + r2 * cos θ2) * cos θ1 * ) i'
  + fmap ( (r1 + r2 * cos θ2) * sin θ1 * ) j'
  + fmap ( r2 * sin θ2 * ) k'
  where
    i' = rotate3 rvec rθ i3
    j' = rotate3 rvec rθ j3
    k' = rotate3 rvec rθ k3

{- FINAL PLAN FOR THE PRIMARY DATA STRUCTURE:
I can have just the bare details necessary stored in a stream, and then feed those to a 
  function that generates a structure of the kind that Gloss takes, in addition to the shadings.
-}

