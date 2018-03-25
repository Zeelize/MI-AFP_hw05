module Data.Shapes where

--------------------------------------------------------------------------------
-- DO NOT CHANGE DATA TYPES DEFINITIONS

newtype Circle = Circle { ciRadius :: Double }
               deriving (Show, Read, Eq)

data Triangle = EquilateralTriangle { etSide :: Double }
              | IsoscelesTriangle { itBase :: Double, itLeg :: Double }
              | ScaleneTriangle { stSideA :: Double, stSideB :: Double, stSideC :: Double }
              deriving (Show, Read, Eq)

data Quadrilateral = Square { sqSide :: Double}
                   | Rectangle { reSideA :: Double, reSideB :: Double }
                   deriving (Show, Read, Eq)

--------------------------------------------------------------------------------

class Validable a where
  valid :: a -> Bool

-- TODO: complete instances for each type to check validity by `valid` function
instance Validable Circle where
  valid (Circle r) = r > 0 

instance Validable Triangle where
  valid (EquilateralTriangle a) = a > 0
  valid (IsoscelesTriangle a b) = a + b > b && b + b > a && a > 0 && b > 0
  valid (ScaleneTriangle a b c) = a + b > c && a + c > b && b + c > a && a > 0 && b > 0 && c > 0

instance Validable Quadrilateral where
  valid (Square a) = a > 0
  valid (Rectangle a b) = a > 0 && b > 0

-- TODO: create appropriate typeclass for 2D shapes (subclass of Validable)
-- TODO: write instances for the types to compute circumference and area
class (Validable a) => (Shapes2D a) where
  area :: a -> Double
  area p 
    | valid p = areaCalcul p
    | otherwise = 0.0
  circumference :: a -> Double
  circumference p 
    | valid p = circum p
    | otherwise = 0.0
  areaCalcul :: a -> Double
  circum :: a -> Double

instance Shapes2D Circle where
  areaCalcul (Circle r) = pi * r * r
  circum (Circle r) = 2 * pi * r

instance Shapes2D Triangle where
  areaCalcul (EquilateralTriangle a) = (1 / 4) * (sqrt 3) * (a ^ 2)
  areaCalcul (IsoscelesTriangle a b) = areaCalcul (ScaleneTriangle a b b)
  areaCalcul (ScaleneTriangle a b c) = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2
  circum (EquilateralTriangle a) = 3 * a
  circum (IsoscelesTriangle a b) = a + 2 * b
  circum (ScaleneTriangle a b c) = a + b + c

instance Shapes2D Quadrilateral where
  areaCalcul (Square a) = a * a
  areaCalcul (Rectangle a b) = a * b
  circum (Square a) = 4 * a
  circum (Rectangle a b) = 2 * a + 2 * b
   