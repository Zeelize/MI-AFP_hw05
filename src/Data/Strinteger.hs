module Data.Strinteger where

import Data.Maybe
-- You might need to use intercalate and splitOn (similar to words/unwords)
import Data.List (intercalate, elemIndex, null)
import Data.List.Split (splitOn)

-- Use Data.Strinteger.Helpers submodule
-- In case of need, feel free to change or enhance Helpers or create own
-- submodule
--
-- DO NOT HARDCODE ANY STRINGs/CHARs IN THIS MODULE!
import qualified Data.Strinteger.Helpers as SH

-- | Strinteger type (wrapper) for English numerals
newtype Strinteger = Strinteger String
                   deriving (Show, Read)

instance Bounded Strinteger where
   maxBound = pack SH.highestPossible
   minBound = negate maxBound

   -- | Pack Integer into Strinteger (English numeral string)
pack :: Integer -> Strinteger
pack integer = Strinteger $ fromMaybe err (integer2EngNumeral integer)
               where
                 err = error $ SH.messageBadInteger integer

-- | Unpack Strinteger (English numeral string) to Integer
unpack :: Strinteger -> Integer
unpack (Strinteger numeral) = fromMaybe err (engNumeral2Integer numeral)
                              where
                                err = error $ SH.messageBadNumeral numeral


-- | Translate Integer to String (if possible)
-- TODO: implement Integer->String translation
integer2EngNumeral :: Integer -> Maybe String
integer2EngNumeral 0 = Just SH.zero
integer2EngNumeral n 
    | abs n > SH.highestPossible = Nothing
    | n > 0 = Just $ finalString
    | otherwise = Just $ (SH.negativePrefix ++ SH.separator ++ finalString)
    where 
        finalString = intercalate SH.separator (scalesCompute (abs n) (reverse SH.scales))
        scalesCompute :: Integer -> [(Integer, String)] -> [String]
        scalesCompute 0 _ = []
        scalesCompute n [] = [intercalate SH.separatorTens (tensCompute n (reverse SH.tens))]
            where 
                tensCompute :: Integer -> [(Integer, String)] -> [String]
                tensCompute 0 _ = []
                tensCompute n [x] = unitsCompute n (reverse SH.units)
                    where
                        unitsCompute :: Integer -> [(Integer, String)] -> [String]
                        unitsCompute 0 _ = []
                        unitsCompute n [] = []
                        unitsCompute n ((f,s):xs) 
                            | n - f == 0 = [s]
                            | otherwise = unitsCompute n xs
                
                tensCompute n ((f,s):xs) 
                    | div n (10 * f) > 1 = tensCompute (div n (10 * f)) xs ++ [s] ++ tensCompute (mod n (10 * f)) xs
                    | div n (10 * f) == 1 = [s] ++ tensCompute (mod n (10 * f)) xs
                    | otherwise = tensCompute (mod n (10 * f)) xs
        
        scalesCompute n ((f,s):xs) 
            | div n (10 ^ f) > 0 = scalesCompute (div n (10 ^ f)) xs ++ [s] ++ scalesCompute (mod n (10 ^ f)) xs
            | otherwise = scalesCompute (mod n (10 ^ f)) xs 

-- | Translate String to Integer (if possible)
-- TODO: implement String->Integer translation
engNumeral2Integer :: String -> Maybe Integer
engNumeral2Integer "zero" = Just 0
engNumeral2Integer str = case stringScales (splitOn SH.separator str) (reverse SH.scales) of
    Just n -> Just n
    _ -> Nothing
    where 
        stringScales :: [String] -> [(Integer, String)] -> Maybe Integer
        stringScales [] _ = Nothing
        stringScales [n] [] = stringTens (splitOn SH.separatorTens n) (reverse SH.tens)
            where 
                stringTens :: [String] -> [(Integer, String)] -> Maybe Integer
                stringTens [n] ((f,s):xs) 
                    | n == s = Just (10 * f)
                    | otherwise = stringTens [n] xs
                
                stringTens [n,m] xs = case (stringTens [n] xs, stringTens [m] xs) of
                    (Just val1, Just val2) -> Just (val1 + val2)
                    _ -> Nothing 
                
                stringTens n [] = stringUnits n (reverse (drop 1 SH.units))
                    where
                        stringUnits :: [String] -> [(Integer, String)] -> Maybe Integer
                        stringUnits [n] ((f,s):xs)
                            | n == s = Just f
                            | otherwise = stringUnits [n] xs
                        stringUnits _ _ = Nothing
                
                stringTens _ _ = Nothing
        
        stringScales _ [] = Nothing
        stringScales ("minus":xs) sc = case stringScales xs sc of
            Just n -> Just (-n)
            _ -> Nothing
            
        stringScales n ((f,s):xs) = case elemIndex s n of
            Just i -> case (stringScales (take i n) xs, stringScales rightSide xs) of
                (Just val1, Just val2) -> Just (val1 * (10 ^ f) + val2)
                (Just val1, _) -> case null rightSide of
                    True -> Just (val1 * (10 ^ f))
                    _ -> Nothing                                        
                _ -> Nothing
                where 
                    rightSide = drop (i + 1) n                       
            _ -> stringScales n xs            

-- TODO: implement Strinteger instances of Num, Ord, Eq, Enum, Real, and Integral
instance Eq Strinteger where
    (==) (Strinteger s1) (Strinteger s2) = s1 == s2

instance Ord Strinteger where
    compare s1 s2 = compare (unpack s1) (unpack s2)

instance Num Strinteger where
    (+) s1 s2 = pack $ (unpack s1) + (unpack s2)
    (*) s1 s2 = pack $ (unpack s1) * (unpack s2)
    negate s = pack $ negate (unpack s) 
    abs s = pack $ abs (unpack s) 
    signum s 
        | unpack s > 0 = pack 1
        | unpack s < 0 = pack (-1)
        | otherwise = pack 0
    fromInteger n = pack n

instance Enum Strinteger where
    toEnum s = pack $ fromIntegral s
    fromEnum s = fromIntegral $ unpack s

instance Real Strinteger where
    toRational s = toRational $ (unpack s)

instance Integral Strinteger where
    quotRem n1 n2 = (pack fQ, pack sR)
        where 
            val1 = unpack n1
            val2 = unpack n2
            fQ = quot val1 val2
            sR = rem val1 val2

    toInteger n = unpack n
