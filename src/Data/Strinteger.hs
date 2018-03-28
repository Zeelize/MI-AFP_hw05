module Data.Strinteger where

import Data.Maybe
-- You might need to use intercalate and splitOn (similar to words/unwords)
import Data.List (intercalate)
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
    | otherwise = Just $ (SH.negativePrefix ++ [SH.separator] ++ finalString)
    where 
        finalString = intercalate [SH.separator] (scalesCompute (abs n) (reverse SH.scales))
        scalesCompute :: Integer -> [(Integer, String)] -> [String]
        scalesCompute 0 _ = []
        scalesCompute n [] = [intercalate [SH.separatorTens] (tensCompute n (reverse SH.tens))]
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
engNumeral2Integer str = case stringSpaces (splitOn [SH.separator] str) of
    Just n -> Just n
    _ -> Nothing
    where 
        stringSpaces :: [String] -> Maybe Integer
        stringSpaces [] = Nothing
        stringSpaces [x] = case stringDash (splitOn [SH.separatorTens] x) of
            Just n -> Just n
            _ -> Nothing
            where 
                stringDash :: [String] -> Maybe Integer
                stringDash [x] = wordExport x
                stringDash [x,y] = couplesExport x y
        stringSpaces (x:y:[]) = couplesExport x y
        stringSpaces (x:y:xs) = restExport (couplesExport x y) xs

        restExport :: Maybe Integer -> [String] -> Maybe Integer
        restExport ce xs = case (ce, stringSpaces xs) of
            (Just val1, Just val2) -> Just (val1 + val2)
            _ -> Nothing
        
        couplesExport :: String -> String -> Maybe Integer
        couplesExport x y = case (stringSpaces [x], wordExport y) of
            (Just val1, Just val2) -> Just (val1 + val2)
            _ -> Nothing

        wordExport :: String -> Maybe Integer
        wordExport str = case SH.word2num str of
            Just (1, v) -> Just v
            Just (10, v) -> Just (10 * v)
            Just (s, 0) -> Just (10 ^ s) 
            _ -> Nothing
        

-- TODO: implement Strinteger instances of Num, Ord, Eq, Enum, Real, and Integral
instance Eq Strinteger where
    (==) (Strinteger s1) (Strinteger s2) = s1 == s2

instance Ord Strinteger where
    compare = undefined

instance Num Strinteger where
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance Enum Strinteger where
    toEnum = undefined
    fromEnum = undefined

instance Real Strinteger where
    toRational = undefined

instance Integral Strinteger where
    quotRem = undefined
    toInteger = undefined
