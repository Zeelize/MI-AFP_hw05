module Data.Logging where

import Data.Time.Clock
import Data.List

data LogLevel = Debug | Info | Warning | Error | Fatal
              deriving (Show, Read, Eq, Ord, Enum, Bounded)

data EventSource = Internal { iesComponent   :: String
                            , iesCallID      :: String }
                 | External { eesURI         :: String
                            , eesDescription :: String }
                 | Combined [EventSource]
                 | Unknown
                 deriving (Read, Eq)
                 -- TODO: remove Ord here after implementing Ord for LogMessage

instance Show EventSource where
    show (Internal c _) = "Internal[" ++ c ++ "]"
    show (External u _) = "External[" ++ u ++ "]"
    show Unknown = "Unknown"
    show (Combined (x:xs)) = "Combined[" ++ show x ++ showCombined xs ++ "]"
        where 
            showCombined :: [EventSource] -> String
            showCombined [] = ""
            showCombined (x:xs) = "," ++ show x ++ showCombined xs

data LogMessage = LogMessage
                { lmSource     :: EventSource
                , lmMessage    :: String
                , lmTimestamp  :: UTCTime
                , lmHiddenFlag :: Bool
                , lmLogLevel   :: LogLevel
                } deriving (Read, Eq)
                -- TODO: custom instance of Show and Ord (hidden, timestamp, logLevel)

instance Show LogMessage where
    show (LogMessage s m _ _ l) = "[" ++ show l ++ "] " ++ show s ++ ": " ++ m  

instance Ord LogMessage where
    --compare lm1 lm2 = compare lm1 lm2 
    (<) (LogMessage _ _ t1 f1 l1) (LogMessage _ _ t2 f2 l2) 
        | f1 /= f2 = f1 > f2
        | l1 /= l2 = l1 < l2
        | otherwise = t1 < t2
    (<=) (LogMessage _ _ t1 f1 l1) (LogMessage _ _ t2 f2 l2)
        | f1 /= f2 = f1 > f2
        | l1 /= l2 = l1 <= l2
        | otherwise = t1 <= t2
    (>) (LogMessage _ _ t1 f1 l1) (LogMessage _ _ t2 f2 l2) 
        | f1 /= f2 = f1 < f2
        | l1 /= l2 = l1 > l2
        | otherwise = t1 > t2
    (>=) (LogMessage _ _ t1 f1 l1) (LogMessage _ _ t2 f2 l2)
        | f1 /= f2 = f1 < f2
        | l1 /= l2 = l1 >= l2
        | otherwise = t1 >= t2
    {--max lm1 lm2 = case lm1 < lm2 of
        True -> lm2
        False -> lm1
    min lm1 lm2 = case lm1 <= lm2 of
        True -> lm1
        False -> lm2--}
    
data EventSourceMatcher = Exact EventSource
                        | With EventSource
                        | AnyInternal
                        | AnyExternal
                        | Any
                        | MatchAny [EventSourceMatcher]
                        | MatchAll [EventSourceMatcher]
                        deriving (Show, Read, Eq)

-- | Change log level operator
-- TODO: implement operator which changes LogLevel of LogMessage
($=) :: LogMessage -> LogLevel -> LogMessage
($=) m nl = m { lmLogLevel = nl }


-- | EventSource "combinator"
-- TODO: implement operator which combines two EventSources (just 1 level for Combined, see tests)
(@@) :: EventSource -> EventSource -> EventSource
(@@) (Combined s1) (Combined s2) = Combined (s1 ++ s2)
(@@) e (Combined s) = Combined ([e] ++ s)
(@@) (Combined s) e = Combined (s ++ [e])
(@@) e1 e2 = Combined ([e1] ++ [e2])

-- | Matching EventSource with EventSourceMatcher operator
-- TODO: implement matching
infixr 6 ~~
(~~) :: EventSourceMatcher -> EventSource -> Bool
(~~) (Exact e1) e2 = e1 == e2
(~~) (With e) (Combined s) = elem e s
(~~) (With _) _ = False
(~~) AnyInternal (Internal _ _) = True
(~~) AnyInternal (Combined s) = checkAny s AnyInternal
(~~) AnyInternal _ = False
(~~) AnyExternal (External _ _) = True
(~~) AnyExternal (Combined s) = checkAny s AnyExternal
(~~) AnyExternal _ = False
(~~) Any _ = True
(~~) (MatchAny []) e = False
(~~) (MatchAny (x:xs)) e = case x ~~ e of
    True -> True
    False -> MatchAny xs ~~ e
(~~) (MatchAll []) e = True
(~~) (MatchAll (x:xs)) e = case x ~~ e of
    True -> MatchAll xs ~~ e
    False -> False  

-- Helper functions
checkAny :: [EventSource] -> EventSourceMatcher -> Bool
checkAny [] _ = False
checkAny (x:xs) sm = case sm ~~ x of
    True -> True
    False -> checkAny xs sm 

-- | Specialized log list filter
-- TODO: implement filter function for logs with matchers, log level and hidden flag
logFilter :: EventSourceMatcher -> LogLevel -> Bool -> [LogMessage] -> [LogMessage]
logFilter e l f m = filter (p e l f) m
    where 
        p :: EventSourceMatcher -> LogLevel -> Bool -> LogMessage -> Bool
        p s l f m = s ~~ (lmSource m) && l <= (lmLogLevel m) && f == (lmHiddenFlag m)
