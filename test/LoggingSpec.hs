module LoggingSpec (spec) where

import Test.Hspec
import Data.Time.Clock

import Data.Logging


timestamp1 = read "2017-11-19 18:28:52.607875 UTC" :: UTCTime
timestamp2 = read "2017-11-20 18:28:52.607875 UTC" :: UTCTime
timestamp3 = read "2017-11-21 18:28:52.607875 UTC" :: UTCTime

esInternal1 = Internal { iesComponent = "comp1", iesCallID = "com.call.id26218" }
esInternal2 = Internal { iesComponent = "comp2", iesCallID = "com.call.id26218" }
esExternal = External { eesURI = "//some/uri", eesDescription = "none" }
esCombined = Combined [esInternal1, esExternal]

defaultLogMsg = LogMessage { lmTimestamp  = timestamp2
                           , lmSource     = Unknown
                           , lmMessage    = "Message is boring"
                           , lmHiddenFlag = True
                           , lmLogLevel   = Warning
                           }

spec :: Spec
spec = do
    describe "LogMessage" $ do
      describe "Show instance" $ do
        it "shows log message with various log levels" $ do
          show defaultLogMsg `shouldBe` "[Warning] Unknown: Message is boring"
          show (defaultLogMsg { lmLogLevel = Error }) `shouldBe` "[Error] Unknown: Message is boring"
          show (defaultLogMsg { lmLogLevel = Info }) `shouldBe` "[Info] Unknown: Message is boring"
        it "shows log message with various event sources" $ do
          show (defaultLogMsg { lmSource = esInternal1 }) `shouldBe` "[Warning] Internal[comp1]: Message is boring"
          show (defaultLogMsg { lmSource = esExternal  }) `shouldBe` "[Warning] External[//some/uri]: Message is boring"
          show (defaultLogMsg { lmSource = esCombined  }) `shouldBe` "[Warning] Combined[Internal[comp1],External[//some/uri]]: Message is boring"
        it "shows log message with various messages" $ do
          show (defaultLogMsg { lmMessage = "Stack overflow" }) `shouldBe` "[Warning] Unknown: Stack overflow"
          show (defaultLogMsg { lmMessage = "Null pointer exception" }) `shouldBe` "[Warning] Unknown: Null pointer exception"
      describe "Ord instance" $ do
        it "has correct compare (hidden)" $ do
          (defaultLogMsg < (defaultLogMsg { lmHiddenFlag = False })) `shouldBe` True
          (defaultLogMsg < (defaultLogMsg { lmHiddenFlag = False, lmLogLevel = Debug })) `shouldBe` True
          (defaultLogMsg < (defaultLogMsg { lmHiddenFlag = False, lmLogLevel = Fatal })) `shouldBe` True
          (defaultLogMsg < (defaultLogMsg { lmHiddenFlag = False, lmTimestamp = timestamp1 })) `shouldBe` True
          (defaultLogMsg < (defaultLogMsg { lmHiddenFlag = False, lmTimestamp = timestamp3 })) `shouldBe` True
        it "has correct compare (loglevel)" $ do
          (defaultLogMsg < (defaultLogMsg { lmLogLevel = Fatal })) `shouldBe` True
          (defaultLogMsg < (defaultLogMsg { lmLogLevel = Error })) `shouldBe` True
          (defaultLogMsg < (defaultLogMsg { lmLogLevel = Fatal, lmTimestamp = timestamp1 })) `shouldBe` True
          (defaultLogMsg < (defaultLogMsg { lmLogLevel = Fatal, lmTimestamp = timestamp3 })) `shouldBe` True
        it "has correct compare (timestamp)" $ do
          (defaultLogMsg < (defaultLogMsg { lmMessage = "Zzzzz", lmTimestamp = timestamp1 })) `shouldBe` False
          (defaultLogMsg < (defaultLogMsg { lmMessage = "Aaargh", lmTimestamp = timestamp3 })) `shouldBe` True
    describe "operators" $ do
      describe "$=" $
        it "changes log level correctly" $ do
          (defaultLogMsg $= Error) `shouldBe` defaultLogMsg { lmLogLevel = Error }
          (defaultLogMsg $= Debug) `shouldBe` defaultLogMsg { lmLogLevel = Debug }
          (defaultLogMsg $= Fatal) `shouldBe` defaultLogMsg { lmLogLevel = Fatal }
      describe "@@" $ do
        it "combines event sources" $ do
          (esInternal1 @@ esExternal)  `shouldBe` Combined [esInternal1,esExternal]
          (esExternal  @@ esInternal1) `shouldBe` Combined [esExternal,esInternal1]
        it "combines event combined sources" $ do
          (esInternal2 @@ esCombined) `shouldBe` Combined [esInternal2,esInternal1,esExternal]
          (esCombined @@ esInternal2) `shouldBe` Combined [esInternal1,esExternal,esInternal2]
          (esCombined @@ esCombined) `shouldBe` Combined [esInternal1,esExternal,esInternal1,esExternal]
        it "can be chained easily" $
          (esInternal1 @@ esInternal2 @@ esExternal) `shouldBe` Combined [esInternal1,esInternal2,esExternal]
      describe "~~" $ do
        it "matches with Exact" $ do
          (Exact esInternal1 ~~ esInternal1) `shouldBe` True
          (Exact esExternal  ~~ esExternal)  `shouldBe` True
          (Exact esInternal1 ~~ esExternal)  `shouldBe` False
          (Exact esCombined  ~~ esCombined)  `shouldBe` True
          (Exact esCombined  ~~ Combined [esExternal,esInternal2]) `shouldBe` False
          (Exact Unknown     ~~ Combined [esExternal,esInternal2]) `shouldBe` False
          (Exact esExternal  ~~ Combined [esExternal,esInternal2]) `shouldBe` False
          (Exact Unknown  ~~ Unknown) `shouldBe` True
        it "matches with With" $ do
          (With esInternal1 ~~ esInternal1) `shouldBe` False
          (With esExternal  ~~ esExternal)  `shouldBe` False
          (With esCombined  ~~ esCombined)  `shouldBe` False
          (With Unknown     ~~ Unknown)     `shouldBe` False
          (With esExternal  ~~ Combined [esExternal,esInternal2]) `shouldBe` True
          (With esInternal2 ~~ Combined [esExternal,esInternal2]) `shouldBe` True
          (With esInternal1 ~~ Combined [esExternal,esInternal2]) `shouldBe` False
        it "matches with AnyInternal" $ do
          (AnyInternal ~~ esInternal1) `shouldBe` True
          (AnyInternal ~~ esExternal)  `shouldBe` False
          (AnyInternal ~~ esCombined)  `shouldBe` True
          (AnyInternal ~~ Unknown)     `shouldBe` False
          (AnyInternal ~~ Combined [esExternal,Unknown]) `shouldBe` False
        it "matches with AnyExternal" $ do
          (AnyExternal ~~ esInternal1) `shouldBe` False
          (AnyExternal ~~ esExternal)  `shouldBe` True
          (AnyExternal ~~ esCombined)  `shouldBe` True
          (AnyExternal ~~ Unknown)     `shouldBe` False
          (AnyExternal ~~ Combined [esInternal1,Unknown]) `shouldBe` False
        it "matches with Any" $ do
          (Any ~~ esInternal1) `shouldBe` True
          (Any ~~ esExternal)  `shouldBe` True
          (Any ~~ esCombined)  `shouldBe` True
          (Any ~~ Unknown)     `shouldBe` True
          (Any ~~ Combined [esInternal1,Unknown]) `shouldBe` True
        it "matches with MatchAny" $ do
          (MatchAny [AnyExternal,Exact esInternal1] ~~ esInternal1) `shouldBe` True
          (MatchAny [AnyExternal,Exact esInternal1] ~~ esInternal2) `shouldBe` False
          (MatchAny [AnyExternal,Exact esInternal1] ~~ esExternal) `shouldBe` True
          (MatchAny [AnyExternal,Exact esInternal1] ~~ Unknown) `shouldBe` False
          (MatchAny [AnyExternal,Exact esInternal1] ~~ esCombined) `shouldBe` True
          (MatchAny [AnyExternal,Exact esInternal1] ~~ Combined [esInternal2,Unknown]) `shouldBe` False
        it "matches with MatchAll" $ do
          (MatchAll [AnyExternal,Exact esInternal1] ~~ esInternal1) `shouldBe` False
          (MatchAll [AnyExternal,Exact esInternal1] ~~ esInternal2) `shouldBe` False
          (MatchAll [AnyExternal,Exact esInternal1] ~~ esExternal) `shouldBe` False
          (MatchAll [AnyExternal,Exact esInternal1] ~~ Unknown) `shouldBe` False
          (MatchAll [AnyExternal,Exact esInternal1] ~~ esCombined) `shouldBe` False
          (MatchAll [AnyExternal,Exact esInternal1] ~~ Combined [esInternal2,Unknown]) `shouldBe` False
          (MatchAll [AnyExternal,Exact esExternal,Any] ~~ esExternal) `shouldBe` True
          (MatchAll [With esInternal1,With esExternal,Any] ~~ esCombined) `shouldBe` True
        it "can be chained easily" $ do
          (Any ~~ esInternal1 @@ esExternal) `shouldBe` True
          (With esExternal ~~ esInternal1 @@ esExternal) `shouldBe` True
          (Exact esExternal ~~ esInternal1 @@ esExternal) `shouldBe` False
    describe "logFilter" $ do
      it "filters based on matchers" $ do
        logFilter Any Warning True [defaultLogMsg] `shouldBe` [defaultLogMsg]
        logFilter (Exact Unknown) Warning True [defaultLogMsg] `shouldBe` [defaultLogMsg]
        logFilter AnyInternal Warning True [defaultLogMsg] `shouldBe` []
        logFilter AnyExternal Warning True [defaultLogMsg] `shouldBe` []
        logFilter (With esInternal1) Warning True [defaultLogMsg] `shouldBe` []
        logFilter (MatchAny [AnyExternal,Any]) Warning True [defaultLogMsg] `shouldBe` [defaultLogMsg]
      it "filters based log level" $ do
        logFilter Any Warning True [defaultLogMsg] `shouldBe` [defaultLogMsg]
        logFilter Any Error True [defaultLogMsg] `shouldBe` []
        logFilter Any Debug True [defaultLogMsg] `shouldBe` [defaultLogMsg]
        logFilter Any Info True [defaultLogMsg] `shouldBe` [defaultLogMsg]
        logFilter Any Warning True [defaultLogMsg, defaultLogMsg { lmLogLevel = Debug }] `shouldBe` [defaultLogMsg]
      it "filters based hidden flag" $ do
        logFilter Any Warning True [defaultLogMsg] `shouldBe` [defaultLogMsg]
        logFilter Any Warning False [defaultLogMsg] `shouldBe` []
        logFilter Any Warning False [defaultLogMsg, defaultLogMsg { lmHiddenFlag = False }] `shouldBe` [defaultLogMsg { lmHiddenFlag = False }]
