{-# LANGUAGE OverloadedStrings #-}

module Test.Reussir.Core.Mangle where

import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Reussir.Core.Mangle
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
    testGroup
        "Reussir.Core.Mangle"
        [ testGroup
            "B62Num encoding"
            [ testCase "0 -> _" $ mangleB62 0 @?= "_"
            , testCase "1 -> 0_" $ mangleB62 1 @?= "0_"
            , testCase "11 -> a_" $ mangleB62 11 @?= "a_"
            , testCase "62 -> Z_" $ mangleB62 62 @?= "Z_"
            , testCase "63 -> 10_" $ mangleB62 63 @?= "10_"
            , testCase "1000 -> g7_" $ mangleB62 1000 @?= "g7_"
            ]
        ]

-- | Helper to mangle B62Num and extract the text
mangleB62 :: Int -> T.Text
mangleB62 = TB.runBuilder . mangle . B62Num
