module Graphics.Bezier.Tests where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Graphics.Bezier



testXandYPoint :: Test
testXandYPoint = testGroup "x and y Point"
    [ testCase "xPoint" (assertEqual "xPoint"
                 1.5
                 (Graphics.Bezier.xPoint (1.5 :: Double, 2.5 :: Double)))
    , testCase "yPoint" (assertEqual "yPoint"
                 2.5
                 (Graphics.Bezier.yPoint (1.5 :: Double, 2.5 :: Double)))
    ]

main :: IO ()
main = defaultMain
    [ testXandYPoint
    ]
