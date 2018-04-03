module Main where

import Graphics.Bezier

main :: IO ()
main = do
    putStrLn $ "Generating a Bezier curve \"" ++ filename ++ "\" with these anchor points:"
    print exampleAnchors
    writeFile filename svgText
  where
    exampleAnchors =
      [ (20,20)
      , (10,90)
      , (90,10)
      , (80,80)
      ]
    filename :: FilePath
    filename = "example.svg"
    svgText = generateSVG $ findBezierPoints2d exampleAnchors
