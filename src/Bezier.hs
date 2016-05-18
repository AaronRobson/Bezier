module Bezier
where

import Data.List

type Coordinate = Double

defCoordinate :: Coordinate
defCoordinate = 0

type Point1d = Coordinate
type Point2d = (Coordinate,Coordinate)

defPoint1d :: Point1d
defPoint1d = defCoordinate
defPoint2d :: Point2d
defPoint2d = (defCoordinate,defCoordinate)

xPoint :: Point2d -> Coordinate
xPoint = fst

yPoint :: Point2d -> Coordinate
yPoint = snd

--Should be between 0 and 1 inclusive.
type T = Double

fluxions :: [T]
fluxions = [0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]

difference1d :: Point1d -> Point1d -> Point1d
difference1d p1 p2 = p2 - p1

difference2d :: Point2d -> Point2d -> Point2d
difference2d p1 p2 = (x,y)
  where
    x = difference1d (xPoint p1) (xPoint p2)
    y = difference1d (yPoint p1) (yPoint p2)

partWayDownLine1d :: Point1d -> Point1d -> T -> Point1d
partWayDownLine1d p1 p2 t = p1 + (diff * t)
  where
    diff = difference1d p1 p2

partWayDownLine2d :: Point2d -> Point2d -> T -> Point2d
partWayDownLine2d p1 p2 t = (x,y)
  where
    x = partWayDownLine1d (xPoint p1) (xPoint p2) t
    y = partWayDownLine1d (yPoint p1) (yPoint p2) t

adjacentPairs :: [a] -> [(a,a)]
adjacentPairs [] = []
adjacentPairs [_] = []
adjacentPairs (x1:x2:xs) = (x1,x2):(adjacentPairs (x2:xs))

--Recursively convert to a mobile set of anchor points of one order fewer (i.e. quadratic to cubic).
bezier1d :: [Point1d] -> T -> Point1d
bezier1d [] _ = defPoint1d
bezier1d [p] _ = p
bezier1d ps t = bezier1d (map f pairs) t
  where
    pairs = adjacentPairs ps
    f (p1,p2) = partWayDownLine1d p1 p2 t

bezier2d :: [Point2d] -> T -> Point2d
bezier2d ps t = (x,y)
  where
    xs :: [Point1d]
    xs = map xPoint ps
    ys :: [Point1d]
    ys = map yPoint ps
    x :: Point1d
    x = bezier1d xs t
    y :: Point1d
    y = bezier1d ys t

findBezierPoints1d :: [Point1d] -> [Point1d]
findBezierPoints1d ps = map (bezier1d ps) fluxions

findBezierPoints2d :: [Point2d] -> [Point2d]
findBezierPoints2d ps = map (bezier2d ps) fluxions

generateSVG :: [Point2d] -> [Char]
generateSVG ps = unlines $ [encodingLine, docType, headerLine] ++ (map (indent ++) (produceLinesSVG True)) ++ [terminatingLine]
  where
    encodingLine = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>"
    docType = "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
    headerLine = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"100%\" height=\"100%\" viewBox=\"0 0 100 100\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">"
    terminatingLine = "</svg>"
    bezierPoints :: [Point2d]
    bezierPoints = findBezierPoints2d ps
    indent = "  "
    produceLinesSVG :: Bool -> [String]
    produceLinesSVG usePolyLine =
        if usePolyLine
          then [polylineSVG] 
          else linesSVG
      where
        style = "stroke-linecap=\"round\" style=\"fill:none;stroke:black;stroke-width:2\""
        polylineSVG = "<polyline points=\"" ++ (joinStr " " $ map (\p -> (show $ xPoint p) ++ "," ++ (show $ yPoint p)) bezierPoints) ++ "\" " ++ style ++ " />"
        linesSVG = map lineFunc $ adjacentPairs bezierPoints
          where
            lineFunc :: (Point2d,Point2d) -> String
            lineFunc ((x1,y1),(x2,y2)) = "<line x1=\"" ++ (show x1) ++ "\" y1=\"" ++ (show y1) ++ "\" x2=\"" ++ (show x2) ++ "\" y2=\"" ++ (show y2) ++ "\" " ++ style ++ " />"
    joinStr :: String -> [String] -> String
    joinStr sep strings = concat $ intersperse sep strings

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
