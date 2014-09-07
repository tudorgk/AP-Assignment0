module Curve where

import System.Environment()
import Text.Printf
import System.IO()

data Point = Point { x :: Double, y :: Double} deriving (Show)
type Curve = [Point]

--Point constructor
point :: (Double, Double) -> Point
point a = Point { x = (fst a) , y = (snd a)}


--Comparing points. Didn't know how to remove the warnings. If you can tell me how
--to do this comparison without those warnings, I would be very grateful :)
comparePoints :: Point -> Point -> Bool
comparePoints a b = (truncate ((x a) * 100)) - (truncate ((x b)* 100)) == 0 && (truncate ((y a) * 100)) - (truncate ((y b)* 100)) == 0

instance Eq Point where
	a == b = comparePoints a b
	a /= b = not (comparePoints a b)

--Curve constructor
curve :: Point -> [Point] -> Curve
curve aPoint [] = [aPoint]
curve aPoint listPoints =  aPoint:listPoints

--List concat
connect :: Curve -> Curve -> Curve
connect c1 c2 = c1 ++ c2

--Curve rotation
rotatePoint :: Double -> Point -> Point
rotatePoint theta aPoint
	= Point { x = (x aPoint) * c + (y aPoint) * s , y = (y aPoint) * c - (x aPoint) * s}
		where (s, c) = (sin	(theta / 180*pi), cos (theta / 180*pi))

rotate :: Curve -> Double -> Curve
rotate points theta 
	= map (rotatePoint theta) points

--Curve translation
translatePoint :: Point -> Point -> Point
translatePoint cPoint pPoint = Point { x = (x cPoint) + (x pPoint), y = (y cPoint) + (y pPoint)}

translate :: Curve -> Point -> Curve
translate (firstPoint:points) pPoint
	= map (translatePoint Point{x = (x pPoint) - (x firstPoint), y = (y pPoint) - (y firstPoint)}) (firstPoint:points)

data Axis = Vertical | Horizontal deriving (Eq, Show)

--Curve reflection
reflectPoint :: Axis -> Double -> Point ->  Point
reflectPoint a aValue cPoint = 
	if a == Horizontal
		then Point { x = (x cPoint), y = aValue - (y cPoint) + aValue}
		else Point { x = aValue - (x cPoint) + aValue, y = (y cPoint)}
		

reflect :: Curve -> Axis -> Double -> Curve
reflect points axis axisValue
	= map (reflectPoint axis axisValue) points


--Bounding box
findMinMaxValues :: (Point, Point) -> Point -> (Point, Point)
findMinMaxValues (p1, p2) comparePoint = (Point {x = (min (x p1) (x comparePoint)) , y = (min (y p1) (y comparePoint))},
										  Point {x = (max (x p2) (x comparePoint)) , y = (max (y p2) (y comparePoint))})

bbox :: Curve -> (Point, Point)
bbox [] = error "no items in the list"
bbox (aPoint:[]) = (aPoint, aPoint)
bbox points = 
		let (minPoint, maxPoint) = (
				Point {x = (min (x (head points)) (x (head (tail points)))) , y = (min (y (head points)) (y (head (tail points))))}, 
				Point {x = (max (x (head points)) (x (head (tail points)))) , y = (max (y (head points)) (y (head (tail points))))})
		in foldl findMinMaxValues (minPoint, maxPoint) (tail (tail points))

--Width & Height
width :: Curve -> Double
width points = abs((x (fst a))) + (x (snd a))
	where a = bbox points

height :: Curve -> Double
height points = abs((y (fst a))) + (y (snd a))
	where a = bbox points

--Printing functions
toList :: Curve -> [Point]
toList [] = []
toList (firstPoint:points) = firstPoint : (toList points)

printPoints :: Curve -> String
printPoints [] = error "empty list"
printPoints (_:[]) = "</g></svg>"
printPoints (firstPoint:points) = 
	"<line style=\"stroke-width: 2px; stroke: black; fill:white\" x1=\"" 
            ++ (printf "%.2f" (x firstPoint)) ++ 
            "\" x2=\"" ++ (printf "%.2f" (x (head points))) ++ 
            "\" y1=\"" ++ (printf "%.2f" (y firstPoint)) ++ 
            "\" y2=\"" ++ (printf "%.2f" (y (head points))) ++ "\" />" ++ (printPoints points)

toSVG :: Curve -> String
toSVG points = 
	"<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
	 ++ (show (ceiling (width points))) ++ "px\" height=\""
	 ++ (show (ceiling (height points))) ++ "px\" version=\"1.1\"><g>" 
	 ++ printPoints points


--Exporting to file
toFile :: Curve -> FilePath -> IO ()
toFile points filePath=
	writeFile filePath (toSVG points)


--Testing hilbert
hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c Horizontal 0

          c0 = ch `rotate` (-90) `translate` (point (w+p+w, h+p+h))
          c1 = c `translate` (point (w+p+w, h))
          c2 = c
          c3 = ch `rotate` 90 `translate` (point (0, h+p))


