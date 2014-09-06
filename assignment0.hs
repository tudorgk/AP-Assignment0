module Curve where

data Point = Point { x :: Double, y :: Double} deriving (Show)
type Curve = [Point]


point :: (Double, Double) -> Point
point a = Point { x = (fst a) , y = (snd a)}

comparePoints :: Point -> Point -> Bool
comparePoints a b = (truncate ((x a) * 100)) - (truncate ((x b)* 100)) == 0 && (truncate ((y a) * 100)) - (truncate ((y b)* 100)) == 0

instance Eq Point where
	a == b = comparePoints a b
	a /= b = not (comparePoints a b)


curve :: Point -> [Point] -> Curve
curve point [] = [point]
curve point listPoints =  point:listPoints

connect :: Curve -> Curve -> Curve
connect c1 c2 = c1 ++ c2

rotatePoint :: Double -> Point -> Point
rotatePoint theta point
	= Point { x = (x point) * c + (y point) * s , y = (y point) * c - (x point) * s}
		where (s, c) = (sin	theta, cos theta)

rotate :: Curve -> Double -> Curve
rotate points theta 
	= map (rotatePoint theta) points

translatePoint :: Point -> Point -> Point
translatePoint cPoint pPoint = Point { x = (x cPoint) + (x pPoint), y = (y cPoint) + (y pPoint)}

translate :: Curve -> Point -> Curve
translate points pPoint
	= map (translatePoint pPoint) points

data Axis = Vertical | Horizontal deriving (Eq, Show)

reflectPoint :: Axis -> Double -> Point ->  Point
reflectPoint a aValue cPoint = 
	if a == Horizontal
		then Point { x = (x cPoint), y = aValue - (y cPoint) + aValue}
		else Point { x = aValue - (x cPoint) + aValue, y = (y cPoint)}
		

reflect :: Curve -> Axis -> Double -> Curve
reflect points axis axisValue
	= map (reflectPoint axis axisValue) points


findMinMaxValues :: (Point, Point) -> Point -> (Point, Point)
findMinMaxValues (p1, p2) comparePoint = (Point {x = (min (x p1) (x comparePoint)) , y = (min (y p1) (y comparePoint))},
										  Point {x = (max (x p2) (x comparePoint)) , y = (max (y p2) (y comparePoint))})

bbox :: Curve -> (Point, Point)
bbox [] = error "no items in the list"
bbox (point:[]) = error "only one point"
bbox points = 
		let (minPoint, maxPoint) = (
				Point {x = (min (x (head points)) (x (head (tail points)))) , y = (min (y (head points)) (y (head (tail points))))}, 
				Point {x = (max (x (head points)) (x (head (tail points)))) , y = (max (y (head points)) (y (head (tail points))))})
		in foldl findMinMaxValues (minPoint, maxPoint) (tail (tail points))

width :: (Point, Point) -> Double
width a = abs((x (fst a))) + (x (snd a))

height :: (Point, Point) -> Double
height a = abs((y (fst a))) + (y (snd a))

toList :: Curve -> [Point]
toList [] = []
toList (firstPoint:points) = firstPoint : (toList points)


--TODO: Sanity Checks 
--let p2 = point (4, 2)
--let c1 = [point (2, 1),point (4, 5), point (3,3),point (2,5)]
--let c2 = [point (3,2), point (-2, -2), point (2,8), point (-4, -1), point (5, -6), point(-6,-5)]