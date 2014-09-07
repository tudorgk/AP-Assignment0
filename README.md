[AP-Assignment0 - Look at Those Curves](https://github.com/tudorgk/AP-Assignment0)
=======================================

The objective of this assignment is to gain hands-on programming experience with Haskell.

The goal is to implement a library, Curves, for working with piecewise linear curves in the plane (based on an idea by Hansen and Rischel, who in turn got the idea from Maarten M. Fokkinga).

SYNOPSIS
--------

Example of usage:

```haskell
toFile  (hilbert $ hilbert $ hilbert $ hilbert $ curve (point (0,0)) []) "hilbert.svg"
```

DESCRIPTION 
-----------

The sections below describe the most important points of the assignment.

##### Point and Curve data types

```haskell
data Point = Point { x :: Double, y :: Double} deriving (Show)
type Curve = [Point]
```

Basically a `Curve` is a list of `Points`

##### Comparing 2 points

We can use `(==)` for comparing 2 points because we declared `Point` as an instance of `Eq`

##### The function `reflect`

We used `map` for reflecting all of the points to a given `Axis`

```haskell
reflect :: Curve -> Axis -> Double -> Curve
reflect points axis axisValue
	= map (reflectPoint axis axisValue) points
```

##### Calculating the bounding box

To compute the lower left and the upper right corner of the bounding box we need to compare the smallest (x,y) coordinate and the biggest (x,y) coordinate from all the points in the list. We do this buy applying `foldl` on the `Curve` so that on every step we find the min/max values.

```haskell
bbox :: Curve -> (Point, Point)
bbox [] = error "no items in the list"
bbox (aPoint:[]) = (aPoint, aPoint)
bbox points = 
		let (minPoint, maxPoint) = (
				Point {x = (min (x (head points)) (x (head (tail points)))) , y = (min (y (head points)) (y (head (tail points))))}, 
				Point {x = (max (x (head points)) (x (head (tail points)))) , y = (max (y (head points)) (y (head (tail points))))})
		in foldl findMinMaxValues (minPoint, maxPoint) (tail (tail points))
```

##### Printing the SVG

We do a basic string concat using `(++)` for example: 

```haskell
toSVG :: Curve -> String
toSVG points = 
	"<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
	 ++ (show (ceiling (width points))) ++ "px\" height=\""
	 ++ (show (ceiling (height points))) ++ "px\" version=\"1.1\"><g>" 
	 ++ printPoints points
```