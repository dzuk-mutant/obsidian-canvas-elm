module Canvas.Coordinate exposing (Coordinate, fromValues, x, y)

{-| Module encapsulating the coordinate system of Obsidian canvases.

@docs Coordinate, fromValues, x, y
-}

{-| Data type encapsulating a single 2D coordinate.
-}
type Coordinate =
    Coordinate
    { x : Int
    , y : Int
    }

{-| Creates a Coordinate from two Ints.

    Coordinate.fromValues 13 -356
-}
fromValues : Int -> Int -> Coordinate
fromValues xVal yVal = 
    Coordinate
        { x = xVal
        , y = yVal
        }

{-| Gets the x coordinate.
-}
x : Coordinate -> Int
x (Coordinate coord) = coord.x

{-| Gets the y coordinate.
-}
y : Coordinate -> Int
y (Coordinate coord) = coord.y
