module Canvas.Position exposing (Position, encodeList, fromValues, set, x, y)

{-| Module encapsulating the coordinate system of Obsidian canvases.

@docs Coordinate, encodeList, fromValues, set, x, y
-}

import Json.Encode as Encode

{-| Data type encapsulating a single position in 2D space.
-}
type Position =
    Position
    { x : Int
    , y : Int
    }


encodeList : Position -> List (String, Encode.Value)
encodeList (Position pos) =
    [ ("x", Encode.int pos.x )
    , ("y", Encode.int pos.y )
    ]

{-| Creates a Coordinate from two Ints.

    Coordinate.fromValues 13 -356
-}
fromValues : Int -> Int -> Position
fromValues xVal yVal = 
        Position
        { x = xVal
        , y = yVal
        }


{-| Creates a Coordinate from two Ints.

    Position.set 13 -356 pos
-}
set : Int -> Int -> Position -> Position
set xVal yVal (Position pos) = 
        pos
        |> (\m -> { m | x = xVal } )
        |> (\m -> { m | y = yVal } )
        |> Position


{-| Gets the x coordinate.
-}
x : Position -> Int
x (Position p) = p.x


{-| Gets the y coordinate.
-}
y : Position -> Int
y (Position p) = p.y
