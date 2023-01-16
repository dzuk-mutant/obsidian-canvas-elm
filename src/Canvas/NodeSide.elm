module Canvas.NodeSide exposing (NodeSide(..), decoder, toString, fromString)
{-| Module for handling the data that governs what
side Edges come to and from.

@docs NodeSide, decoder, toString, fromString
-}

import Json.Decode exposing (Decoder, andThen, string, fail, succeed)

{-| The side of the node that a connection is connected to.
-}
type NodeSide
    = Top
    | Right
    | Bottom
    | Left


{-| Decodes a NodeSide value from JSON.
-}
decoder : Decoder NodeSide
decoder =
    string |> andThen
        (\ str ->
            case fromString str of
                Just ns -> succeed ns
                _ -> fail <| "'" ++ str ++ "' is not a valid name for a side."
        )


{-| Converts a NodeSide to its corresponding string
that appears in JSON.

    NodeSide.toString Top
    -- "top"

    NodeSide.toString Right
    -- "right"
-}
toString : NodeSide -> String
toString nodeSide =
    case nodeSide of
        Top -> "top"
        Right -> "right"
        Bottom -> "bottom"
        Left -> "left"


{-| Converts corresponding strings to NodeSide values.

Will return Nothing if the string is not valid.

    NodeSide.fromString "top"
    -- Just Top

    NodeSide.fromString "blah"
    -- Nothing
-}
fromString : String -> Maybe NodeSide
fromString str =
    case str of
        "top" -> Just Top
        "right" -> Just Right
        "bottom" -> Just Bottom
        "left" -> Just Left
        _ -> Nothing
