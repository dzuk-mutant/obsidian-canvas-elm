module Canvas.ID exposing (ID, fromInt, fromString, toString, decoder)
{-| Module for handling the IDs that identify
nodes and link edges.

@docs ID, decoder, toString, fromString
-}

import Json.Decode exposing (Decoder, andThen, string, int, fail, succeed)
import Hex


type ID = ID Int


{-| Decodes an ID value from JSON.

Fails if the given ID is not a valid hexadecimal string.
-}
decoder : Decoder ID
decoder =
    string
    |> andThen 
        (\ str ->
            case fromString str of 
                Just int -> succeed int
                Nothing -> fail <| "ID '"++ str ++"' is not a valid hex."
        )


{-| Creates an ID from an Int.

    ID.fromInt 346587
-}
fromInt : Int -> ID
fromInt int =
    ID int


{-| Converts an ID to its corresponding string
that appears in JSON.

    ID.toString 325465
-}
toString : ID -> String
toString (ID id) =
    Hex.toString id


{-| Converts corresponding strings to ID values.

Will return Nothing if the string is not a valid
hexadecimal number.

    ID.fromString "454ae23f"
-}
fromString : String -> Maybe ID
fromString str =
    case Hex.fromString str of 
        Ok int -> Just <| ID int
        Err _ -> Nothing
