module Canvas.Helper exposing (packMaybeJSONValue)

import Json.Encode as Encode

----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
--------------------- HELPERS ----------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------


{-| Packs a maybe JSON value ready to stack with other
encoded JSON values in other lists.
-}
packMaybeJSONValue :
    String
    -> (a -> Encode.Value)
    -> Maybe a
    -> List (String, Encode.Value)
packMaybeJSONValue fieldStr encoderFunc maybeVal =
    case maybeVal of
        Just val -> [ (fieldStr, encoderFunc val)]
        Nothing -> []

