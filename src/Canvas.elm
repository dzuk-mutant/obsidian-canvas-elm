module Canvas exposing ( Canvas
    , decoder
    , encoder
    , newlinesIn
    , newlinesOut
    )

import Canvas.Edge as Edge exposing (Edge)
import Canvas.Node as Node exposing (Node)
import Json.Decode exposing (Decoder, list, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode







{-| The complete data structure of a Canvas.
-}
type alias Canvas =
    { nodes : List Node
    , edges : List Edge
    }


{-| Canvas JSON decoder.
-}
decoder : Decoder Canvas
decoder =
    succeed Canvas
    |> required "nodes" (list Node.decoder)
    |> required "edges" (list Edge.decoder)


{-| Canvas JSON encoder.
-}
encoder : Canvas -> Encode.Value
encoder data =
    Encode.object <|
        [ ( "nodes", (Encode.list Node.encoder data.nodes))
        , ( "edges", (Encode.list Edge.encoder data.edges))
        ]


----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
--------------------- HELPERS ----------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------



newlinesIn : String -> String
newlinesIn canvasJson =
    canvasJson
    |> String.trim
    |> String.replace "\n" "\\n" 


newlinesOut : String -> String
newlinesOut canvasJson =
    String.replace "\\n" "\n" canvasJson

