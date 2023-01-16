module Canvas exposing
    ( Canvas
    , decoder
    , encode
    )
    
{-| The module for an entire Obsidian Canvas file.

@docs Canvas, decoder, encode
-}


import Canvas.Edge as Edge exposing (Edge)
import Canvas.Node as Node exposing (Node)
import Json.Decode exposing (Decoder, list, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


{-| The complete data structure of a Canvas.
-}
type Canvas =
    Canvas
    { nodes : List Node
    , edges : List Edge
    }


{-| Canvas JSON decoder.
-}
decoder : Decoder Canvas
decoder =
    succeed constructor
    |> required "nodes" (list Node.decoder)
    |> required "edges" (list Edge.decoder)


{-| Constructs a Canvas at the final stage of decoding.
-}
constructor : List Node -> List Edge -> Canvas
constructor nodes edges =
    Canvas
    { nodes = nodes
    , edges = edges
    }


{-| Encodes a Canvas into a JSON object.
-}
encode : Canvas -> Encode.Value
encode (Canvas canvas) =
    Encode.object <|
        [ ( "nodes", (Encode.list Node.encode canvas.nodes))
        , ( "edges", (Encode.list Edge.encode canvas.edges))
        ]

