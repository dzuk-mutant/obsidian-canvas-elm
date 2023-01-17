module Canvas exposing
    ( Canvas
    , decoder
    , encode
    , Err(..)
    )
    
{-| The module for an entire Obsidian Canvas file.

@docs Canvas, decoder, encode, Err
-}


import Canvas.Edge as Edge exposing (Edge)
import Canvas.Node as Node exposing (Node)
import Json.Decode exposing (Decoder, list, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Result.Extra as Result

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
encode : Canvas -> Result Err Encode.Value
encode (Canvas canvas) =
    let
        encodedEdges =
            List.map Edge.encode canvas.edges
            |> Result.combine
    in
        case encodedEdges of
            Err e -> Err <| EdgeErr e
            Ok unwrappedEdges ->
                Ok <|
                    Encode.object <|
                        [ ( "nodes", Encode.list Node.encode canvas.nodes)
                        , ( "edges", Encode.list (\ a -> a) unwrappedEdges)
                        ]


{-| Errors a Canvas can throw.

- EdgeErr - an Edge has thrown an Error.
-}
type Err 
    = EdgeErr Edge.Err
