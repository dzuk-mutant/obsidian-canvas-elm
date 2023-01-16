module Canvas.Edge exposing (Edge, decoder, encoder)

import Canvas.Color exposing (Color)
import Canvas.Helper exposing (packMaybeJSONValue)
import Canvas.ID as ID exposing (ID)
import Canvas.NodeSide as NodeSide exposing (NodeSide)
import Json.Decode exposing (Decoder, string, maybe, succeed)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode



{-| Edges are the arrows that create relationships between
nodes.
-}
type alias Edge =
    { id : ID -- unique ID for this edge
    , fromNode : ID -- ID of the node this comes from
    , fromSide : NodeSide
    , toNode : ID -- ID of the node where this goes to.
    , toSide : NodeSide
    , color : Maybe Color
    , label : Maybe String
    }


{-| Decodes a corresponding JSON object into an Edge.
-}
decoder : Decoder Edge
decoder =
    succeed Edge
    |> required "id" ID.decoder
    |> required "fromNode" ID.decoder
    |> required "fromSide" NodeSide.decoder
    |> required "toNode" ID.decoder
    |> required "toSide" NodeSide.decoder
    |> optional "color" (maybe string) Nothing
    |> optional "label" (maybe string) Nothing


{-| Encodes an Edge into a JSON object.
-}
encoder :
    Edge
    -> Encode.Value
encoder edge =
    Encode.object <|
        [ ("id", Encode.string <| ID.toString edge.id)
        , ("fromNode", Encode.string <| ID.toString edge.fromNode)
        , ("fromSide", Encode.string <| NodeSide.toString edge.fromSide)
        , ("toNode", Encode.string <| ID.toString edge.toNode)
        , ("toSide", Encode.string <| NodeSide.toString edge.toSide)
        ]
        ++ packMaybeJSONValue "color" Encode.string edge.color
        ++ packMaybeJSONValue "label" Encode.string edge.label

