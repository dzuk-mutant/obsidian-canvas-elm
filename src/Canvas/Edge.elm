module Canvas.Edge exposing
    ( Edge
    , decoder
    , encoder

    , fromValues

    , getID
    , getFromNode
    , getFromSide
    , getToNode
    , getToSide
    , getColor
    , getLabel

    , changeFromNode
    , changeFromSide
    , changeToNode
    , changeToSide
    , changeColor
    , changeLabel
    )
{-| The module handling Edges.

Edges are the arrows that create visual relationships between Nodes on a Canvas.

@docs Edge

# Creation

@docs fromValues

# JSON decode/encode

@docs decoder, encoder

# Retrieving values

@docs getID, getFromNode, getFromSide, getToNode, getToSide, getColor, getLabel

# Changing values

You cannot change IDs manually.

    edge
    |> Edge.changeFromNode 545287
    |> Edge.changeFromSide Left

@docs changeFromNode, changeFromSide, changeToNode, changeToSide, changeColor, changeLabel
-}

import Canvas.Color exposing (Color)
import Canvas.Helper exposing (packMaybeJSONValue)
import Canvas.ID as ID exposing (ID)
import Canvas.NodeSide as NodeSide exposing (NodeSide)
import Json.Decode exposing (Decoder, string, maybe, succeed)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode


{-| Edges are the arrows that create relationships between
nodes.

Edge is an opaque type for more stable control over values.
-}


type Edge =
    Edge
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
    succeed constructor
    |> required "id" ID.decoder
    |> required "fromNode" ID.decoder
    |> required "fromSide" NodeSide.decoder
    |> required "toNode" ID.decoder
    |> required "toSide" NodeSide.decoder
    |> optional "color" (maybe string) Nothing
    |> optional "label" (maybe string) Nothing


{-| Constructor function for the JSON decoder.
We need a constructor because this is an opaque type.
-}
constructor :
    ID
    -> ID
    -> NodeSide
    -> ID
    -> NodeSide
    -> Maybe String
    -> Maybe String
    -> Edge
constructor id fromNode fromSide toNode toSide color label =
    Edge
    { id = id
    , fromNode = fromNode
    , fromSide = fromSide
    , toNode = toNode
    , toSide = toSide
    , color = color
    , label = label
    }


{-| Encodes an Edge into a JSON object.
-}
encoder :
    Edge
    -> Encode.Value
encoder (Edge edge) =
    Encode.object <|
        [ ("id", Encode.string <| ID.toString edge.id)
        , ("fromNode", Encode.string <| ID.toString edge.fromNode)
        , ("fromSide", Encode.string <| NodeSide.toString edge.fromSide)
        , ("toNode", Encode.string <| ID.toString edge.toNode)
        , ("toSide", Encode.string <| NodeSide.toString edge.toSide)
        ]
        ++ packMaybeJSONValue "color" Encode.string edge.color
        ++ packMaybeJSONValue "label" Encode.string edge.label


{-| Constructs an edge from a series of values.

- fromNode : The ID of the Node this arrow comes from,
- fromSide : What side of the Node this arrow comes from.
- toNode : The ID of the Node this arrow points to,
- toSide : What side of the Node this arrow points at.
- color : The color of this arrow (optional).
- label : The label of this arrow (optional).

```
    Edge.fromValues
        { fromNode = 15742
        , fromSide = Left
        , toNode = 927
        , toSide = Right
        , color = Nothing
        , label = Nothing
        } 
```
-}
fromValues :
    { fromNode : ID
    , fromSide : NodeSide
    , toNode : ID
    , toSide : NodeSide
    , color : Maybe Color
    , label : Maybe String
    }
    -> Edge
fromValues {fromNode, fromSide, toNode, toSide, color, label} =
    Edge
    { id = 234235 -- TODO: Figure out how to dynamically create these.
    , fromNode = fromNode
    , fromSide = fromSide
    , toNode = toNode
    , toSide = toSide
    , color = color
    , label = label
    }


-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------
----------------------------- GET ------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------

{-| Gets the ID of an Edge.

    Edge.getID edge
-}
getID : Edge -> ID
getID (Edge edge) = edge.id


{-| Gets the ID of the node that an Edge is coming from.

    Edge.getFromNode edge
-}
getFromNode : Edge -> ID
getFromNode (Edge edge) = edge.fromNode


{-| Gets the NodeSide of the node that an Edge is coming from.

    Edge.getFromSide edge
-}
getFromSide : Edge -> NodeSide
getFromSide (Edge edge) = edge.fromSide


{-| Gets the ID of the node that an Edge is pointing towards.

    Edge.getToNode edge
-}
getToNode : Edge -> ID
getToNode (Edge edge) = edge.toNode


{-| Gets the NodeSide of the node that an Edge is pointing towards./

    Edge.getToSide edge
-}

getToSide : Edge -> NodeSide
getToSide (Edge edge) = edge.toSide


{-| Gets the Color of an Edge (if any).

    Edge.getColor edge
-}
getColor : Edge -> Maybe Color
getColor (Edge edge) = edge.color


{-| Gets the label of an Edge (if any).

    Edge.getLabel edge
-}
getLabel : Edge -> Maybe String
getLabel (Edge edge) = edge.label


-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------
----------------------------- EDIT ------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------



{-| Changes the ID of the node of an Edge is coming from.

    Edge.changeFromNode 234567 edge
-}
changeFromNode : ID -> Edge -> Edge
changeFromNode newID (Edge edge) =
    Edge { edge | fromNode = newID }


{-| Changes the fromSide of an Edge.

    Edge.changeFromSide Top edge
-}
changeFromSide : NodeSide -> Edge -> Edge
changeFromSide newSide (Edge edge) =
    Edge { edge | fromSide = newSide }


{-| Changes the ID of the node of an Edge is pointing towards.

    Edge.changeToNode 234567 edge
-}
changeToNode : ID -> Edge -> Edge
changeToNode newID (Edge edge) =
    Edge { edge | toNode = newID }


{-| Changes the toSide of an Edge.

    Edge.changeToSide Right edge
-}
changeToSide : NodeSide -> Edge -> Edge
changeToSide newSide (Edge edge) =
    Edge { edge | fromSide = newSide }


{-| Changes the color of an Edge.

    Edge.changeColor (Just "#00fff00") edge
-}
changeColor : Maybe Color -> Edge -> Edge
changeColor newColor (Edge edge) =
    Edge { edge | color = newColor }


{-| Changes the color of an Edge.

    Edge.changeColor (Just "#00fff00") edge
-}
changeLabel : Maybe String -> Edge -> Edge
changeLabel newLabel (Edge edge) =
    Edge { edge | label = newLabel }
