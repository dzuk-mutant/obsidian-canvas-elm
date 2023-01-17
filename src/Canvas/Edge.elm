module Canvas.Edge exposing
    ( Edge
    , decoder
    , encode
    , Err(..)

    , fromValues
    
    , getID
    , getAttachments
    , getColor
    , getLabel

    , setAttachments
    , setColor
    , setLabel
    )

{-| The module handling Edges.

Edges are the arrows that create visual relationships between Nodes on a Canvas.

@docs Edge, Err

# Creation

@docs fromValues

# JSON decode/encode

@docs decoder, encode

# Retrieving values

@docs getID, getAttachments, getColor, getLabel

# Changing values

You cannot change IDs manually.

@docs setAttachments, setColor, setLabel
-}

import Canvas.Attachments as Attachments exposing (Attachments(..))
import Canvas.Color exposing (Color)
import Canvas.Helper exposing (packMaybeJSONValue)
import Canvas.ID as ID exposing (ID)
import Canvas.NodeSide as NodeSide exposing (NodeSide)
import Json.Decode exposing (Decoder, string, maybe, succeed)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode


{-| Edges are the arrows that create relationships between
nodes.

This type is opaque for more stable API interactions.
-}


type Edge =
    Edge
    { id : ID -- unique ID for this edge
    , attachments : Attachments
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
    , attachments = Attachments.fromValues fromNode fromSide toNode toSide
    , color = color
    , label = label
    }


{-| Errors this module can throw.

- AttachmentErr - an Attachment has thrown an Err.
-}
type Err
    = AttachmentErr ID Attachments.Err


{-| Encodes an Edge into a JSON object.

If the attachments are not fully set, then it will throw an Err.
-}
encode : Edge -> Result Err Encode.Value
encode (Edge edge) =
    case Attachments.encodeList edge.attachments of
        Err e -> Err <| AttachmentErr edge.id e
        Ok attachmentList ->
            Ok <|
                Encode.object <|
                    ("id", Encode.string <| ID.toString edge.id )
                    :: attachmentList
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
    constructor
        (ID.fromInt 234235) -- TODO: Figure out how to dynamically create these.
        fromNode
        fromSide
        toNode
        toSide
        color
        label


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


{-| Gets the attachments of a node.

    Edge.getAttachments edge
-}
getAttachments : Edge -> Attachments
getAttachments (Edge edge) = edge.attachments


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

    Edge.setFromNode 234567 edge
-}
setAttachments : Attachments -> Edge -> Edge
setAttachments newVal (Edge edge) =
    Edge { edge | attachments = newVal }

{-| Changes the color of an Edge.

    Edge.setColor (Just "#00fff00") edge
-}
setColor : Maybe Color -> Edge -> Edge
setColor newColor (Edge edge) =
    Edge { edge | color = newColor }


{-| Changes the color of an Edge.

    Edge.setColor (Just "#00fff00") edge
-}
setLabel : Maybe String -> Edge -> Edge
setLabel newLabel (Edge edge) =
    Edge { edge | label = newLabel }
