module Canvas.Attachments exposing
    (Attachments(..)
    , Attachment
    , Err(..)
    , encodeList, fromValues
    )

{-| A module encapsulating the way that Edges are attached to Nodes.

@docs Attachments, Attachment, Err

@docs fromValues, encodeList
-}

import Canvas.Position exposing (Position)
import Canvas.ID as ID exposing (ID)
import Canvas.NodeSide as NodeSide exposing (NodeSide)
import Json.Encode as Encode


{-| During the editing process, the start and end
point of an Edge can be:

- Set (Nothing is being edited)
- Start position being edited
(start position is a floating coordinate as it's being dragged)
- End position being edited 
(end position is a floating coordinate as it's being dragged)
-}
type Attachments
    = Set Attachment Attachment
    | UnsetStart Position Attachment
    | UnsetEnd Attachment Position


{-| A node's attachment
-}
type Attachment =
    Attachment
    { node : ID
    , side : NodeSide
    }

getAttachNode : Attachment -> ID
getAttachNode (Attachment attach) =
    attach.node

getAttachSide : Attachment -> NodeSide
getAttachSide (Attachment attach) =
    attach.side

attachmentFromValues : ID -> NodeSide -> Attachment
attachmentFromValues id side =
    Attachment
    { node = id
    , side = side
    }


{-| Creates Attachments from values.

    Attachments.fromValues
        463889
        Left
        54332
        Right
-}
fromValues :
    ID
    -> NodeSide
    -> ID
    -> NodeSide
    -> Attachments
fromValues startNode startSide endNode endSide =
    Set
        (attachmentFromValues startNode startSide)
        (attachmentFromValues endNode endSide)


{-| The ways this module can possibly throw errors.

- EncodeWithUnsetAttachment - Attempted to ancode
an Attachments type without having both Attachments Set.
-}
type Err
    = EncodeWithUnsetAttachment


{-| Encodes a List of Json.Encode.Values to insert into an Edge encoding.

Because an Attachment cannot be saved when it is unset, this
will throw an Err if it is not set.
-}
encodeList : Attachments -> Result Err ( List ( String, Encode.Value ) )
encodeList attachments =
    case attachments of
        UnsetStart _ _ -> Err EncodeWithUnsetAttachment
        UnsetEnd _ _ -> Err EncodeWithUnsetAttachment
        Set start end ->
            Ok
            [ ("fromNode", Encode.string <| ID.toString <| getAttachNode start)
            , ("fromSide", Encode.string <| NodeSide.toString <| getAttachSide start)
            , ("toNode", Encode.string <| ID.toString <| getAttachNode end)
            , ("toSide", Encode.string <| NodeSide.toString <| getAttachSide end)
            ]
