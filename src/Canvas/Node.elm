module Canvas.Node exposing (Node(..), decoder, encoder)

import Canvas.Color exposing (Color)
import Canvas.Helper exposing (packMaybeJSONValue)
import Canvas.ID as ID exposing (ID)
import Json.Decode as Decode exposing (Decoder, andThen, string, int, maybe, fail, field, succeed)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode


{-| Nodes in a Canvas can be one of several
types. This Custom Type branches these
possibilities out.
-}
type Node
    = FileNode File
    | TextNode Text
    | LinkNode Link
    | GroupNode Group


{-| Decodes a node based on it's type.

A node identifies what type it is with the "type" field,
so this checks it and carries on to the relevant decoder
once found. (Or fails if the type is invalid/unsupported.)
-}
decoder : Decoder Node
decoder =
    field "type" string
        |> andThen
            (\ typeStr -> 
                case typeStr of
                "file" -> Decode.map FileNode fileDecoder
                "text" -> Decode.map TextNode textDecoder
                "link" -> Decode.map LinkNode linkDecoder
                "group" -> Decode.map GroupNode groupDecoder
                _ -> fail "blah"
            )

{-| Encodes a node based on it's type.

(dict keys are given here so they're all in one place
like with nodeDecoder.)
-}
encoder : Node -> Encode.Value
encoder node =
    case node of
        FileNode f -> fileEncoder "file" f
        TextNode t -> textEncoder "text" t
        LinkNode l -> linkEncoder "link" l
        GroupNode g -> groupEncoder "group" g




----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
-------------------- CANVASNODE --------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------

{-| All nodes have shared fields, represented by
this extensible record.
-}
type alias CanvasNode r = 
    { r | id : ID
        , x : Int
        , y : Int
        , width : Int
        , height : Int
        , color : Maybe Color
    }

{-| Packs the  shared section of a Node for encoding.

Must be chained together with another function
to make a valid node.
-}
canvasNodeToJSONVals :
    String
    -> CanvasNode r
    -> List (String, Encode.Value)
canvasNodeToJSONVals typeStr canvasNode =
    [ ("id", Encode.string (ID.toString canvasNode.id))
    , ("type", Encode.string typeStr)
    , ("x", Encode.int canvasNode.x)
    , ("y", Encode.int canvasNode.y)
    , ("width", Encode.int canvasNode.width)
    , ("height", Encode.int canvasNode.height)
    ]
    ++ packMaybeJSONValue "color" Encode.string canvasNode.color


----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
--------------------- FILE -------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------


{-| A node that is a file, where the
file is located somewhere in the vault.
-}
type alias File =
    CanvasNode
        { file : String
        -- An optional subpath which links to a heading or
        -- a block. Always starts with a `#`.
        , subpath : Maybe String
        }


{-| Decodes a corresponding JSON object to a File.
-}
fileDecoder : Decoder File
fileDecoder =
    succeed fileConstructor
    |> required "id" ID.decoder
    |> required "x" int
    |> required "y" int
    |> required "width" int
    |> required "height" int
    |> optional "color" (maybe string) Nothing
    |> required "file" string
    |> optional "subpath" (maybe string) Nothing


{-| Elm doesn't allow for creating records using their
alias name as a constructor.
You gotta do this yay~
-}
fileConstructor :
    ID
    -> Int
    -> Int
    -> Int
    -> Int
    -> Maybe String
    -> String
    -> Maybe String
    -> File
fileConstructor id x y width height color file subpath =
    { id = id
    , x = x
    , y = y
    , width = width
    , height = height
    , color = color
    , file = file
    , subpath = subpath
    }

{-| Encodes a file node as a JSON object.
-}
fileEncoder :
    String
    -> File
    -> Encode.Value
fileEncoder typeStr file =
    Encode.object <|
        canvasNodeToJSONVals typeStr file
        ++ [("text", Encode.string file.file)]
        ++ packMaybeJSONValue "subpath" Encode.string file.subpath


----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
--------------------- FILE -------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------


{-| A node tha
t is plaintext.
-}
type alias Text =
    CanvasNode
        { text : String
        }


{-| Decodes a corresponding JSON object to a Text node.
-}
textDecoder : Decoder Text
textDecoder =
    succeed textConstructor
    |> required "id" ID.decoder
    |> required "x" int
    |> required "y" int
    |> required "width" int
    |> required "height" int
    |> optional "color" (maybe string) Nothing
    |> required "text" string


{-| Elm doesn't allow for creating records using their alias name as a constructor.
You gotta do this yay~
-}
textConstructor :
    ID
    -> Int
    -> Int
    -> Int
    -> Int
    -> Maybe String
    -> String
    -> Text
textConstructor id x y width height color text =
    { id = id
    , x = x
    , y = y
    , width = width
    , height = height
    , color = color
    , text = text
    }

{-| Encodes a text node as a JSON object.
-}
textEncoder :
    String
    -> Text
    -> Encode.Value
textEncoder typeStr text =
    Encode.object <|
        canvasNodeToJSONVals typeStr text
        ++ [("text", Encode.string text.text)]


----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
--------------------- LINK -------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------


{-| A node that is an external resource.
-}
type alias Link =
    CanvasNode
        { url : String
        }


{-| Decodes a corresponding JSON object to a Link node.
-}
linkDecoder : Decoder Link
linkDecoder =
    succeed linkConstructor
    |> required "id" ID.decoder
    |> required "x" int
    |> required "y" int
    |> required "width" int
    |> required "height" int
    |> optional "color" (maybe string) Nothing
    |> required "url" string


{-| Elm doesn't allow for creating records using their alias name as a constructor.
You gotta do this yay~
-}
linkConstructor :
    ID
    -> Int
    -> Int
    -> Int
    -> Int
    -> Maybe String
    -> String
    -> Link
linkConstructor id x y width height color url =
    { id = id
    , x = x
    , y = y
    , width = width
    , height = height
    , color = color
    , url = url
    }


{-| Encodes a link node as a JSON object.
-}
linkEncoder :
    String
    -> Link
    -> Encode.Value
linkEncoder typeStr link =
    Encode.object <|
        canvasNodeToJSONVals typeStr link
        ++ [("url", Encode.string link.url)]


----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
--------------------- LINK -------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------


{-| A node that represents a group.
-}
type alias Group =
    CanvasNode
        { label : Maybe String
        }


{-| Decodes a corresponding JSON object to a Group node.
-}
groupDecoder : Decoder Group
groupDecoder =
    succeed groupConstructor
    |> required "id" ID.decoder
    |> required "x" int
    |> required "y" int
    |> required "width" int
    |> required "height" int
    |> optional "color" (maybe string) Nothing
    |> optional "label" (maybe string) Nothing


{-| Elm doesn't allow for creating records using their alias name as a constructor.
You gotta do this yay~
-}
groupConstructor :
    ID
    -> Int
    -> Int
    -> Int
    -> Int
    -> Maybe String
    -> Maybe String
    -> Group
groupConstructor id x y width height color label =
    { id = id
    , x = x
    , y = y
    , width = width
    , height = height
    , color = color
    , label = label
    }


{-| Encodes a link node as a JSON object.
-}
groupEncoder :
    String
    -> Group
    -> Encode.Value
groupEncoder typeStr group =
    Encode.object <|
        canvasNodeToJSONVals typeStr group
        ++ packMaybeJSONValue "label" Encode.string group.label
