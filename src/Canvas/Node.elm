module Canvas.Node exposing
    ( Node(..)
    , decoder
    , encode

    , Base
    , setPosition, setWidth, setHeight, setColor
    , setSize

    , File
    , fileFromValues, convertToFile
    , setFilepath, setSubpath

    , Text
    , textFromValues, convertToText
    , setText

    , Link
    , linkFromValues, convertToLink
    , setURL

    , Group
    , groupFromValues, convertToGroup
    , setLabel
    )

{-| Nodes are the objects on a Canvas that contain content.

@docs Node, decoder, encode

-------------------------

# Node data

## Base node stuff

@docs Base

### Changing values

@docs setX, setY, setWidth, setHeight, setColor
@docs setPosition, setSize

-------------------------

## File

@docs File

### Creating a File Node

@docs fileFromValues, convertToFile

### Changing values

@docs setFilepath, setSubpath

-------------------------

## Text

@docs Text

### Creating a Text Node

@docs textFromValues, convertToText

### Changing values

@docs setText

-------------------------

## Link

@docs Link

### Creating a Link Node

@docs linkFromValues, convertToLink

### Changing values

@docs setURL

-------------------------

## Group

@docs Group

### Creating a Group Node

@docs groupFromValues, convertToGroup

### Changing values

@docs setLabel


-}

import Canvas.Color exposing (Color)
import Canvas.Helper exposing (packMaybeJSONValue)
import Canvas.Position as Position exposing (Position)
import Canvas.ID as ID exposing (ID)
import Json.Decode as Decode exposing (Decoder, andThen, string, int, maybe, fail, field, succeed)
import Json.Decode.Pipeline exposing (required, optional)
import Json.Encode as Encode



{-| A Node type.

A Node can be multiple similar things, which is represented
here as a Custom Type.
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


{-| Encodes a Node into a JSON object.
-}
encode : Node -> Encode.Value
encode node =
    {- (dict keys are given here so they're all in one place
    like with nodeDecoder.)
    -}
    case node of
        FileNode f -> fileEncoder "file" f
        TextNode t -> textEncoder "text" t
        LinkNode l -> linkEncoder "link" l
        GroupNode g -> groupEncoder "group" g


----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
-------------------- NODE BASE ---------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------
----------------------------------------------------


{-| All nodes have shared fields, represented by
this extensible record.

This type is opaque for more stable API interactions.
-}
type Base r = 
    NodeData
    { r | id : ID
        , position : Position
        , width : Int
        , height : Int
        , color : Maybe Color
    }


{-| A Node base that only covers the basics.

For converting one node type into another.
-}
type alias Convertible = 
    { id : ID
    , position : Position
    , width : Int
    , height : Int
    , color : Maybe Color
    }


{-| A hwlper function to convert one node type into another.
-}
toConvertible : Base r -> Convertible
toConvertible (NodeData node) =
    { id = node.id
    , position = node.position
    , width = node.width
    , height = node.height
    , color = node.color
    }


{-| Packs the  shared section of a Node for encoding.

Must be chained together with another function
to make a valid node.
-}
canvasNodeToJSONVals :
    String
    -> Base r
    -> List (String, Encode.Value)
canvasNodeToJSONVals typeStr (NodeData node) =
    [ ("id", Encode.string (ID.toString node.id))
    , ("type", Encode.string typeStr)
    , ("width", Encode.int node.width)
    , ("height", Encode.int node.height)
    ]
    ++ Position.encodeList node.position
    ++ packMaybeJSONValue "color" Encode.string node.color


{-| Change the X-coordinate of a node.

    Node.setX 12 node
-}
setPosition : Int -> Int -> Base r -> Base r
setPosition xVal yVal (NodeData node) =
    NodeData { node | position = Position.set xVal yVal node.position }


{-| Change the width of a node.

    Node.setWidth 123 node
-}
setWidth : Int -> Base r -> Base r
setWidth newVal (NodeData node) =
    NodeData { node | width = newVal }


{-| Change the height of a node.

    Node.setHeight 250 node
-}
setHeight : Int -> Base r -> Base r
setHeight newVal (NodeData node) =
    NodeData { node | height = newVal }


{-| Change the height of a node.

    Node.setColor "#FFFFFF" node
-}
setColor : Maybe Color -> Base r -> Base r
setColor newVal (NodeData node) =
    NodeData { node | color = newVal }


{-| Changes the size of a node.
It combines `setWidth` and `setHeight` in a single declaration.

    Node.setSize 680 200 node
-}
setSize : Int -> Int -> Base r -> Base r
setSize valWidth valHeight node =
    node
    |> setWidth valWidth
    |> setHeight valHeight


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
    Base
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
    -> Maybe Color
    -> String
    -> Maybe String
    -> File
fileConstructor id x y width height color file subpath =
    NodeData
        { id = id
        , position = Position.fromValues x y
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
fileEncoder typeStr (NodeData file) =
    Encode.object <|
        canvasNodeToJSONVals typeStr (NodeData file)
        ++ ("text", Encode.string file.file)
        :: packMaybeJSONValue "subpath" Encode.string file.subpath


{-| Creates a File node from a series of values.

Best used with `FileNode` to create a `Node`.

    FileNode <| Node.fileFromValues
        { x = 120
        , y = -39
        , width = 500
        , height = 204
        , color = Nothing
        , file = "file/path"
        , subpath = Nothing
        }
-}
fileFromValues :
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , color : Maybe Color
    , file : String
    , subpath : Maybe String
    }
    -> File
fileFromValues {x, y, width, height, color, file, subpath} =
    fileConstructor
        (ID.fromInt 5346345) -- TODO: Figure out how to dynamically generate IDs.
        x
        y
        width
        height
        color
        file
        subpath


{-| Converts any node to a File.

Any data points that aren't part of the [`Base`](#Base) type will be removed.

    Node.convertToFile
        { path = "path/to/thing"
        , subpath = Nothing
        }
        node
-}
convertToFile :
    Base r
    ->  { file : String 
        , subpath : Maybe String
        }
    -> File
convertToFile node {file, subpath} =
    let
        convertible = toConvertible node
    
    in
    fileConstructor
        convertible.id
        (Position.x convertible.position)
        (Position.y convertible.position)
        convertible.width
        convertible.height
        convertible.color
        file
        subpath


{-| Change the filepath of a File.

    Node.setFilepath "new/path/here" file
-}
setFilepath : String -> File -> File
setFilepath newVal (NodeData node) =
    NodeData { node | file = newVal }


{-| Change the subpath of a Node.

    Node.setSubpath (Just "something") file
-}
setSubpath : Maybe String -> File -> File
setSubpath newVal (NodeData node) =
    NodeData { node | subpath = newVal }


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
    Base
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
    -> Maybe Color
    -> String
    -> Text
textConstructor id x y width height color text =
    NodeData
        { id = id
        , position = Position.fromValues x y
        , width = width
        , height = height
        , color = color
        , text = text
        }


{-| Creates a Text node from a series of values.

Best used with `TextNode` to create a `Node`.

    TextNode <| Node.textFromValues
        { x = 120
        , y = -39
        , width = 500
        , height = 204
        , color = Nothing
        , text = "My note goes something like this..."
-}
textFromValues :
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , color : Maybe Color
    , text : String
    }
    -> Text
textFromValues {x, y, width, height, color, text} =
    textConstructor
        (ID.fromInt 5346345) -- TODO: Figure out how to dynamically generate IDs.
        x
        y
        width
        height
        color
        text


{-| Converts any node to a Text.

Any data points that aren't part of the [`Base`](#Base) type will be removed.

    Node.convertToText
        { text = "My note thing"
        }
        node
-}
convertToText :
    Base r
    ->  { text : String 
        }
    -> Text
convertToText node {text} =
    let
        convertible = toConvertible node
    
    in
    textConstructor
        convertible.id
        (Position.x convertible.position)
        (Position.y convertible.position)
        convertible.width
        convertible.height
        convertible.color
        text


{-| Encodes a text node as a JSON object.
-}
textEncoder :
    String
    -> Text
    -> Encode.Value
textEncoder typeStr (NodeData node) =
    Encode.object <|
        canvasNodeToJSONVals typeStr (NodeData node)
        ++ [("text", Encode.string node.text)]


{-| Change the text of a Text.

    Node.setText "some new thought I've had" text
-}
setText : String -> Text -> Text
setText newVal (NodeData node) =
    NodeData { node | text = newVal }


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
    Base
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
    NodeData
        { id = id
        , position = Position.fromValues x y
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
linkEncoder typeStr (NodeData link) =
    Encode.object <|
        canvasNodeToJSONVals typeStr (NodeData link)
        ++ [("url", Encode.string link.url)]


{-| Creates a Link node from a series of values.

Best used with `LinkNode` to create a `Node`.

    LinkNode <| Node.linkFromValues
        { x = 120
        , y = -39
        , width = 500
        , height = 204
        , color = Nothing
        , url = "https://haha.business/"
-}
linkFromValues :
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , color : Maybe Color
    , url : String
    }
    -> Link
linkFromValues {x, y, width, height, color, url} =
    linkConstructor
        (ID.fromInt 5346345) -- TODO: Figure out how to dynamically generate IDs.
        x
        y
        width
        height
        color
        url


{-| Converts any node to a Link.

Any data points that aren't part of the [`Base`](#Base) type will be removed.

    Node.convertToLink
        { url = "https://haha.business"
        }
        node
-}
convertToLink :
    Base r
    ->  { url : String 
        }
    -> Link
convertToLink node {url} =
    let
        convertible = toConvertible node
    
    in
    linkConstructor
        convertible.id
        (Position.x convertible.position)
        (Position.y convertible.position)
        convertible.width
        convertible.height
        convertible.color
        url


{-| Change the URL of a Link.

    Node.setText "https://haha.business/" link
-}
setURL : String -> Link -> Link
setURL newVal (NodeData node) =
    NodeData { node | url = newVal }


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
    Base
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
    NodeData
        { id = id
        , position = Position.fromValues x y
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
groupEncoder typeStr (NodeData group) =
    Encode.object <|
        canvasNodeToJSONVals typeStr (NodeData group)
        ++ packMaybeJSONValue "label" Encode.string group.label


{-| Creates a Group node from a series of values.

Best used with `GroupNode` to create a `Node`.

    GroupNode <| Node.groupFromValues
        { x = 120
        , y = -39
        , width = 500
        , height = 204
        , color = Nothing
        , label = "Plans"
-}
groupFromValues :
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , color : Maybe Color
    , label : Maybe String
    }
    -> Group
groupFromValues {x, y, width, height, color, label} =
    groupConstructor
        (ID.fromInt 5346345) -- TODO: Figure out how to dynamically generate IDs.
        x
        y
        width
        height
        color
        label


{-| Converts any node to a Group.

Any data points that aren't part of the [`Base`](#Base) type will be removed.

    Node.convertToGroup
        { label = Just "Plans"
        }
        node
-}
convertToGroup :
    Base r
    ->  { label : Maybe String 
        }
    -> Group
convertToGroup node {label} =
    let
        convertible = toConvertible node
    
    in
    groupConstructor
        convertible.id
        (Position.x convertible.position)
        (Position.y convertible.position)
        convertible.width
        convertible.height
        convertible.color
        label


{-| Change the label of a Group.

    Node.setLabel (Just "Plans") group
-}
setLabel : Maybe String -> Group -> Group
setLabel newVal (NodeData node) =
    NodeData { node | label = newVal }
