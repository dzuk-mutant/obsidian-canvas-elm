module Canvas.Node exposing
    ( Node(..)
    , decoder
    , encode

    , Base
    , changeX, changeY, changeWidth, changeHeight, changeColor
    , changePos, changeSize

    , File
    , fileFromValues, convertToFile
    , changeFile
    , changeSubpath

    , Text
    , textFromValues, convertToText
    , changeText

    , Link
    , linkFromValues, convertToLink
    , changeURL

    , Group
    , groupFromValues, convertToGroup
    , changeLabel
    )

{-| Nodes are the objects on a Canvas that contain content.

@docs Node, decoder, encode

-------------------------

# Node data

## Base node stuff

@docs Base

### Changing values

@docs changeX, changeY, changeWidth, changeHeight, changeColor
@docs changePos, changeSize

-------------------------

## File

@docs File

### Creating a File Node

@docs fileFromValues, convertToFile

### Changing values

@docs changeFile, changeSubpath

-------------------------

## Text

@docs Text

### Creating a Text Node

@docs textFromValues, convertToText

### Changing values

@docs changeText

-------------------------

## Link

@docs Link

### Creating a Link Node

@docs linkFromValues, convertToLink

### Changing values

@docs changeURL

-------------------------

## Group

@docs Group

### Creating a Group Node

@docs groupFromValues, convertToGroup

### Changing values

@docs changeLabel


-}

import Canvas.Color exposing (Color)
import Canvas.Helper exposing (packMaybeJSONValue)
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
        , x : Int
        , y : Int
        , width : Int
        , height : Int
        , color : Maybe Color
    }


{-| A Node base that only covers the basics.

For converting one node type into another.
-}
type alias Convertible = 
    { id : ID
    , x : Int
    , y : Int
    , width : Int
    , height : Int
    , color : Maybe Color
    }


{-| A hwlper function to convert one node type into another.
-}
toConvertible : Base r -> Convertible
toConvertible (NodeData node) =
    { id = node.id
    , x = node.x
    , y = node.y
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
    , ("x", Encode.int node.x)
    , ("y", Encode.int node.y)
    , ("width", Encode.int node.width)
    , ("height", Encode.int node.height)
    ]
    ++ packMaybeJSONValue "color" Encode.string node.color


{-| Change the X-coordinate of a node.

    Node.changeX 12 node
-}
changeX : Int -> Base r -> Base r
changeX newVal (NodeData node) =
    NodeData { node | x = newVal }


{-| Change the Y-coordinate of a node.

    Node.changeY -567 node
-}
changeY : Int -> Base r -> Base r
changeY newVal (NodeData node) =
    NodeData { node | y = newVal }


{-| Change the width of a node.

    Node.changeWidth 123 node
-}
changeWidth : Int -> Base r -> Base r
changeWidth newVal (NodeData node) =
    NodeData { node | width = newVal }


{-| Change the height of a node.

    Node.changeHeight 250 node
-}
changeHeight : Int -> Base r -> Base r
changeHeight newVal (NodeData node) =
    NodeData { node | height = newVal }


{-| Change the height of a node.

    Node.changeColor "#FFFFFF" node
-}
changeColor : Maybe Color -> Base r -> Base r
changeColor newVal (NodeData node) =
    NodeData { node | color = newVal }


{-| Changes the position of a node.
It combines `changeX` and `changeY` in a single declaration.

    Node.changePos 12 -837 node
-}
changePos : Int -> Int -> Base r -> Base r
changePos valX valY node =
    node
    |> changeX valX
    |> changeY valY


{-| Changes the size of a node.
It combines `changeWidth` and `changeHeight` in a single declaration.

    Node.changeSize 680 200 node
-}
changeSize : Int -> Int -> Base r -> Base r
changeSize valWidth valHeight node =
    node
    |> changeWidth valWidth
    |> changeHeight valHeight


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
        5346345 -- TODO: Figure out how to dynamically generate IDs.
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
        convertible.x
        convertible.y
        convertible.width
        convertible.height
        convertible.color
        file
        subpath



{-| Change the filepath of a File.

    Node.changeFile "new/path/here" file
-}
changeFile : String -> File -> File
changeFile newVal (NodeData node) =
    NodeData { node | file = newVal }


{-| Change the subpath of a Node.

    Node.changeSubpath (Just "something") file
-}
changeSubpath : Maybe String -> File -> File
changeSubpath newVal (NodeData node) =
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
        , x = x
        , y = y
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
        5346345 -- TODO: Figure out how to dynamically generate IDs.
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
        convertible.x
        convertible.y
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
textEncoder typeStr (NodeData text) =
    Encode.object <|
        canvasNodeToJSONVals typeStr (NodeData text)
        ++ [("text", Encode.string text.text)]


{-| Change the text of a Text.

    Node.changeText "some new thought I've had" text
-}
changeText : String -> Text -> Text
changeText newVal (NodeData node) =
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
        5346345 -- TODO: Figure out how to dynamically generate IDs.
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
        convertible.x
        convertible.y
        convertible.width
        convertible.height
        convertible.color
        url


{-| Change the URL of a Link.

    Node.changeText "https://haha.business/" link
-}
changeURL : String -> Link -> Link
changeURL newVal (NodeData node) =
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
        5346345 -- TODO: Figure out how to dynamically generate IDs.
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
        convertible.x
        convertible.y
        convertible.width
        convertible.height
        convertible.color
        label



{-| Change the label of a Group.

    Node.changeLabel (Just "Plans") group
-}
changeLabel : Maybe String -> Group -> Group
changeLabel newVal (NodeData node) =
    NodeData { node | label = newVal }
