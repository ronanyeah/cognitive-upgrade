module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Cb exposing (cb)
import Element exposing (Attribute, Color, Element, column, el, fill, height, html, maximum, none, padding, paragraph, px, rgb255, spacing, text, width, wrappedRow)
import Json.Decode as Decode exposing (Decoder)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { view = ViewAll
      }
    , Cmd.none
    )


type Node
    = Tree String (List Node)
    | Leaf String


type Msg
    = GoTo View


type alias Model =
    { view : View }


type alias Entry =
    { name : String }


type alias Category =
    { name : String, entries : List Entry }


type alias Bias =
    { name : String, entries : List Category }


type View
    = ViewAll
    | ViewBias Bias
    | ViewCategory Category
    | ViewEntry Entry


decodeName : Decoder String
decodeName =
    Decode.field "name" Decode.string


decodeChildren : Decoder (List Node)
decodeChildren =
    decodeNode
        |> Decode.lazy
        |> Decode.list
        |> Decode.field "children"


decodeNode : () -> Decoder Node
decodeNode _ =
    Decode.oneOf
        [ decodeTree
        , decodeLeaf
        ]


decodeTree : Decoder Node
decodeTree =
    Decode.map2 Tree
        decodeName
        decodeChildren


decodeLeaf : Decoder Node
decodeLeaf =
    Decode.map Leaf decodeName


viewNode : Node -> Element msg
viewNode n =
    case n of
        Tree name children ->
            column [ padding 10 ]
                [ el [] <| text name
                , children
                    |> List.map viewNode
                    |> column []
                ]

        Leaf name ->
            el [] <| text <| "###" ++ name


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoTo v ->
            ( { model | view = v }, Cmd.none )


view : Model -> Document Msg
view _ =
    { title = "Cognitive Upgrade"
    , body =
        [ text "yeah"
            |> Element.layout []
        ]
    }
