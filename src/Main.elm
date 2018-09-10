module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation exposing (Key)
import Cb exposing (cb)
import Element exposing (Attribute, Color, Element, centerX, centerY, column, el, fill, height, html, maximum, newTabLink, none, padding, paragraph, px, rgb255, row, spacing, text, width, wrappedRow)
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg, svg)
import Svg.Attributes as SA
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
      , data =
            cb
                |> Decode.decodeString
                    (Decode.field "children"
                        (Decode.list decodeBias)
                    )
                |> Result.withDefault []
      }
    , Cmd.none
    )


type Msg
    = GoTo View


type alias Model =
    { view : View
    , data : List Bias
    }


type alias Entry =
    { wiki : Int, name : String, example : String }


type alias Category =
    { name : String, entries : List Entry }


type alias Bias =
    { name : String, categories : List Category }


type View
    = ViewAll
    | ViewBias Bias
    | ViewCategory Bias Category
    | ViewEntry Bias Category Entry


decodeName : Decoder String
decodeName =
    Decode.field "name" Decode.string


decodeCategory : Decoder Category
decodeCategory =
    Decode.map2 Category
        decodeName
        (Decode.field "children" (Decode.list decodeEntry))


decodeBias : Decoder Bias
decodeBias =
    Decode.map2 Bias
        decodeName
        (Decode.field "children" (Decode.list decodeCategory))


decodeEntry : Decoder Entry
decodeEntry =
    Decode.map3 Entry
        (Decode.field "wiki" Decode.int)
        decodeName
        (Decode.field "example" Decode.string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoTo v ->
            ( { model | view = v }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Cognitive Upgrade"
    , body =
        [ Html.node "meta"
            [ Html.Attributes.name "viewport"
            , Html.Attributes.attribute "content"
                "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
            ]
            []
        , (case model.view of
            ViewAll ->
                model.data
                    |> List.map
                        (\bias ->
                            button []
                                { onPress = Just <| GoTo <| ViewBias bias
                                , label = txt bias.name
                                }
                        )
                    |> column [ spacing 10 ]

            ViewBias bias ->
                column [ spacing 20 ]
                    [ button []
                        { onPress = Just <| GoTo ViewAll
                        , label = txt <| "Bias: " ++ bias.name
                        }
                    , bias.categories
                        |> List.map
                            (\category ->
                                button []
                                    { onPress = Just <| GoTo <| ViewCategory bias category
                                    , label = txt <| "· " ++ category.name
                                    }
                            )
                        |> column [ spacing 10 ]
                    , reset
                    ]

            ViewCategory bias category ->
                column [ spacing 20 ]
                    [ button []
                        { onPress = Just <| GoTo ViewAll
                        , label = txt <| "Bias: " ++ bias.name
                        }
                    , button []
                        { onPress = Just <| GoTo <| ViewBias bias
                        , label = txt <| "Category: " ++ category.name
                        }
                    , category.entries
                        |> List.map
                            (\entry ->
                                button []
                                    { onPress = Just <| GoTo <| ViewEntry bias category entry
                                    , label = txt <| "· " ++ entry.name
                                    }
                            )
                        |> column [ spacing 10 ]
                    , reset
                    ]

            ViewEntry bias category entry ->
                column [ spacing 20 ]
                    [ button []
                        { onPress = Just <| GoTo ViewAll
                        , label = txt <| "Bias: " ++ bias.name
                        }
                    , button []
                        { onPress = Just <| GoTo <| ViewCategory bias category
                        , label = txt <| "Category: " ++ category.name
                        }
                    , newTabLink []
                        { url = "https://en.wikipedia.org/wiki?curid=" ++ String.fromInt entry.wiki
                        , label =
                            row [ spacing 5 ]
                                [ el [] <| text entry.name
                                , book
                                ]
                        }
                    , reset
                    ]
          )
            |> (\x ->
                    column [ centerX, spacing 20 ]
                        [ el [ Font.size 35, centerX ] <| text "Cognitive Upgrade"
                        , x
                        ]
               )
            |> Element.layout
                [ padding 20
                , Bg.color blue
                , Font.color pink
                , Font.size 25
                , font
                ]
        ]
    }


reset : Element Msg
reset =
    button [ Border.rounded 10, Border.width 2, centerX, padding 10 ]
        { onPress = Just <| GoTo ViewAll
        , label = el [] <| text "Back to Start"
        }


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ SA.class <| "feather feather-" ++ className
        , SA.fill "none"
        , SA.height "24"
        , SA.stroke "currentColor"
        , SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.strokeWidth "2"
        , SA.viewBox "0 0 24 24"
        , SA.width "24"
        ]


book : Element msg
book =
    svgFeatherIcon "book"
        [ Svg.path [ SA.d "M4 19.5A2.5 2.5 0 0 1 6.5 17H20" ] []
        , Svg.path [ SA.d "M6.5 2H20v20H6.5A2.5 2.5 0 0 1 4 19.5v-15A2.5 2.5 0 0 1 6.5 2z" ] []
        ]
        |> html


pink : Color
pink =
    rgb255 164 103 152


blue : Color
blue =
    rgb255 150 208 255


font : Attribute msg
font =
    Font.family
        [ Font.external
            { name = "Oswald"
            , url = "https://fonts.googleapis.com/css?family=Oswald"
            }
        , Font.sansSerif
        ]


txt : String -> Element msg
txt =
    text >> List.singleton >> paragraph []
