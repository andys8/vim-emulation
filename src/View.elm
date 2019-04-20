module View exposing (view, viewDocument)

import Browser exposing (Document)
import Buffer exposing (bufferToLines, cursorInNormalModeLine, cursorLine_, splitLine)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (id, tabindex)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decode
import Model exposing (Cursor(..), Mode(..), Model, Msg(..))


onKeyDown : Html.Attribute Msg
onKeyDown =
    Decode.field "key" Decode.string
        |> Decode.map (\m -> ( KeyDown m, True ))
        |> preventDefaultOn "keydown"


viewDocument : Model -> Document Msg
viewDocument model =
    { title = "Vim"
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    Element.layout
        [ height fill
        , width fill
        , htmlAttribute onKeyDown
        , htmlAttribute <| tabindex 0
        , htmlAttribute <| id "outermost"
        ]
    <|
        column
            [ width fill
            , height fill
            , Font.family [ Font.monospace ]
            , Font.size fontSize
            ]
            [ viewBufferNames
            , viewBuffer model
            , viewAirline model
            , viewAllKeyStrokes model.allKeyStrokes
            ]


viewBufferNames : Element msg
viewBufferNames =
    row [ width fill, padding 4, Background.color (rgb255 222 222 222) ]
        [ text "Buffer" ]


viewBuffer : Model -> Element msg
viewBuffer { cursor, buffer, mode } =
    let
        lines =
            bufferToLines buffer

        lineNumbers =
            lines
                |> List.indexedMap (\a _ -> a)
                |> List.map ((+) 1 >> String.fromInt >> text >> el [ width fill ])
                |> column [ Font.alignRight, paddingXY 8 0, alignTop, Background.color (rgb255 240 240 240) ]

        bufferLines =
            lines
                |> List.indexedMap (viewBufferLine mode cursor)
                |> column [ alignTop ]
    in
    row
        [ alignTop, height fill, width fill ]
        [ lineNumbers
        , bufferLines
        ]


viewBufferLine : Mode -> Cursor -> Int -> String -> Element msg
viewBufferLine mode cursor lineNumber lineContent =
    row [ height (minimum fontSize fill) ] <|
        if lineNumber == cursorLine_ cursor then
            let
                (Cursor _ normalizedCursorChar) =
                    if mode == Normal then
                        cursorInNormalModeLine lineContent cursor

                    else
                        cursor

                ( before, middle, after ) =
                    splitLine normalizedCursorChar lineContent
            in
            [ text before, viewCursor middle, text after ]

        else
            [ text lineContent ]


viewCursor : String -> Element msg
viewCursor charUnderCursor =
    el [ Background.color (rgb255 100 100 100), Font.color (rgb255 255 255 255) ] <|
        if String.isEmpty charUnderCursor then
            text " "

        else
            text charUnderCursor


viewAirline : Model -> Element msg
viewAirline model =
    row [ alignBottom, width fill, Background.color (rgb255 222 222 222) ]
        [ el [ Background.color (rgb255 200 200 200), padding 4, Font.bold ] <| text (modeToString model.mode) ]


viewAllKeyStrokes : List String -> Element msg
viewAllKeyStrokes keyStrokes =
    row
        [ alignBottom, Font.color (rgb255 170 170 170), height (minimum fontSize fill) ]
        [ text <| String.join " " keyStrokes ]


modeToString : Mode -> String
modeToString mode =
    case mode of
        Insert ->
            "Insert"

        Normal ->
            "Normal"



-- Constants


fontSize : Int
fontSize =
    20
