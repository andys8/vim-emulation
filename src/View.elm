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
        |> Decode.map (\key -> ( KeyDown key, True ))
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
            , viewCommandLine
            ]


viewBufferNames : Element msg
viewBufferNames =
    row [ width fill, Background.color colors.bufferNamesLineBg ]
        [ el [ paddingXY 10 4, Background.color colors.bufferNameBg ] (text "[No Name]")
        , el [ paddingXY 10 4, Font.color colors.white, Background.color colors.bufferNameRightBg, alignRight ] (text "buffers")
        ]


viewBuffer : Model -> Element msg
viewBuffer { cursor, buffer, mode } =
    let
        lines =
            bufferToLines buffer

        lineNumbers =
            lines
                |> List.indexedMap (\a _ -> a)
                |> List.map ((+) 1 >> String.fromInt >> text >> el [ width fill ])
                |> column [ Font.alignRight, paddingXY 10 0, alignTop, Font.color colors.lineFont ]

        bufferLines =
            lines
                |> List.indexedMap (viewBufferLine mode cursor)
                |> column [ alignTop, Font.color colors.bufferFont ]
    in
    row
        [ alignTop, height fill, width fill, Background.color colors.bufferBg ]
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
    el [ Background.color colors.bufferFont, Font.color colors.bufferBg ] <|
        if String.isEmpty charUnderCursor then
            text " "

        else
            text charUnderCursor


viewAirline : Model -> Element msg
viewAirline { mode, cursor, buffer } =
    let
        modeBackgroundColor =
            case mode of
                Insert ->
                    colors.airLineInsertModeBg

                _ ->
                    colors.airLineNormalModeBg

        currentLine =
            cursorLine_ cursor + 1

        totalLines =
            List.length <| bufferToLines buffer

        linesPercent =
            String.fromInt (floor ((toFloat currentLine / toFloat totalLines) * 100)) ++ "%"

        linesCount =
            String.fromInt currentLine ++ "/" ++ String.fromInt totalLines
    in
    row
        [ alignBottom, width fill, Background.color colors.airLineBg ]
        [ el
            [ Background.color modeBackgroundColor, paddingXY 10 4, Font.bold ]
            (text (modeToString mode))
        , row
            [ Background.color modeBackgroundColor, paddingXY 10 4, alignRight, spacing 10 ]
            [ el [ alignRight, Font.alignRight, width (shrink |> minimum 80) ] <| text linesPercent
            , el [ alignRight, Font.alignRight, width (shrink |> minimum 80), Font.bold ] <| text linesCount
            ]
        ]


viewCommandLine : Element msg
viewCommandLine =
    row
        [ alignBottom, padding 4, Background.color colors.bufferBg, Font.color colors.bufferFont, height (minimum fontSize fill), width fill ]
        [ text " " ]


modeToString : Mode -> String
modeToString mode =
    case mode of
        Insert ->
            "Insert"

        Normal ->
            "Normal"



-- Constants


colors =
    { white = rgb255 255 255 255
    , bufferNamesLineBg = rgb255 48 48 48
    , bufferNameBg = rgb255 175 135 255
    , bufferNameRightBg = rgb255 95 95 175
    , bufferBg = rgb255 38 38 38
    , bufferFont = rgb255 255 215 175
    , lineFont = rgb255 118 118 118
    , airLineBg = rgb255 95 95 95
    , airLineNormalModeBg = rgb255 175 135 255
    , airLineInsertModeBg = rgb255 95 255 135
    }


fontSize : Int
fontSize =
    20
