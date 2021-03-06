module View exposing (view, viewDocument)

import Browser exposing (Document)
import Buffer exposing (bufferToLines, cursorInModeLine, cursorLine_, splitLine)
import Config exposing (Colors, config)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (id, tabindex)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decode
import Model exposing (Cursor(..), Mode(..), Model, Msg(..))
import Svg
import Svg.Attributes


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
            , viewCommandLine model
            ]


viewBufferNames : Element msg
viewBufferNames =
    row
        [ width fill
        , lineHeight
        , Background.color colors.bufferNamesLineBg
        ]
        [ el [ paddingXY 10 yPadding, Background.color colors.bufferNameBg ] (text "[No Name]")
        , el [ alignLeft ] <| viewArrow ArrowRight colors.bufferNameBg
        , el [ alignRight ] <| viewArrow ArrowLeft colors.bufferNameRightBg
        , el [ paddingXY 10 yPadding, Font.color colors.white, Background.color colors.bufferNameRightBg, alignRight ] (text "buffers")
        ]


viewBuffer : Model -> Element msg
viewBuffer { cursor, buffer, mode } =
    let
        lines =
            bufferToLines buffer

        lineNumbers =
            lines
                |> List.indexedMap (\index _ -> index - cursorLine_ cursor)
                |> List.map (viewBufferLineNumber cursor)
                |> column [ Font.alignRight, alignTop ]

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
    row [ lineHeight ] <|
        if lineNumber == cursorLine_ cursor && mode /= Command then
            let
                (Cursor _ normalizedCursorChar) =
                    cursorInModeLine mode lineContent cursor

                { before, middle, after } =
                    splitLine normalizedCursorChar lineContent
            in
            [ text before, viewCursor middle, text after ]

        else
            [ text lineContent ]


viewBufferLineNumber : Cursor -> Int -> Element msg
viewBufferLineNumber cursor lineDifference =
    let
        attributes =
            [ width fill, lineHeight, width (minimum 48 fill) ]
    in
    case lineDifference of
        0 ->
            el
                (attributes
                    ++ [ paddingXY 0 yPadding
                       , Background.color colors.black
                       , Font.alignLeft
                       , Font.color colors.lineNumberCurrentFont
                       ]
                )
                (text (String.fromInt (cursorLine_ cursor + 1)))

        _ ->
            el
                (attributes
                    ++ [ paddingXY 10 yPadding
                       , Font.alignRight
                       , Font.color colors.lineNumberFont
                       ]
                )
                (text (String.fromInt (abs lineDifference)))


viewCursor : String -> Element msg
viewCursor charUnderCursor =
    el [ Background.color colors.cursor, Font.color colors.bufferBg, paddingXY 0 yPadding, lineHeight ] <|
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
        [ alignBottom
        , width fill
        , lineHeight
        , Font.color colors.black
        , Background.color colors.airLineBg
        ]
        [ el
            [ Background.color modeBackgroundColor, paddingXY 10 yPadding, Font.bold ]
            (text (modeToString mode))
        , el [ alignLeft ] <| viewArrow ArrowRight modeBackgroundColor
        , el [ alignRight ] <| viewArrow ArrowLeft modeBackgroundColor
        , row
            [ Background.color modeBackgroundColor, paddingXY 10 yPadding, alignRight, spacing 10 ]
            [ el [ alignRight, Font.alignRight, width (shrink |> minimum 80) ] <| text linesPercent
            , el [ alignRight, Font.alignRight, width (shrink |> minimum 80), Font.bold ] <| text linesCount
            ]
        ]


viewCommandLine : Model -> Element msg
viewCommandLine { mode, commandLine } =
    let
        content =
            case mode of
                Command ->
                    [ text ":"
                    , text commandLine
                    , viewCursor ""
                    ]

                Insert ->
                    [ el [ Font.bold ] <| text "-- INSERT --" ]

                Normal ->
                    [ text " " ]
    in
    row
        [ alignBottom
        , padding yPadding
        , Background.color colors.bufferBg
        , Font.color colors.bufferFont
        , lineHeight
        , width fill
        ]
        content


modeToString : Mode -> String
modeToString mode =
    case mode of
        Insert ->
            "INSERT"

        _ ->
            "NORMAL"


viewArrow : ArrowDirection -> Color -> Element msg
viewArrow direction color =
    let
        points =
            case direction of
                ArrowRight ->
                    [ ( 0, 0 ), ( 10, 5 ), ( 0, 10 ) ]

                ArrowLeft ->
                    [ ( 10, 0 ), ( 0, 5 ), ( 10, 10 ) ]

        pointsToString =
            List.map (\( x, y ) -> String.fromInt x ++ "," ++ String.fromInt y)
                >> String.join " "

        svg =
            Svg.svg
                [ Svg.Attributes.viewBox "0 0 10 10"
                , Svg.Attributes.height "100%"
                , Svg.Attributes.preserveAspectRatio "none"
                ]
                [ Svg.polygon
                    [ Svg.Attributes.fill "currentColor"
                    , Svg.Attributes.stroke "none"
                    , Svg.Attributes.points (pointsToString points)
                    ]
                    []
                ]
    in
    el
        [ width (px 12), lineHeight, Font.color color ]
        (html svg)


type ArrowDirection
    = ArrowRight
    | ArrowLeft



-- Constants


colors : Colors
colors =
    config.colors


fontSize : Int
fontSize =
    20


yPadding : Int
yPadding =
    2


lineHeight : Attribute msg
lineHeight =
    height <| px <| fontSize + 2 * yPadding
