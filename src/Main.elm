module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Element exposing (Element, alignBottom, alignTop, column, el, fill, height, layout, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as Decode
import Platform.Sub as Sub exposing (Sub)
import Update.Extra exposing (sequence)


type alias Model =
    { keyStrokes : List String
    , bufferContent : String
    , mode : Mode
    , cursor : Cursor
    }


type Msg
    = NoOp
    | KeyDown String
    | SetMode Mode


type Mode
    = Normal
    | Insert


type Cursor
    = Cursor Int Int


initModel : Model
initModel =
    { keyStrokes = []
    , bufferContent = ""
    , mode = Normal
    , cursor = Cursor 0 0
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


sub : Sub Msg
sub =
    Browser.Events.onKeyDown keyDecoder
        |> Sub.map KeyDown


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> sub
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            let
                setModeCmd =
                    case ( model.mode, key ) of
                        ( Normal, "i" ) ->
                            Just (SetMode Insert)

                        ( Insert, "Escape" ) ->
                            Just (SetMode Normal)

                        _ ->
                            Nothing

                bufferContent =
                    if model.mode == Insert then
                        handleInsertMode model.bufferContent key

                    else
                        model.bufferContent
            in
            ( { model
                | keyStrokes = key :: model.keyStrokes
                , bufferContent = bufferContent
              }
            , Cmd.none
            )
                |> sequence update (List.filterMap identity [ setModeCmd ])

        SetMode mode ->
            ( { model | mode = mode }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout [ height fill, width fill ] <|
        column
            [ width fill, height fill, Font.family [ Font.monospace ] ]
            [ viewBufferNames
            , viewBuffer model.cursor model.bufferContent
            , viewAirline model
            , viewKeyBuffer model.keyStrokes
            ]


viewBufferNames : Element msg
viewBufferNames =
    row [ width fill, padding 4, Background.color (rgb255 222 222 222) ]
        [ text "Buffer" ]


viewBuffer : Cursor -> String -> Element msg
viewBuffer cursor bufferContent =
    let
        lines =
            String.lines bufferContent

        lineNumbers =
            lines
                |> List.indexedMap (\a _ -> a)
                |> List.map ((+) 1 >> String.fromInt >> text)
                |> column [ alignTop, Background.color (rgb255 240 240 240) ]

        bufferLines =
            lines
                |> List.indexedMap (viewBufferLine cursor)
                |> column [ alignTop ]
    in
    row
        [ alignTop, height fill, width fill, spacing 4 ]
        [ lineNumbers
        , bufferLines
        ]


viewBufferLine : Cursor -> Int -> String -> Element msg
viewBufferLine (Cursor cursorLine cursorChar) lineNumber lineContent =
    if lineNumber == cursorLine then
        let
            ( before, middle, after ) =
                splitLine cursorChar lineContent

            cursorElement =
                el [ Background.color (rgb255 100 100 100), Font.color (rgb255 255 255 255) ] <| text middle
        in
        row [] [ text before, cursorElement, text after ]

    else
        text (emptyToSpace lineContent)


viewAirline : Model -> Element msg
viewAirline model =
    row [ alignBottom, width fill, Background.color (rgb255 222 222 222) ]
        [ el [ Background.color (rgb255 200 200 200), padding 4 ] <| text (modeToString model.mode) ]


viewKeyBuffer : List String -> Element msg
viewKeyBuffer keyStrokes =
    row
        [ alignBottom, Font.size 10, padding 4 ]
        [ text <| String.join " " keyStrokes ]


modeToString : Mode -> String
modeToString mode =
    case mode of
        Insert ->
            "Insert"

        Normal ->
            "Normal"


handleInsertMode : String -> String -> String
handleInsertMode buffer key =
    case key of
        "Enter" ->
            buffer ++ "\n"

        "Backspace" ->
            String.dropRight 1 buffer

        "Shift" ->
            buffer

        "Alt" ->
            buffer

        "Meta" ->
            buffer

        "Escape" ->
            buffer

        _ ->
            buffer ++ key


emptyToSpace : String -> String
emptyToSpace s =
    case s of
        "" ->
            " "

        _ ->
            s


splitLine : Int -> String -> ( String, String, String )
splitLine charAt content =
    let
        before =
            String.slice 0 charAt content

        middle =
            String.slice charAt (charAt + 1) content

        after =
            String.slice (charAt + 1) (String.length content) content
    in
    ( before, middle, after )
