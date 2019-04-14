module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Element exposing (Element, alignBottom, alignTop, column, el, fill, height, layout, padding, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as Decode
import List.Extra
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
    | SetCursor Cursor


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
                ( newModel, msgs ) =
                    if model.mode == Normal then
                        handleNormalMode model key

                    else if model.mode == Insert then
                        handleInsertMode model key

                    else
                        ( model, [] )
            in
            ( { newModel | keyStrokes = key :: model.keyStrokes }, Cmd.none )
                |> sequence update msgs

        SetMode mode ->
            ( { model | mode = mode }, Cmd.none )

        SetCursor cursor ->
            ( { model | cursor = cursor }, Cmd.none )

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
                splitLine cursorChar <| emptyToSpace lineContent

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


handleInsertMode : Model -> String -> ( Model, List Msg )
handleInsertMode ({ bufferContent, cursor } as model) keyDown =
    if keyDown == "Escape" then
        ( model, [ SetMode Normal ] )

    else
        let
            ( beforeCursor, atCursor, afterCursor ) =
                splitBufferContent cursor bufferContent

            ( newBufferContent, newCursor ) =
                case keyDown of
                    "Enter" ->
                        ( beforeCursor ++ "\n" ++ atCursor ++ afterCursor
                        , cursor
                        )

                    "Backspace" ->
                        ( String.dropRight 1 beforeCursor ++ atCursor ++ afterCursor
                        , cursorMoveLeft cursor
                        )

                    "Shift" ->
                        ( bufferContent, cursor )

                    "Alt" ->
                        ( bufferContent, cursor )

                    "Meta" ->
                        ( bufferContent, cursor )

                    "Escape" ->
                        ( bufferContent, cursor )

                    _ ->
                        ( beforeCursor ++ keyDown ++ atCursor ++ afterCursor
                        , cursorMoveRight cursor
                        )
        in
        ( { model | bufferContent = newBufferContent, cursor = newCursor }, [] )


handleNormalMode : Model -> String -> ( Model, List Msg )
handleNormalMode ({ cursor, bufferContent } as model) keyDown =
    let
        (Cursor cursorLine cursorChar) =
            cursor
    in
    case keyDown of
        "i" ->
            ( model, [ SetMode Insert ] )

        "h" ->
            if cursorChar > 0 then
                ( model, [ SetCursor (cursorMoveLeft cursor) ] )

            else
                ( model, [] )

        "j" ->
            if cursorLine < List.length (String.lines bufferContent) - 1 then
                ( model, [ SetCursor (cursorMoveDown cursor) ] )

            else
                ( model, [] )

        "k" ->
            if cursorLine > 0 then
                ( model, [ SetCursor (cursorMoveUp cursor) ] )

            else
                ( model, [] )

        "l" ->
            if cursorChar < (String.length (currentBufferLine model) - 1) then
                ( model, [ SetCursor (cursorMoveRight cursor) ] )

            else
                ( model, [] )

        _ ->
            ( model, [] )


currentBufferLine : Model -> String
currentBufferLine { bufferContent, cursor } =
    let
        (Cursor cursorLine _) =
            cursor
    in
    String.lines bufferContent
        |> List.Extra.getAt cursorLine
        -- Note: Not sure if this is a good idea.
        -- Shouldn't be possible and maybe handling is overhead, but defaulting can lead to errors.
        |> Maybe.withDefault ""


emptyToSpace : String -> String
emptyToSpace s =
    case s of
        "" ->
            " "

        _ ->
            s


splitBufferContent : Cursor -> String -> ( String, String, String )
splitBufferContent (Cursor cursorLine cursorChar) content =
    let
        lines =
            String.lines content

        linesBefore =
            String.join "" <|
                List.take cursorLine lines

        currentLine =
            String.join "" <|
                List.take 1 <|
                    List.drop cursorLine lines

        linesAfter =
            String.join "" <|
                List.drop (cursorLine + 1) lines

        ( beforeCurrentLine, middleCurrentLine, afterCurrentLine ) =
            splitLine cursorChar currentLine
    in
    ( linesBefore ++ beforeCurrentLine, middleCurrentLine, afterCurrentLine ++ linesAfter )
        |> Debug.log "triplet"


splitLine : Int -> String -> ( String, String, String )
splitLine cursorChar content =
    let
        -- The cursor can be positioned behind the last char when moved from a longer line
        charAt =
            if cursorChar >= String.length content then
                String.length content - 1

            else
                cursorChar

        before =
            String.slice 0 charAt content

        middle =
            String.slice charAt (charAt + 1) content

        after =
            String.slice (charAt + 1) (String.length content) content
    in
    ( before, middle, after )


cursorMoveRight : Cursor -> Cursor
cursorMoveRight (Cursor line char) =
    Cursor line (char + 1)


cursorMoveLeft : Cursor -> Cursor
cursorMoveLeft (Cursor line char) =
    Cursor line (char - 1)


cursorMoveUp : Cursor -> Cursor
cursorMoveUp (Cursor line char) =
    Cursor (line - 1) char


cursorMoveDown : Cursor -> Cursor
cursorMoveDown (Cursor line char) =
    Cursor (line + 1) char
