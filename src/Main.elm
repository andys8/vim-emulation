module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Element exposing (Element, alignBottom, alignTop, column, el, fill, height, layout, minimum, padding, paddingXY, rgb255, row, text, width)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as Decode
import List.Extra
import Platform.Sub as Sub exposing (Sub)
import Update.Extra exposing (sequence)


type alias Model =
    { allKeyStrokes : List String
    , bufferContent : String
    , mode : Mode
    , cursor : Cursor
    , keyStrokes : List String
    }


type Msg
    = NoOp
    | KeyDown String
    | SetMode Mode
    | ExecuteAction Action
    | SetCursor Cursor
    | InsertNewLine Int


type Action
    = DeleteLine Int


type Mode
    = Normal
    | Insert


type Cursor
    = Cursor Int Int


initModel : Model
initModel =
    { allKeyStrokes = []
    , bufferContent = ""
    , mode = Normal
    , cursor = Cursor 0 0
    , keyStrokes = []
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
                handleKey =
                    if model.mode == Insert then
                        handleInsertMode

                    else
                        handleNormalMode

                allKeyStrokes =
                    key :: model.allKeyStrokes

                keyStrokes =
                    key :: model.keyStrokes

                ( newModel, msgs ) =
                    { model | allKeyStrokes = allKeyStrokes, keyStrokes = keyStrokes }
                        |> handleKey key
            in
            ( newModel, Cmd.none )
                |> sequence update msgs

        SetMode Normal ->
            let
                cursor =
                    if cursorChar_ model.cursor > 0 then
                        cursorMoveLeft model.cursor

                    else
                        model.cursor
            in
            ( { model | mode = Normal, cursor = cursor }, Cmd.none )

        SetMode mode ->
            ( { model | mode = mode }, Cmd.none )

        SetCursor cursor ->
            ( { model | cursor = cursor }, Cmd.none )

        InsertNewLine lineNumber ->
            let
                ( linesBefore, linesAfter ) =
                    String.lines model.bufferContent
                        |> List.Extra.splitAt lineNumber

                bufferContent =
                    linesBefore ++ "" :: linesAfter |> String.join "\n"
            in
            ( { model | bufferContent = bufferContent }, Cmd.none )

        ExecuteAction action ->
            (case action of
                DeleteLine lineNumber ->
                    let
                        bufferContent =
                            model.bufferContent
                                |> String.lines
                                |> List.Extra.removeAt lineNumber
                                |> String.join "\n"

                        lastLineWasDeleted =
                            cursorLine_ model.cursor >= List.length (String.lines bufferContent)

                        moveCursor =
                            if lastLineWasDeleted then
                                cursorMoveUp >> cursorMoveLineBegin

                            else
                                cursorMoveLineBegin
                    in
                    ( { model | bufferContent = bufferContent, cursor = moveCursor model.cursor }, Cmd.none )
            )
                |> Update.Extra.updateModel (\model_ -> { model_ | keyStrokes = [] })

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout [ height fill, width fill ] <|
        column
            [ width fill, height fill, Font.family [ Font.monospace ], Font.size fontSize ]
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
viewBuffer { cursor, bufferContent, mode } =
    let
        lines =
            String.lines bufferContent

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
                        cursorInNormalModeLine (String.length lineContent) cursor

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


handleInsertMode : String -> Model -> ( Model, List Msg )
handleInsertMode keyDown ({ bufferContent, cursor } as model) =
    let
        ignoredKeys =
            [ "Alt", "Shift", "Meta", "Control", "ArrowRight", "ArrowLeft", "ArrowDown", "ArrowUp" ]
    in
    if keyDown == "Escape" then
        ( model, [ SetMode Normal ] )

    else if List.member keyDown ignoredKeys then
        ( model, [] )

    else
        let
            (Cursor cursorLine cursorChar) =
                cursor

            { linesBefore, before, middle, after, linesAfter } =
                splitBufferContent cursor bufferContent

            ( newCurrentLine, newCursor ) =
                case keyDown of
                    "Enter" ->
                        ( before ++ "\n" ++ middle ++ after
                        , Cursor (cursorLine + 1) 0
                        )

                    "Backspace" ->
                        ( String.dropRight 1 before ++ middle ++ after
                        , if cursorChar > 0 then
                            cursorMoveLeft cursor

                          else
                            cursor
                        )

                    "Delete" ->
                        ( before ++ after, cursor )

                    _ ->
                        ( before ++ keyDown ++ middle ++ after
                        , cursorMoveRight cursor
                        )

            newBufferContent =
                linesBefore
                    ++ newCurrentLine
                    :: linesAfter
                    |> String.join "\n"
        in
        ( { model | bufferContent = newBufferContent, cursor = newCursor }, [] )


handleNormalMode : String -> Model -> ( Model, List Msg )
handleNormalMode _ ({ cursor, bufferContent, keyStrokes } as model) =
    let
        (Cursor cursorLine cursorChar) =
            cursor
    in
    case keyStrokes of
        "d" :: "d" :: _ ->
            ( model, [ ExecuteAction (DeleteLine cursorLine) ] )

        "i" :: _ ->
            ( model, [ SetMode Insert ] )

        "I" :: _ ->
            ( model, [ SetMode Insert, SetCursor (cursorMoveLineBegin cursor) ] )

        "a" :: _ ->
            ( model, [ SetMode Insert, SetCursor (cursorMoveRight cursor) ] )

        "o" :: _ ->
            ( model
            , [ InsertNewLine (cursorLine + 1), SetMode Insert, SetCursor ((cursorMoveDown >> cursorMoveLineBegin) cursor) ]
            )

        "O" :: _ ->
            ( model
            , [ InsertNewLine cursorLine, SetMode Insert, SetCursor (cursorMoveLineBegin cursor) ]
            )

        "0" :: _ ->
            ( model
            , [ SetCursor (cursorMoveLineBegin cursor) ]
            )

        "h" :: _ ->
            if cursorChar > 0 then
                ( model, [ SetCursor (cursorMoveLeft (cursorInNormalModeBuffer bufferContent cursor)) ] )

            else
                ( model, [] )

        "j" :: _ ->
            if cursorLine < List.length (String.lines bufferContent) - 1 then
                ( model, [ SetCursor (cursorMoveDown cursor) ] )

            else
                ( model, [] )

        "k" :: _ ->
            if cursorLine > 0 then
                ( model, [ SetCursor (cursorMoveUp cursor) ] )

            else
                ( model, [] )

        "l" :: _ ->
            if cursorChar < (String.length (currentBufferLine cursor bufferContent) - 1) then
                ( model, [ SetCursor (cursorMoveRight cursor) ] )

            else
                ( model, [] )

        _ ->
            ( model, [] )


currentBufferLine : Cursor -> String -> String
currentBufferLine cursor bufferContent =
    let
        (Cursor cursorLine _) =
            cursor
    in
    String.lines bufferContent
        |> List.Extra.getAt cursorLine
        -- Note: Not sure if this is a good idea.
        -- Shouldn't be possible and maybe handling is overhead, but defaulting can lead to errors.
        |> Maybe.withDefault ""


splitBufferContent :
    Cursor
    -> String
    ->
        { linesBefore : List String
        , before : String
        , middle : String
        , after : String
        , linesAfter : List String
        }
splitBufferContent ((Cursor cursorLine cursorChar) as cursor) bufferContent =
    let
        lines =
            String.lines bufferContent

        linesBefore =
            List.take cursorLine lines

        currentLine =
            currentBufferLine cursor bufferContent

        linesAfter =
            List.drop (cursorLine + 1) lines

        ( beforeCurrentLine, middleCurrentLine, afterCurrentLine ) =
            splitLine cursorChar currentLine
    in
    { linesBefore = linesBefore
    , before = beforeCurrentLine
    , middle = middleCurrentLine
    , after = afterCurrentLine
    , linesAfter = linesAfter
    }


splitLine : Int -> String -> ( String, String, String )
splitLine cursorChar content =
    let
        charAt =
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


cursorMoveLineBegin : Cursor -> Cursor
cursorMoveLineBegin (Cursor line _) =
    Cursor line 0


cursorChar_ : Cursor -> Int
cursorChar_ (Cursor _ cursorChar) =
    cursorChar


cursorLine_ : Cursor -> Int
cursorLine_ (Cursor cursorLine _) =
    cursorLine


cursorInNormalModeLine : Int -> Cursor -> Cursor
cursorInNormalModeLine currentLineLength ((Cursor cursorLine cursorChar) as cursor) =
    if cursorChar >= currentLineLength then
        Cursor cursorLine (currentLineLength - 1)

    else
        cursor


cursorInNormalModeBuffer : String -> Cursor -> Cursor
cursorInNormalModeBuffer bufferContent cursor =
    let
        currentLineLength =
            String.length (currentBufferLine cursor bufferContent)
    in
    cursorInNormalModeLine currentLineLength cursor


fontSize : Int
fontSize =
    20
