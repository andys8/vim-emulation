module Main exposing (main, update)

import Browser
import Browser.Events
import Buffer exposing (..)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Model exposing (Action(..), Buffer(..), Cursor(..), Mode(..), Model, Msg(..), initModel)
import Platform.Sub as Sub exposing (Sub)
import Update.Extra exposing (sequence)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> sub
        }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


sub : Sub Msg
sub =
    keyDecoder
        |> Browser.Events.onKeyDown
        |> Sub.map KeyDown


keyDecoder : Decoder String
keyDecoder =
    Decode.field "key" Decode.string


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
                    model.buffer
                        |> bufferToLines
                        |> List.Extra.splitAt lineNumber

                buffer =
                    linesBefore ++ "" :: linesAfter |> String.join "\n"
            in
            ( { model | buffer = Buffer buffer }, Cmd.none )

        ExecuteAction action ->
            (case action of
                DeleteLine lineNumber ->
                    let
                        buffer =
                            model.buffer
                                |> bufferToLines
                                |> List.Extra.removeAt lineNumber
                                |> String.join "\n"

                        lastLineWasDeleted =
                            cursorLine_ model.cursor >= List.length (String.lines buffer)

                        moveCursor =
                            if lastLineWasDeleted then
                                cursorMoveUp >> cursorMoveLineBegin

                            else
                                cursorMoveLineBegin
                    in
                    ( { model | buffer = Buffer buffer, cursor = moveCursor model.cursor }, Cmd.none )
            )
                |> Update.Extra.updateModel (\model_ -> { model_ | keyStrokes = [] })

        NoOp ->
            ( model, Cmd.none )


handleInsertMode : String -> Model -> ( Model, List Msg )
handleInsertMode keyDown ({ buffer, cursor } as model) =
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
                splitBufferContent cursor buffer

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

            buffer_ =
                linesBefore
                    ++ newCurrentLine
                    :: linesAfter
                    |> String.join "\n"
        in
        ( { model | buffer = Buffer buffer_, cursor = newCursor }, [] )


handleNormalMode : String -> Model -> ( Model, List Msg )
handleNormalMode _ ({ cursor, buffer, keyStrokes } as model) =
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

        "A" :: _ ->
            ( model, [ SetMode Insert, SetCursor (cursorMoveToEndOfLine buffer cursor) ] )

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
                ( model, [ SetCursor (cursorMoveLeft (cursorInNormalModeBuffer buffer cursor)) ] )

            else
                ( model, [] )

        "j" :: _ ->
            if cursorLine < List.length (bufferToLines buffer) - 1 then
                ( model, [ SetCursor (cursorMoveDown cursor) ] )

            else
                ( model, [] )

        "k" :: _ ->
            if cursorLine > 0 then
                ( model, [ SetCursor (cursorMoveUp cursor) ] )

            else
                ( model, [] )

        "l" :: _ ->
            if cursorChar < (String.length (currentBufferLine cursor buffer) - 1) then
                ( model, [ SetCursor (cursorMoveRight cursor) ] )

            else
                ( model, [] )

        _ ->
            ( model, [] )
