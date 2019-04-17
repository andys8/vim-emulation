module Main exposing (main, update)

import Browser
import Browser.Events
import Buffer exposing (..)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Model exposing (Buffer(..), Cursor(..), CursorDirection(..), Mode(..), Model, Msg(..), initModel)
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
        NoOp ->
            ( model, Cmd.none )

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

        ActionExecuted ->
            ( { model | keyStrokes = [] }, Cmd.none )

        YankLine lineNumber ->
            let
                line =
                    model.buffer
                        |> bufferToLines
                        |> List.Extra.getAt lineNumber
                        |> Maybe.withDefault ""
            in
            ( { model | register = line }, Cmd.none )

        PasteBefore ->
            -- Note: This is implemented for single lines only
            let
                ( beforeLines, afterLines ) =
                    List.Extra.splitAt (cursorLine_ model.cursor) (bufferToLines model.buffer)

                buffer =
                    beforeLines
                        ++ model.register
                        :: afterLines
                        |> String.join "\n"
                        |> Buffer
            in
            ( { model | buffer = buffer }, Cmd.none )

        PasteAfter ->
            -- Note: This is implemented for single lines only
            let
                ( beforeLines, afterLines ) =
                    List.Extra.splitAt (cursorLine_ model.cursor + 1) (bufferToLines model.buffer)

                buffer =
                    beforeLines
                        ++ model.register
                        :: afterLines
                        |> String.join "\n"
                        |> Buffer
            in
            ( { model | buffer = buffer }, Cmd.none )

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

        MoveCursor direction ->
            let
                (Cursor line char) =
                    model.cursor

                cursor =
                    case direction of
                        Up ->
                            ifThenElse
                                (line > 0)
                                (Cursor (line - 1) char)
                                model.cursor

                        Down ->
                            ifThenElse
                                (line < List.length (bufferToLines model.buffer) - 1)
                                (Cursor (line + 1) char)
                                model.cursor

                        Right ->
                            let
                                lastChar =
                                    lastCharIndexInLine model.cursor model.buffer
                            in
                            ifThenElse
                                (char < lastChar || (model.mode == Insert && char <= lastChar))
                                (Cursor line (char + 1))
                                model.cursor

                        Left ->
                            ifThenElse (char > 0)
                                (cursorMoveLeft (cursorInNormalModeBuffer model.buffer model.cursor))
                                model.cursor

                        LineBegin ->
                            Cursor line 0

                        LineEnd ->
                            let
                                lastChar =
                                    lastCharIndexInLine model.cursor model.buffer
                            in
                            Cursor line (ifThenElse (model.mode == Insert) (lastChar + 1) lastChar)

                        FirstNonBlankChar ->
                            -- TODO: Find first non blank
                            Cursor line 0
            in
            ( { model | cursor = cursor }, Cmd.none )


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

                    "Tab" ->
                        ( before ++ "\t" ++ middle ++ after
                        , cursorMoveRight cursor
                        )

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

        msgs =
            case keyStrokes of
                "d" :: "d" :: _ ->
                    [ YankLine cursorLine, DeleteLine cursorLine, ActionExecuted ]

                "y" :: "y" :: _ ->
                    [ YankLine cursorLine, ActionExecuted ]

                "i" :: _ ->
                    [ SetMode Insert ]

                "I" :: _ ->
                    [ SetMode Insert, MoveCursor LineBegin ]

                "a" :: _ ->
                    [ SetMode Insert, MoveCursor Right ]

                "A" :: _ ->
                    [ SetMode Insert, MoveCursor LineEnd ]

                "p" :: _ ->
                    [ PasteAfter, MoveCursor Down, MoveCursor FirstNonBlankChar ]

                "P" :: _ ->
                    [ PasteBefore ]

                "o" :: _ ->
                    [ InsertNewLine (cursorLine + 1), SetMode Insert, MoveCursor Down, MoveCursor LineBegin ]

                "O" :: _ ->
                    [ InsertNewLine cursorLine, SetMode Insert, MoveCursor LineBegin ]

                "0" :: _ ->
                    [ MoveCursor LineBegin ]

                "$" :: _ ->
                    [ MoveCursor LineEnd ]

                "h" :: _ ->
                    [ MoveCursor Left ]

                "j" :: _ ->
                    [ MoveCursor Down ]

                "k" :: _ ->
                    [ MoveCursor Up ]

                "l" :: _ ->
                    [ MoveCursor Right ]

                _ ->
                    []
    in
    ( model, msgs )



-- helper


ifThenElse : Bool -> a -> a -> a
ifThenElse pred then_ else_ =
    if pred then
        then_

    else
        else_
