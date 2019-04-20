module Main exposing (main, update)

import Browser
import Browser.Dom
import Buffer exposing (..)
import List.Extra
import Model
    exposing
        ( Buffer(..)
        , Cursor(..)
        , CursorDirection(..)
        , Mode(..)
        , Model
        , Msg(..)
        , Position(..)
        , WORD(..)
        , initModel
        )
import Platform.Sub as Sub
import Task
import Update.Extra exposing (sequence)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( initModel
    , Browser.Dom.focus "outermost" |> Task.attempt (always NoOp)
    )


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

        DeleteChar line char ->
            let
                { linesBefore, before, after, linesAfter } =
                    splitBufferContent (Cursor line char) model.buffer

                newLine =
                    before ++ after

                buffer =
                    linesBefore ++ newLine :: linesAfter |> String.join "\n"

                cursor =
                    cursorInNormalModeLine newLine model.cursor
            in
            ( { model | buffer = Buffer buffer, cursor = cursor }, Cmd.none )

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

                        FirstWORDinLine ->
                            model.buffer
                                |> currentBufferLine model.cursor
                                |> lineToWORDs line
                                |> List.Extra.getAt 0
                                |> Maybe.map cursorFromWORD
                                |> Maybe.withDefault model.cursor

                        NextWord ->
                            bufferToWords model.buffer
                                |> List.Extra.find (isWordAfterCursor model.cursor)
                                |> Maybe.map cursorFromWord
                                |> Maybe.withDefault (cursorMoveToEndOfLine model.buffer model.cursor)

                        NextWORD ->
                            bufferToWORDs model.buffer
                                |> List.Extra.find (isWORDafterCursor model.cursor)
                                |> Maybe.map cursorFromWORD
                                |> Maybe.withDefault (cursorMoveToEndOfLine model.buffer model.cursor)

                        NextWordEnd ->
                            bufferToWords model.buffer
                                |> wordsToWordEnds
                                |> List.Extra.find (isWordEndAfterCursor model.cursor)
                                |> Maybe.map cursorFromWordEnd
                                |> Maybe.withDefault (cursorMoveToEndOfLine model.buffer model.cursor)

                        PrevWORD ->
                            bufferToWORDs model.buffer
                                |> List.filter (isWORDbeforeCursor model.cursor)
                                |> List.Extra.last
                                |> Maybe.map cursorFromWORD
                                |> Maybe.withDefault (Cursor 0 0)

                        PrevWord ->
                            bufferToWords model.buffer
                                |> List.filter (isWordBeforeCursor model.cursor)
                                |> List.Extra.last
                                |> Maybe.map cursorFromWord
                                |> Maybe.withDefault (Cursor 0 0)
            in
            ( { model | cursor = cursor }, Cmd.none )


handleInsertMode : String -> Model -> ( Model, List Msg )
handleInsertMode keyDown ({ buffer, cursor } as model) =
    if keyDown == "Escape" then
        ( model, [ SetMode Normal ] )

    else if List.member keyDown ignoredKeysInInsertMode then
        ( model, [] )

    else
        let
            (Cursor _ cursorChar) =
                cursor

            { linesBefore, before, middle, after, linesAfter } =
                splitBufferContent cursor buffer

            ( newCurrentLine, msgs ) =
                case keyDown of
                    "Enter" ->
                        ( before ++ "\n" ++ middle ++ after
                        , [ MoveCursor Down, MoveCursor LineBegin ]
                        )

                    "Backspace" ->
                        ( String.dropRight 1 before ++ middle ++ after
                        , [ SetCursor <| ifThenElse (cursorChar > 0) (cursorMoveLeft cursor) cursor ]
                        )

                    "Delete" ->
                        ( before ++ after, [] )

                    "Tab" ->
                        ( before ++ "  " ++ middle ++ after, [ MoveCursor Right, MoveCursor Right ] )

                    _ ->
                        ( before ++ keyDown ++ middle ++ after, [ MoveCursor Right ] )

            buffer_ =
                linesBefore
                    ++ newCurrentLine
                    :: linesAfter
                    |> String.join "\n"
        in
        ( { model | buffer = Buffer buffer_ }, msgs )


handleNormalMode : String -> Model -> ( Model, List Msg )
handleNormalMode _ ({ cursor, keyStrokes } as model) =
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
                    [ PasteAfter, MoveCursor Down, MoveCursor FirstWORDinLine ]

                "P" :: _ ->
                    [ PasteBefore, MoveCursor FirstWORDinLine ]

                "o" :: _ ->
                    [ InsertNewLine (cursorLine + 1), SetMode Insert, MoveCursor Down, MoveCursor LineBegin ]

                "O" :: _ ->
                    [ InsertNewLine cursorLine, SetMode Insert, MoveCursor LineBegin ]

                "Delete" :: _ ->
                    [ DeleteChar cursorLine cursorChar ]

                "x" :: _ ->
                    [ DeleteChar cursorLine cursorChar ]

                "X" :: _ ->
                    ifThenElse
                        (cursorChar > 0)
                        [ DeleteChar cursorLine (cursorChar - 1), MoveCursor Left ]
                        []

                "0" :: _ ->
                    [ MoveCursor LineBegin ]

                "^" :: _ ->
                    [ MoveCursor FirstWORDinLine ]

                "w" :: _ ->
                    [ MoveCursor NextWord ]

                "W" :: _ ->
                    [ MoveCursor NextWORD ]

                "b" :: _ ->
                    [ MoveCursor PrevWord ]

                "B" :: _ ->
                    [ MoveCursor PrevWORD ]

                "e" :: _ ->
                    [ MoveCursor NextWordEnd ]

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



-- Contstants


ignoredKeysInInsertMode : List String
ignoredKeysInInsertMode =
    -- TODO: Extend list with media keys
    [ "Alt"
    , "AltGraph"
    , "ArrowDown"
    , "ArrowLeft"
    , "ArrowRight"
    , "ArrowUp"
    , "Control"
    , "F1"
    , "F10"
    , "F11"
    , "F12"
    , "F2"
    , "F2"
    , "F3"
    , "F4"
    , "F5"
    , "F6"
    , "F7"
    , "F8"
    , "F9"
    , "Insert"
    , "Meta"
    , "PageDown"
    , "PageUp"
    , "Shift"
    ]



-- helper


ifThenElse : Bool -> a -> a -> a
ifThenElse pred then_ else_ =
    if pred then
        then_

    else
        else_
