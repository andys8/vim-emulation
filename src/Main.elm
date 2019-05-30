module Main exposing (main, update)

import Action exposing (Action(..), Change(..), Motion(..), isChangeAction)
import Browser
import Browser.Dom
import Buffer exposing (..)
import List.Extra
import Model
    exposing
        ( Buffer(..)
        , Command(..)
        , Cursor(..)
        , CursorDirection(..)
        , Mode(..)
        , Model
        , Msg(..)
        , Position(..)
        , Register(..)
        , TextObject(..)
        , WORD(..)
        , Word(..)
        , initModel
        )
import Platform.Sub as Sub
import Task
import Update.Extra exposing (sequence)
import View exposing (viewDocument)


main : Program () Model Msg
main =
    Browser.document
        { view = viewDocument
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
            in
            model
                |> handleKey key
                |> applyMsgs

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

        ActionExecuted action ->
            ( { model | keyStrokes = [], actions = action :: model.actions }
            , Cmd.none
            )

        RepeatLastChangeAction ->
            let
                -- TODO: E.g. 'ciw' has to include the actual change in insert mode
                -- TODO: Repeat changes done in insert mode
                isLastChangeAction action =
                    isChangeAction action && action /= ActionChange Action_Dot

                msgs =
                    model.actions
                        |> List.Extra.find isLastChangeAction
                        |> Maybe.map (executeAction model.cursor)
                        |> Maybe.withDefault []
            in
            ( model, Cmd.none )
                |> sequence update msgs

        YankLine lineNumber ->
            let
                line =
                    model.buffer
                        |> bufferToLines
                        |> List.Extra.getAt lineNumber
                        |> Maybe.withDefault ""
            in
            ( { model | register = RegisterLine line }, Cmd.none )

        PasteBefore ->
            case model.register of
                RegisterLine register ->
                    let
                        ( beforeLines, afterLines ) =
                            List.Extra.splitAt (cursorLine_ model.cursor) (bufferToLines model.buffer)

                        buffer =
                            beforeLines ++ register :: afterLines |> String.join "\n"
                    in
                    ( { model | buffer = Buffer buffer }, Cmd.none )
                        |> sequence update [ MoveCursor FirstWORDinLine ]

                RegisterString register ->
                    let
                        { before, middle, after } =
                            splitBufferContent (cursorToPosition model.cursor) model.buffer

                        buffer =
                            before ++ register ++ middle ++ after
                    in
                    ( { model | buffer = Buffer buffer }, Cmd.none )
                        |> sequence update [ MoveCursor <| Right <| (String.length register - 1) ]

        PasteAfter ->
            case model.register of
                RegisterLine register ->
                    let
                        ( beforeLines, afterLines ) =
                            List.Extra.splitAt (cursorLine_ model.cursor + 1) (bufferToLines model.buffer)

                        buffer =
                            beforeLines ++ register :: afterLines |> String.join "\n"
                    in
                    ( { model | buffer = Buffer buffer }, Cmd.none )
                        |> sequence update [ MoveCursor Down, MoveCursor FirstWORDinLine ]

                RegisterString register ->
                    let
                        { before, middle, after } =
                            splitBufferContent (cursorToPosition model.cursor) model.buffer

                        buffer =
                            before ++ middle ++ register ++ after
                    in
                    ( { model | buffer = Buffer buffer }, Cmd.none )
                        |> sequence update [ MoveCursor <| Right <| String.length register ]

        ClearLine lineNumber ->
            let
                buffer =
                    model.buffer
                        |> bufferToLines
                        |> List.Extra.setAt lineNumber ""
                        |> String.join "\n"
            in
            ( { model | buffer = Buffer buffer, cursor = cursorMoveLineBegin model.cursor }, Cmd.none )

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
                { before, after } =
                    splitBufferContent (Position line char) model.buffer

                buffer =
                    Buffer (before ++ after)

                cursor =
                    cursorInMode model.mode buffer model.cursor
            in
            ( { model | buffer = buffer, cursor = cursor }, Cmd.none )

        ApplyCommandOnTextObject command textObject ->
            case textObject of
                InWord ->
                    let
                        -- TODO: Deleting spaces between words in not implemented
                        (Word position wordContent) =
                            model.buffer
                                |> bufferToWords
                                |> List.filter (wordToPosition WordBegin >> isPositionBeforeOrEqualToCursor model.cursor)
                                |> List.Extra.last
                                |> Maybe.withDefault (Word (Position 0 0) "")

                        { before, middle, after } =
                            splitBufferContent position model.buffer

                        buffer =
                            ifThenElse
                                (command == YankCommand)
                                model.buffer
                                (Buffer <| before ++ String.dropLeft (String.length wordContent) (middle ++ after))

                        mode =
                            ifThenElse
                                (command == ChangeCommand)
                                Insert
                                model.mode
                    in
                    ( { model
                        | cursor = cursorFromPosition position
                        , register = RegisterString wordContent
                        , buffer = buffer
                        , mode = mode
                      }
                    , Cmd.none
                    )

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

                        Right n ->
                            let
                                adaptToMode =
                                    case model.mode of
                                        Insert ->
                                            (+) 1

                                        _ ->
                                            identity

                                maxChar =
                                    lastCharIndexInLine model.cursor model.buffer
                                        |> adaptToMode
                            in
                            ifThenElse
                                ((char + n) < maxChar)
                                (Cursor line (char + n))
                                (Cursor line maxChar)

                        Left n ->
                            if char - n >= 0 then
                                let
                                    (Cursor line_ char_) =
                                        cursorInMode model.mode model.buffer model.cursor
                                in
                                Cursor line_ (char_ - n)

                            else
                                cursorMoveLineBegin model.cursor

                        LineBegin ->
                            Cursor line 0

                        LineEnd ->
                            let
                                lastChar =
                                    lastCharIndexInLine model.cursor model.buffer
                            in
                            Cursor line (ifThenElse (model.mode == Insert) (lastChar + 1) lastChar)

                        FirstLine ->
                            Cursor 0 char

                        LastLine ->
                            Cursor ((bufferToLines model.buffer |> List.length) - 1) char

                        FirstWORDinLine ->
                            model.buffer
                                |> currentBufferLine model.cursor
                                |> lineToWORDs line
                                |> List.Extra.getAt 0
                                |> Maybe.map (wORDToPosition WordBegin >> cursorFromPosition)
                                |> Maybe.withDefault model.cursor

                        NextWord ->
                            model.buffer
                                |> bufferToWords
                                |> List.map (wordToPosition WordBegin)
                                |> List.Extra.find (isPositionAfterCursor model.cursor)
                                |> Maybe.map cursorFromPosition
                                |> Maybe.withDefault (cursorMoveToEndOfLine model.buffer model.cursor)

                        NextWordEnd ->
                            model.buffer
                                |> bufferToWords
                                |> rejectEmptyWords
                                |> List.map (wordToPosition WordEnd)
                                |> List.Extra.find (isPositionAfterCursor model.cursor)
                                |> Maybe.map cursorFromPosition
                                |> Maybe.withDefault (cursorMoveToEndOfLine model.buffer model.cursor)

                        NextWORD ->
                            model.buffer
                                |> bufferToWORDs
                                |> List.map (wORDToPosition WordBegin)
                                |> List.Extra.find (isPositionAfterCursor model.cursor)
                                |> Maybe.map cursorFromPosition
                                |> Maybe.withDefault (cursorMoveToEndOfLine model.buffer model.cursor)

                        NextWORDEnd ->
                            model.buffer
                                |> bufferToWORDs
                                |> rejectEmptyWORDs
                                |> List.map (wORDToPosition WordEnd)
                                |> List.Extra.find (isPositionAfterCursor model.cursor)
                                |> Maybe.map cursorFromPosition
                                |> Maybe.withDefault (cursorMoveToEndOfLine model.buffer model.cursor)

                        PrevWord ->
                            model.buffer
                                |> bufferToWords
                                |> List.map (wordToPosition WordBegin)
                                |> List.filter (isPositionBeforeCursor model.cursor)
                                |> List.Extra.last
                                |> Maybe.map cursorFromPosition
                                |> Maybe.withDefault (Cursor 0 0)

                        PrevWORD ->
                            model.buffer
                                |> bufferToWORDs
                                |> List.map (wORDToPosition WordBegin)
                                |> List.filter (isPositionBeforeCursor model.cursor)
                                |> List.Extra.last
                                |> Maybe.map cursorFromPosition
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
            (Cursor cursorLine cursorChar) =
                cursor

            { before, middle, after } =
                splitBufferContent (cursorToPosition cursor) buffer

            ( buffer_, msgs ) =
                case keyDown of
                    "Enter" ->
                        ( before ++ "\n" ++ middle ++ after
                        , [ MoveCursor Down, MoveCursor LineBegin ]
                        )

                    "Backspace" ->
                        let
                            before_ =
                                String.dropRight 1 before

                            lastLineLength =
                                before_
                                    |> String.lines
                                    |> List.Extra.last
                                    |> Maybe.map String.length
                                    |> Maybe.withDefault 0

                            cursor_ =
                                if String.endsWith "\n" before then
                                    Cursor (cursorLine - 1) lastLineLength

                                else if cursorChar > 0 then
                                    cursorMoveLeft cursor

                                else
                                    cursor
                        in
                        ( before_ ++ middle ++ after
                        , [ SetCursor cursor_ ]
                        )

                    "Delete" ->
                        let
                            after_ =
                                if middle == "" && String.startsWith "\n" after then
                                    String.dropLeft 1 after

                                else
                                    after
                        in
                        ( before ++ after_, [] )

                    "Tab" ->
                        ( before ++ "  " ++ middle ++ after, [ MoveCursor (Right 2) ] )

                    _ ->
                        ( before ++ keyDown ++ middle ++ after, [ MoveCursor (Right 1) ] )
        in
        ( { model | buffer = Buffer buffer_ }, msgs )


handleNormalMode : String -> Model -> ( Model, List Msg )
handleNormalMode key model =
    let
        keyStrokes =
            key :: model.keyStrokes

        actionToMsgs action =
            executeAction model.cursor action ++ [ ActionExecuted action ]

        msgs =
            keyStrokes
                |> Action.fromKeyStrokes
                |> Maybe.map actionToMsgs
                |> Maybe.withDefault []
    in
    ( { model | keyStrokes = keyStrokes }, msgs )


executeAction : Cursor -> Action -> List Msg
executeAction (Cursor cursorLine cursorChar) action =
    case action of
        ActionChange Action_diw ->
            [ ApplyCommandOnTextObject DeleteCommand InWord ]

        ActionChange Action_ciw ->
            [ ApplyCommandOnTextObject ChangeCommand InWord ]

        ActionMotion Action_yiw ->
            [ ApplyCommandOnTextObject YankCommand InWord ]

        ActionChange Action_dd ->
            [ YankLine cursorLine, DeleteLine cursorLine ]

        ActionMotion Action_yy ->
            [ YankLine cursorLine ]

        ActionMotion Action_gg ->
            [ MoveCursor FirstLine, MoveCursor FirstWORDinLine ]

        ActionChange Action_i ->
            [ SetMode Insert ]

        ActionChange Action_I ->
            [ SetMode Insert, MoveCursor LineBegin ]

        ActionChange Action_Dot ->
            [ RepeatLastChangeAction ]

        ActionChange Action_S ->
            [ ClearLine cursorLine, SetMode Insert ]

        ActionChange Action_a ->
            [ SetMode Insert, MoveCursor (Right 1) ]

        ActionChange Action_A ->
            [ SetMode Insert, MoveCursor LineEnd ]

        ActionChange Action_p ->
            [ PasteAfter ]

        ActionChange Action_P ->
            [ PasteBefore ]

        ActionChange Action_o ->
            [ InsertNewLine (cursorLine + 1), SetMode Insert, MoveCursor Down, MoveCursor LineBegin ]

        ActionChange Action_O ->
            [ InsertNewLine cursorLine, SetMode Insert, MoveCursor LineBegin ]

        ActionChange Action_Delete ->
            [ DeleteChar cursorLine cursorChar ]

        ActionChange Action_x ->
            [ DeleteChar cursorLine cursorChar ]

        ActionChange Action_X ->
            if cursorChar > 0 then
                [ DeleteChar cursorLine (cursorChar - 1), MoveCursor (Left 1) ]

            else
                []

        ActionMotion Action_0 ->
            [ MoveCursor LineBegin ]

        ActionMotion Action_Graph ->
            [ MoveCursor FirstWORDinLine ]

        ActionMotion Action_G ->
            [ MoveCursor LastLine, MoveCursor FirstWORDinLine ]

        ActionMotion Action_w ->
            [ MoveCursor NextWord ]

        ActionMotion Action_W ->
            [ MoveCursor NextWORD ]

        ActionMotion Action_b ->
            [ MoveCursor PrevWord ]

        ActionMotion Action_B ->
            [ MoveCursor PrevWORD ]

        ActionMotion Action_e ->
            [ MoveCursor NextWordEnd ]

        ActionMotion Action_E ->
            [ MoveCursor NextWORDEnd ]

        ActionMotion Action_Dollar ->
            [ MoveCursor LineEnd ]

        ActionMotion Action_h ->
            [ MoveCursor (Left 1) ]

        ActionMotion Action_j ->
            [ MoveCursor Down ]

        ActionMotion Action_k ->
            [ MoveCursor Up ]

        ActionMotion Action_l ->
            [ MoveCursor (Right 1) ]



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


applyMsgs : ( Model, List Msg ) -> ( Model, Cmd Msg )
applyMsgs ( model, msgs ) =
    ( model, Cmd.none )
        |> sequence update msgs
