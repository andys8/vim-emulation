module Main exposing (main, update)

import Action exposing (Action(..), ActionChange(..), ActionInsert(..), ActionMove(..))
import Browser
import Browser.Dom
import Browser.Navigation as Navigation
import Buffer exposing (..)
import Config exposing (config)
import File.Download
import List.Extra
import Model
    exposing
        ( Buffer(..)
        , Command(..)
        , Cursor(..)
        , CursorDirection(..)
        , File(..)
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
                    case model.mode of
                        Insert ->
                            handleInsertMode

                        Normal ->
                            handleNormalMode

                        Command ->
                            handleCommandMode
            in
            model
                |> handleKey key
                |> applyMsgs

        SetMode mode ->
            let
                cursor =
                    if mode == Normal && cursorChar_ model.cursor > 0 then
                        cursorMoveLeft model.cursor

                    else
                        model.cursor

                insertModeKeyStrokes =
                    if mode == Insert then
                        []

                    else
                        model.insertModeKeyStrokes
            in
            ( { model | mode = mode, cursor = cursor, insertModeKeyStrokes = insertModeKeyStrokes }
            , Cmd.none
            )

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

        InsertText position text ->
            let
                { before, middle, after } =
                    splitBufferContent position model.buffer

                buffer =
                    before ++ text ++ middle ++ after
            in
            ( { model | buffer = Buffer buffer }, Cmd.none )

        DeleteTextPartial position text ->
            let
                deletePartialAcc search result =
                    if not (String.isEmpty search) && String.startsWith (String.left 1 search) result then
                        deletePartialAcc (String.dropLeft 1 search) (String.dropLeft 1 result)

                    else
                        result

                { before, middle, after } =
                    splitBufferContent position model.buffer

                buffer =
                    before ++ deletePartialAcc text (middle ++ after)
            in
            ( { model | buffer = Buffer buffer }, Cmd.none )

        ActionExecuted action ->
            ( { model
                | normalModeKeyStrokes = []
                , actions = action :: model.actions
              }
            , Cmd.none
            )

        RepeatLastChangeAction ->
            let
                toMsgs actions =
                    case actions of
                        [] ->
                            []

                        (ActionMoveType _) :: xs ->
                            toMsgs xs

                        (ActionChangeType Action_Dot) :: xs ->
                            toMsgs xs

                        ((ActionChangeType _) as action) :: _ ->
                            executeAction model.cursor action

                        ((ActionInsertType _) as action) :: _ ->
                            let
                                replayInsertMode =
                                    model.insertModeKeyStrokes
                                        |> List.reverse
                                        |> List.map KeyDown
                            in
                            executeAction model.cursor action
                                ++ replayInsertMode
                                ++ [ SetMode Normal ]
            in
            ( model, Cmd.none )
                |> sequence update (toMsgs model.actions)

        YankLine lineNumber ->
            let
                line =
                    model.buffer
                        |> bufferToLines
                        |> List.Extra.getAt lineNumber
                        |> Maybe.withDefault ""
            in
            ( { model | register = RegisterLine line }, Cmd.none )

        ExecuteCmd cmd ->
            ( model, cmd )

        SaveFile (File fileName) ->
            ( model
            , File.Download.string fileName "text/plain" (bufferToString model.buffer)
            )

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

        ClearBuffer ->
            ( { model | buffer = Buffer "", cursor = Cursor 0 0 }, Cmd.none )

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

        DeleteChar (Position line char) ->
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
                    in
                    ( { model
                        | cursor = cursorFromPosition position
                        , register = RegisterString wordContent
                        , buffer = buffer
                      }
                    , Cmd.none
                    )

        MoveCursor direction ->
            ( { model | cursor = handleMoveCursor model direction }, Cmd.none )


handleMoveCursor : Model -> CursorDirection -> Cursor
handleMoveCursor { cursor, buffer, mode } direction =
    let
        (Cursor line char) =
            cursor
    in
    case direction of
        Up ->
            ifThenElse
                (line > 0)
                (Cursor (line - 1) char)
                cursor

        Down ->
            ifThenElse
                (line < List.length (bufferToLines buffer) - 1)
                (Cursor (line + 1) char)
                cursor

        Right n ->
            let
                adaptToMode =
                    case mode of
                        Insert ->
                            (+) 1

                        _ ->
                            identity

                maxChar =
                    lastCharIndexInLine cursor buffer |> adaptToMode
            in
            ifThenElse
                ((char + n) < maxChar)
                (Cursor line (char + n))
                (Cursor line maxChar)

        Left n ->
            if char - n >= 0 then
                let
                    (Cursor line_ char_) =
                        cursorInMode mode buffer cursor
                in
                Cursor line_ (char_ - n)

            else
                cursorMoveLineBegin cursor

        LineBegin ->
            Cursor line 0

        LineEnd ->
            let
                lastChar =
                    lastCharIndexInLine cursor buffer
            in
            Cursor line (ifThenElse (mode == Insert) (lastChar + 1) lastChar)

        FirstLine ->
            Cursor 0 char

        LastLine ->
            Cursor (bufferToLinesCount buffer - 1) char

        ToLine lineNumber ->
            Cursor (clamp 0 (bufferToLinesCount buffer - 1) (lineNumber - 1)) char

        FirstWORDinLine ->
            buffer
                |> currentBufferLine cursor
                |> lineToWORDs line
                |> List.Extra.getAt 0
                |> Maybe.map (wORDToPosition WordBegin >> cursorFromPosition)
                |> Maybe.withDefault cursor

        NextWord ->
            buffer
                |> bufferToWords
                |> List.map (wordToPosition WordBegin)
                |> List.Extra.find (isPositionAfterCursor cursor)
                |> Maybe.map cursorFromPosition
                |> Maybe.withDefault (cursorMoveToEndOfLine buffer cursor)

        NextWordEnd ->
            buffer
                |> bufferToWords
                |> rejectEmptyWords
                |> List.map (wordToPosition WordEnd)
                |> List.Extra.find (isPositionAfterCursor cursor)
                |> Maybe.map cursorFromPosition
                |> Maybe.withDefault (cursorMoveToEndOfLine buffer cursor)

        NextWORD ->
            buffer
                |> bufferToWORDs
                |> List.map (wORDToPosition WordBegin)
                |> List.Extra.find (isPositionAfterCursor cursor)
                |> Maybe.map cursorFromPosition
                |> Maybe.withDefault (cursorMoveToEndOfLine buffer cursor)

        NextWORDEnd ->
            buffer
                |> bufferToWORDs
                |> rejectEmptyWORDs
                |> List.map (wORDToPosition WordEnd)
                |> List.Extra.find (isPositionAfterCursor cursor)
                |> Maybe.map cursorFromPosition
                |> Maybe.withDefault (cursorMoveToEndOfLine buffer cursor)

        PrevWord ->
            buffer
                |> bufferToWords
                |> List.map (wordToPosition WordBegin)
                |> List.filter (isPositionBeforeCursor cursor)
                |> List.Extra.last
                |> Maybe.map cursorFromPosition
                |> Maybe.withDefault (Cursor 0 0)

        PrevWORD ->
            buffer
                |> bufferToWORDs
                |> List.map (wORDToPosition WordBegin)
                |> List.filter (isPositionBeforeCursor cursor)
                |> List.Extra.last
                |> Maybe.map cursorFromPosition
                |> Maybe.withDefault (Cursor 0 0)


handleInsertMode : String -> Model -> ( Model, List Msg )
handleInsertMode keyDown ({ buffer, cursor } as model) =
    if keyDown == "Escape" then
        ( model, [ SetMode Normal ] )

    else if List.member keyDown ignoredTextInsertKeys then
        ( model, [] )

    else if keyDown == "Backspace" && cursor == Cursor 0 0 then
        -- Ignoring this edge casse early helps to handle different behavior
        -- with Delete and Backspace when being repeated
        -- Backspace "nothing" will not be replayed, but "Delete" will
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
                        ( before ++ String.repeat config.shiftWidth " " ++ middle ++ after
                        , [ MoveCursor (Right config.shiftWidth) ]
                        )

                    _ ->
                        ( before ++ keyDown ++ middle ++ after, [ MoveCursor (Right 1) ] )
        in
        ( { model
            | buffer = Buffer buffer_
            , insertModeKeyStrokes = keyDown :: model.insertModeKeyStrokes
          }
        , msgs
        )


handleNormalMode : String -> Model -> ( Model, List Msg )
handleNormalMode key model =
    let
        keyStrokes =
            key :: model.normalModeKeyStrokes

        actionToMsgs action =
            executeAction model.cursor action ++ [ ActionExecuted action ]

        msgs =
            keyStrokes
                |> Action.fromKeyStrokes
                |> Maybe.map actionToMsgs
                |> Maybe.withDefault []
    in
    ( { model | normalModeKeyStrokes = keyStrokes }, msgs )


executeAction : Cursor -> Action -> List Msg
executeAction (Cursor cursorLine cursorChar) action =
    case action of
        ActionInsertType action_ ->
            case action_ of
                Action_ciw ->
                    [ ApplyCommandOnTextObject ChangeCommand InWord, SetMode Insert ]

                Action_cc_or_S ->
                    [ ClearLine cursorLine, SetMode Insert ]

                Action_i ->
                    [ SetMode Insert ]

                Action_I ->
                    [ SetMode Insert, MoveCursor LineBegin ]

                Action_a ->
                    [ SetMode Insert, MoveCursor (Right 1) ]

                Action_A ->
                    [ SetMode Insert, MoveCursor LineEnd ]

                Action_o ->
                    [ InsertNewLine (cursorLine + 1), SetMode Insert, MoveCursor Down, MoveCursor LineBegin ]

                Action_O ->
                    [ InsertNewLine cursorLine, SetMode Insert, MoveCursor LineBegin ]

        ActionChangeType action_ ->
            case action_ of
                Action_diw ->
                    [ ApplyCommandOnTextObject DeleteCommand InWord ]

                Action_dd ->
                    [ YankLine cursorLine, DeleteLine cursorLine ]

                Action_Dot ->
                    [ RepeatLastChangeAction ]

                Action_RightShift ->
                    [ InsertText (Position cursorLine 0) shift, MoveCursor FirstWORDinLine ]

                Action_LeftShift ->
                    [ DeleteTextPartial (Position cursorLine 0) shift, MoveCursor FirstWORDinLine ]

                Action_p ->
                    [ PasteAfter ]

                Action_P ->
                    [ PasteBefore ]

                Action_Delete ->
                    [ DeleteChar (Position cursorLine cursorChar) ]

                Action_x ->
                    [ DeleteChar (Position cursorLine cursorChar) ]

                Action_X ->
                    if cursorChar > 0 then
                        [ DeleteChar (Position cursorLine (cursorChar - 1)), MoveCursor (Left 1) ]

                    else
                        []

        ActionMoveType action_ ->
            case action_ of
                Action_yiw ->
                    [ ApplyCommandOnTextObject YankCommand InWord ]

                Action_yy_or_Y ->
                    [ YankLine cursorLine ]

                Action_gg ->
                    [ MoveCursor FirstLine, MoveCursor FirstWORDinLine ]

                Action_0 ->
                    [ MoveCursor LineBegin ]

                Action_Graph ->
                    [ MoveCursor FirstWORDinLine ]

                Action_G ->
                    [ MoveCursor LastLine, MoveCursor FirstWORDinLine ]

                Action_w ->
                    [ MoveCursor NextWord ]

                Action_W ->
                    [ MoveCursor NextWORD ]

                Action_b ->
                    [ MoveCursor PrevWord ]

                Action_B ->
                    [ MoveCursor PrevWORD ]

                Action_e ->
                    [ MoveCursor NextWordEnd ]

                Action_E ->
                    [ MoveCursor NextWORDEnd ]

                Action_Dollar ->
                    [ MoveCursor LineEnd ]

                Action_h ->
                    [ MoveCursor (Left 1) ]

                Action_j ->
                    [ MoveCursor Down ]

                Action_k ->
                    [ MoveCursor Up ]

                Action_l ->
                    [ MoveCursor (Right 1) ]

                Action_Colon ->
                    [ SetMode Command ]


handleCommandMode : String -> Model -> ( Model, List Msg )
handleCommandMode key model =
    let
        ignoredKeys =
            "Tab" :: "Delete" :: ignoredTextInsertKeys
    in
    case key of
        "Enter" ->
            ( { model | commandLine = "" }
            , SetMode Normal :: handleCommandLineEntered model.commandLine
            )

        "Backspace" ->
            if String.isEmpty model.commandLine then
                ( model, [ SetMode Normal ] )

            else
                ( { model | commandLine = String.dropRight 1 model.commandLine }, [] )

        "Escape" ->
            ( { model | commandLine = "" }, [ SetMode Normal ] )

        key_ ->
            if List.member key_ ignoredKeys then
                ( model, [] )

            else
                ( { model | commandLine = model.commandLine ++ key_ }, [] )


handleCommandLineEntered : String -> List Msg
handleCommandLineEntered text =
    case String.toInt text of
        Just lineNumber ->
            if String.startsWith "+" text then
                -- TODO: Implement "MoveCursor (Down i)"
                []

            else if String.startsWith "-" text then
                -- TODO: Implement "MoveCursor (Up i)"
                []

            else
                [ MoveCursor (ToLine lineNumber), MoveCursor FirstWORDinLine ]

        Nothing ->
            let
                command =
                    -- Ignore "!" for now, but has to be implemented
                    String.replace "!" "" text

                argument =
                    command
                        |> String.split " "
                        |> List.drop 1
                        |> String.join " "

                isCommand str =
                    command == str || String.startsWith (str ++ " ") command
            in
            if List.member command [ "q", "qa", "quit" ] then
                [ ExecuteCmd quitVim ]

            else if isCommand "x" || isCommand "wq" then
                [ SaveFile (File argument), ExecuteCmd quitVim ]

            else if isCommand "w" then
                [ SaveFile (File argument) ]

            else if List.member command [ "bd", "bdelete" ] then
                [ ClearBuffer ]

            else
                []


quitVim : Cmd a
quitVim =
    Navigation.load "https://github.com/andys8/vim-emulation"



-- Contstants


ignoredTextInsertKeys : List String
ignoredTextInsertKeys =
    -- TODO: Extend list with media keys
    [ "Alt"
    , "AltGraph"
    , "ArrowDown"
    , "ArrowLeft"
    , "ArrowRight"
    , "ArrowUp"
    , "Control"
    , "Home"
    , "End"
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


shift : String
shift =
    String.repeat config.shiftWidth " "
