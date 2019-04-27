module MainTest exposing (all)

import Buffer exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import List
import Main exposing (update)
import Model exposing (Buffer(..), Cursor(..), Mode(..), Model, Msg(..), initModel)
import Test exposing (..)
import Update.Extra exposing (sequence)


all : Test
all =
    describe "Main"
        [ describe "update"
            [ test "Start with empty Buffer" <|
                \_ ->
                    update NoOp initModel
                        |> expectBuffer ""
            , test "Enter and leave insert mode" <|
                \_ ->
                    [ "i", "Escape" ]
                        |> initWithKeySequence
                        |> expectMode Normal
            , test "Type a word" <|
                \_ ->
                    [ "i", "a", "s", "d", "f", "Escape" ]
                        |> initWithKeySequence
                        |> expectBuffer "asdf"
            , test "Duplicate a line" <|
                \_ ->
                    [ "i", "a", "b", "Escape", "y", "y", "p" ]
                        |> initWithKeySequence
                        |> expectBuffer "ab\nab"
            , test "Insert a new line with o" <|
                \_ ->
                    [ "o", "a", "Escape", "o", "b" ]
                        |> initWithKeySequence
                        |> expectBuffer "\na\nb"
            , test "Delete lines with dd" <|
                \_ ->
                    initModelWithBuffer "a\nb"
                        |> keySequence [ "d", "d", "d", "d" ]
                        |> expectBuffer ""
            , test "Delete first line of multiple lines with dd" <|
                \_ ->
                    initModelWithBuffer "a\nb"
                        |> keySequence [ "d", "d" ]
                        |> expectBuffer "b"
            , test "Yank line and paste twice" <|
                \_ ->
                    initModelWithBuffer "ab"
                        |> keySequence [ "y", "y", "p", "p" ]
                        |> Expect.all
                            [ expectBuffer "ab\nab\nab"
                            , expectCursor (Cursor 2 0)
                            ]
            , test "word movement with 2x w" <|
                \_ ->
                    initModelWithBuffer "(asdf) (asdf)"
                        |> keySequence [ "w", "w" ]
                        |> expectCursorAt ")"
            , test "word movement with 3x w" <|
                \_ ->
                    initModelWithBuffer "(asdf) (asdf)"
                        |> keySequence [ "w", "w", "w" ]
                        |> expectCursorAt "("
            , test "word movement with W" <|
                \_ ->
                    initModelWithBuffer "(asdf) (asdf)"
                        |> keySequence [ "W" ]
                        |> expectCursorAt "("
            , test "word movement with w, W, b, e" <|
                \_ ->
                    initModelWithBuffer "(asdf) (asdf)"
                        |> keySequence [ "w", "W", "b", "b", "e" ]
                        |> expectCursorAt "f"
            , test "e movement across lines skips empty lines" <|
                \_ ->
                    initModelWithBuffer "a\nb\n\nc"
                        |> keySequence [ "e", "e" ]
                        |> expectCursorAt "c"
            , test "w movement across lines is in empty line" <|
                \_ ->
                    initModelWithBuffer "a\nb\n\nc"
                        |> keySequence [ "w", "w" ]
                        |> expectCursorAt ""
            , test "W movement across lines is in empty line" <|
                \_ ->
                    initModelWithBuffer "a\nb\n\nc"
                        |> keySequence [ "W", "W" ]
                        |> expectCursorAt ""
            , test "append to line with A" <|
                \_ ->
                    initModelWithBuffer "ab\ncd"
                        |> keySequence [ "j", "A", "x" ]
                        |> expectBuffer "ab\ncdx"
            , test "append to line with a" <|
                \_ ->
                    initModelWithBuffer "ab\ncd"
                        |> keySequence [ "j", "a", "x" ]
                        |> expectBuffer "ab\ncxd"
            , test "insert to begin of line with i" <|
                \_ ->
                    initModelWithBuffer "ab"
                        |> keySequence [ "i", "x" ]
                        |> expectBuffer "xab"
            , test "insert to begin of line with I" <|
                \_ ->
                    initModelWithBuffer "ab"
                        |> keySequence [ "l", "I", "x" ]
                        |> expectBuffer "xab"
            , test "go to last character with G $" <|
                \_ ->
                    initModelWithBuffer "ab\ncd\nef"
                        |> keySequence [ "G", "$" ]
                        |> expectCursorAt "f"
            , test "G will be first character in last line" <|
                \_ ->
                    initModelWithBuffer "ab\ncd\nef"
                        |> keySequence [ "$", "G" ]
                        |> expectCursorAt "e"
            , test "gg will go to first line" <|
                \_ ->
                    initModelWithBuffer "ab\ncd\nef"
                        |> keySequence [ "j", "g", "g" ]
                        |> expectCursorAt "a"
            , test "gg will go to first line from last line" <|
                \_ ->
                    initModelWithBuffer "ab\ncd\nef"
                        |> keySequence [ "G", "g", "g" ]
                        |> expectCursorAt "a"
            , test "Cursor will be at end of line when moving along shorter lines" <|
                \_ ->
                    initModelWithBuffer "abcde\nfg\nhij"
                        |> keySequence [ "j", "j", "l", "l", "k", "k" ]
                        |> expectCursorAt "c"
            , test "Cursor will be at last available char when coming from longer line" <|
                \_ ->
                    initModelWithBuffer "abcde\nfg\nhij"
                        |> keySequence [ "$", "j", "j" ]
                        |> expectCursorAt "j"
            , test "Cursor will be at last available char, but h (left) is working" <|
                \_ ->
                    initModelWithBuffer "abcde\nfg\nhij"
                        |> keySequence [ "$", "j", "j", "h" ]
                        |> expectCursorAt "i"
            ]
        ]



-- helper


initModelWithBuffer : String -> ( Model, Cmd Msg )
initModelWithBuffer bufferContent =
    ( { initModel | buffer = Buffer bufferContent }, Cmd.none )


initWithKeySequence : List String -> ( Model, Cmd Msg )
initWithKeySequence keys =
    ( initModel, Cmd.none )
        |> keySequence keys


keySequence : List String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
keySequence keys =
    sequence update (List.map KeyDown keys)


expectBuffer : String -> ( Model, Cmd Msg ) -> Expectation
expectBuffer bufferContent =
    Tuple.first >> .buffer >> Expect.equal (Buffer bufferContent)


expectMode : Mode -> ( Model, Cmd Msg ) -> Expectation
expectMode mode =
    Tuple.first >> .mode >> Expect.equal mode


expectCursor : Cursor -> ( Model, Cmd Msg ) -> Expectation
expectCursor cursor =
    Tuple.first >> .cursor >> Expect.equal cursor


expectCursorAt : String -> ( Model, Cmd Msg ) -> Expectation
expectCursorAt char ( model, _ ) =
    let
        cursor =
            cursorInMode model.mode model.buffer model.cursor

        { middle } =
            splitBufferContent cursor model.buffer
    in
    Expect.equal char middle
