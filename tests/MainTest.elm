module MainTest exposing (all)

import Buffer exposing (..)
import Expect exposing (Expectation)
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
            , test "Enter insert mode" <|
                \_ ->
                    [ "i" ]
                        |> initWithKeySequence
                        |> expectMode Insert
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
            , describe "Delete line with dd"
                [ test "Delete two lines" <|
                    \_ ->
                        initModelWithBuffer "a\nb"
                            |> keySequence [ "d", "d", "d", "d" ]
                            |> expectBuffer ""
                , test "Delete first line of multiple lines" <|
                    \_ ->
                        initModelWithBuffer "a\nb"
                            |> keySequence [ "d", "d" ]
                            |> expectBuffer "b"
                , test "Delete and paste a line after" <|
                    \_ ->
                        initModelWithBuffer "abc"
                            |> keySequence [ "d", "d", "p" ]
                            |> Expect.all
                                [ expectBuffer "\nabc"
                                , expectCursorAt "a"
                                ]
                , test "Delete and paste a line before" <|
                    \_ ->
                        initModelWithBuffer "abc"
                            |> keySequence [ "d", "d", "P" ]
                            |> Expect.all
                                [ expectBuffer "abc\n"
                                , expectCursorAt "a"
                                ]
                ]
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
            , test "insert when at begin of line with i" <|
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
            , test "Delete with x" <|
                \_ ->
                    initModelWithBuffer "abcde"
                        |> keySequence [ "l", "x", "x" ]
                        |> Expect.all
                            [ expectBuffer "ade"
                            , expectCursorAt "d"
                            ]
            , test "Delete with X" <|
                \_ ->
                    initModelWithBuffer "abc"
                        |> keySequence [ "l", "X", "X" ]
                        |> Expect.all
                            [ expectBuffer "bc"
                            , expectCursorAt "b"
                            ]
            , test "S deletes line, goes in insert mode in first char" <|
                \_ ->
                    initModelWithBuffer "abcde"
                        |> keySequence [ "l", "S" ]
                        |> Expect.all
                            [ expectBuffer ""
                            , expectCursor (Cursor 0 0)
                            , expectMode Insert
                            ]
            , test "S with multiple lines" <|
                \_ ->
                    initModelWithBuffer "ab\ncd\nef"
                        |> keySequence [ "j", "S" ]
                        |> Expect.all
                            [ expectBuffer "ab\n\nef"
                            , expectCursor (Cursor 1 0)
                            , expectMode Insert
                            ]
            , test "Multiple lines in insert mode" <|
                \_ ->
                    [ "i", "a", "b", "Enter", "c", "Enter", "d", "Escape" ]
                        |> initWithKeySequence
                        |> Expect.all
                            [ expectBuffer "ab\nc\nd"
                            , expectCursor (Cursor 2 0)
                            , expectMode Normal
                            ]
            , test "2x Backspace over multiple lines in insert mode" <|
                \_ ->
                    [ "i", "a", "Enter", "b", "Backspace", "Backspace" ]
                        |> initWithKeySequence
                        |> expectBuffer "a"
            , test "3x Backspace over multiple lines in insert mode" <|
                \_ ->
                    [ "i", "a", "Enter", "b", "Backspace", "Backspace", "Backspace" ]
                        |> initWithKeySequence
                        |> expectBuffer ""
            , test "Delete in insert mode" <|
                \_ ->
                    initModelWithBuffer "ab"
                        |> keySequence [ "i", "Delete" ]
                        |> expectBuffer "b"
            , test "Delete line end in insert mode" <|
                \_ ->
                    initModelWithBuffer "\na"
                        |> keySequence [ "i", "Delete" ]
                        |> expectBuffer "a"
            , test "Delete characters over multiple lines in insert mode" <|
                \_ ->
                    initModelWithBuffer "ab\ncd"
                        |> keySequence [ "i", "Delete", "Delete", "Delete", "Delete" ]
                        |> expectBuffer "d"
            , describe "Delete in word"
                [ test "single word" <|
                    \_ ->
                        initModelWithBuffer "ab"
                            |> keySequence [ "d", "i", "w" ]
                            |> expectBuffer ""
                , test "two words" <|
                    \_ ->
                        initModelWithBuffer "ab cd"
                            |> keySequence [ "w", "d", "i", "w" ]
                            |> expectBuffer "ab "
                , test "delete and paste a single word" <|
                    \_ ->
                        initModelWithBuffer "ab"
                            |> keySequence [ "d", "i", "w", "p", "p" ]
                            |> Expect.all
                                [ expectBuffer "abab"
                                , expectCursor (Cursor 0 3)
                                ]
                , test "delete and paste before with P" <|
                    \_ ->
                        initModelWithBuffer "ab"
                            |> keySequence [ "d", "i", "w", "P", "P" ]
                            |> Expect.all
                                [ expectBuffer "aabb"
                                , expectCursor (Cursor 0 2)
                                ]
                , test "delete and paste a word at line begin" <|
                    \_ ->
                        initModelWithBuffer "ab cd"
                            |> keySequence [ "d", "i", "w", "p", "p" ]
                            |> Expect.all
                                [ expectBuffer " ababcd"
                                , expectCursor (Cursor 0 4)
                                ]
                , test "delete a word in middle of line" <|
                    \_ ->
                        initModelWithBuffer "ab cd ef"
                            |> keySequence [ "w", "d", "i", "w" ]
                            |> Expect.all
                                [ expectBuffer "ab  ef"
                                , expectCursor (Cursor 0 3)
                                ]
                , test "delete and paste a word in middle of line" <|
                    \_ ->
                        initModelWithBuffer "ab cd ef"
                            |> keySequence [ "w", "d", "i", "w", "p" ]
                            |> Expect.all
                                [ expectBuffer "ab  cdef"
                                , expectCursorAt "d"
                                ]
                , test "not triggering insert mode accidentally" <|
                    \_ ->
                        initWithKeySequence [ "d", "i" ]
                            |> expectMode Normal
                ]
            , describe "Yank in word"
                [ test "yank a word and paste it in front" <|
                    \_ ->
                        initModelWithBuffer "abc"
                            |> keySequence [ "l", "y", "i", "w", "P" ]
                            |> Expect.all
                                [ expectBuffer "abcabc"
                                , expectCursorAt "c"
                                ]
                , test "yank a word and paste in next line" <|
                    \_ ->
                        initModelWithBuffer "abc"
                            |> keySequence [ "y", "i", "w", "o", "Escape", "p" ]
                            |> expectBuffer "abc\nabc"
                , test "jumps to word begin" <|
                    \_ ->
                        initModelWithBuffer "abc"
                            |> keySequence [ "l", "y", "i", "w" ]
                            |> expectCursorAt "a"
                , test "not triggering insert mode accidentally" <|
                    \_ ->
                        initWithKeySequence [ "y", "i" ]
                            |> expectMode Normal
                ]
            , describe "Change in word"
                [ test "changes to Insert mode" <|
                    \_ ->
                        initWithKeySequence [ "c", "i", "w" ]
                            |> expectMode Insert
                , test "change a word in the middle" <|
                    \_ ->
                        initModelWithBuffer "ab cd ef"
                            |> keySequence [ "e", "e", "c", "i", "w", "x", "y", "z", "Escape" ]
                            |> Expect.all
                                [ expectBuffer "ab xyz ef"
                                , expectMode Normal
                                , expectCursorAt "z"
                                ]
                , test "change also yanks the word" <|
                    \_ ->
                        initModelWithBuffer "abc"
                            |> keySequence [ "c", "i", "w", "d", "e", "Escape", "p" ]
                            |> Expect.all
                                [ expectBuffer "deabc"
                                , expectCursorAt "c"
                                ]
                , test "not triggering insert mode accidentally" <|
                    \_ ->
                        initWithKeySequence [ "c", "i" ]
                            |> expectMode Normal
                ]
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
            splitBufferContent (cursorToPosition cursor) model.buffer
    in
    Expect.equal char middle
