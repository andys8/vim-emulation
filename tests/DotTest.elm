module DotTest exposing (all)

import Buffer exposing (..)
import Model exposing (Buffer(..), Cursor(..), Mode(..), Msg(..))
import Test exposing (..)
import TestUtil exposing (..)


all : Test
all =
    describe "Dot (.) will repeat commands"
        [ test "Delete line" <|
            \_ ->
                initModelWithBuffer "a\nb\nc"
                    |> keySequence [ "d", "d", "." ]
                    |> expectBuffer "c"
        , test "Paste line" <|
            \_ ->
                initModelWithBuffer "a"
                    |> keySequence [ "y", "y", "p", "." ]
                    |> expectBuffer "a\na\na"
        , test "Dot repeats paste, but content can change" <|
            \_ ->
                initModelWithBuffer "a\nb"
                    |> keySequence [ "y", "y", "p", "j", "y", "y", "." ]
                    |> expectBuffer "a\na\nb\nb"
        , test "change in word can be repeated with dot" <|
            \_ ->
                initModelWithBuffer "abc def"
                    |> keySequence [ "c", "i", "w", "a", "Escape", "w", "." ]
                    |> expectBuffer "a a"
        , test "repeat new line with content" <|
            \_ ->
                initWithKeySequence [ "o", "a", "Escape", "." ]
                    |> expectBuffer "\na\na"
        , test "insert mode with new line" <|
            \_ ->
                initWithKeySequence [ "i", "a", "Enter", "Escape", "." ]
                    |> expectBuffer "a\na\n"
        , test "change in word can be repeated on empty lines" <|
            \_ ->
                initModelWithBuffer "ab\n\ncd"
                    |> keySequence [ "c", "i", "w", "x", "Escape", "j", ".", "j", "." ]
                    |> expectBuffer "x\nx\nx"
        , describe "Verify key sequences lead to the same result"
            [ testEqualKeySequence "Has no effect with movements only"
                []
                [ "." ]
            , testEqualKeySequence "Delete with x"
                [ "x", "x" ]
                [ "x", "." ]
            , testEqualKeySequence "Delete with X"
                [ "X", "X" ]
                [ "X", "." ]
            , testEqualKeySequence "Delete with x and minor movement"
                [ "x", "j", "k", "x" ]
                [ "x", "j", "k", "." ]
            , testEqualKeySequence "Pasting line"
                [ "y", "y", "p", "p" ]
                [ "y", "y", "p", "." ]
            , testEqualKeySequence "Pasting line somewhere"
                [ "y", "y", "p", "j", "p" ]
                [ "y", "y", "p", "j", "." ]
            , testEqualKeySequence "3x delete in word"
                [ "d", "i", "w", "d", "i", "w", "d", "i", "w" ]
                [ "d", "i", "w", ".", "." ]
            , testEqualKeySequence "3x delete in word and next word"
                [ "d", "i", "w", "w", "d", "i", "w", "w", "d", "i", "w" ]
                [ "d", "i", "w", "w", ".", "w", "." ]
            , testEqualKeySequence "left shift"
                [ "<", "<", "<", "<" ]
                [ "<", "<", "." ]
            , testEqualKeySequence "right shift"
                [ ">", ">", ">", ">" ]
                [ ">", ">", "." ]
            , testEqualKeySequence "delete line"
                [ "d", "d", "d", "d" ]
                [ "d", "d", "." ]
            , testEqualKeySequence "change whole line and replace content"
                [ "c", "c", "a", "Escape", "j", "c", "c", "a", "Escape" ]
                [ "c", "c", "a", "Escape", "j", "." ]
            , testEqualKeySequence "insert twice"
                [ "i", "x", "Escape", "i", "x", "Escape" ]
                [ "i", "x", "Escape", "." ]
            , testEqualKeySequence "append char to end of line twice"
                [ "A", "x", "Escape", "j", "A", "x", "Escape" ]
                [ "A", "x", "Escape", "j", "." ]
            , testEqualKeySequence "append space with word to end of line twice"
                [ "$", "i", " ", "a", "b", "Escape", "i", " ", "a", "b", "Escape" ]
                [ "$", "i", " ", "a", "b", "Escape", "." ]
            ]
        ]
