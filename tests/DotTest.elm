module DotTest exposing (all)

import Buffer exposing (..)
import Expect
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
        , skip <|
            test "change can be repeated with dot" <|
                \_ ->
                    initModelWithBuffer "abc def"
                        |> keySequence [ "c", "i", "w", "a", "Escape", "w", "." ]
                        |> Expect.all
                            [ expectBuffer "a a"
                            , expectMode Normal
                            ]
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
            ]
        ]
