module MainTest exposing (all)

import Buffer exposing (..)
import Config exposing (config)
import Expect
import Fuzz
import Fuzzers
import Main exposing (update)
import Model exposing (Buffer(..), Cursor(..), Mode(..), Msg(..), initModel)
import Test exposing (..)
import TestUtil exposing (..)


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
            , describe "Yank line"
                [ test "Yank line and paste twice" <|
                    \_ ->
                        initModelWithBuffer "ab"
                            |> keySequence [ "y", "y", "p", "p" ]
                            |> Expect.all
                                [ expectBuffer "ab\nab\nab"
                                , expectCursor (Cursor 2 0)
                                ]
                , fuzz Fuzzers.buffer "Y and yy are synonyms" <|
                    \buffer ->
                        expectEqualModel buffer
                            (keySequence [ "y", "y", "p", "p" ])
                            (keySequence [ "Y", "p", "p" ])
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
            , test "S deletes line, goes in insert mode to first char" <|
                \_ ->
                    initModelWithBuffer "abcde\nb"
                        |> keySequence [ "l", "S" ]
                        |> Expect.all
                            [ expectBuffer "\nb"
                            , expectCursor (Cursor 0 0)
                            , expectMode Insert
                            ]
            , fuzz2 Fuzzers.buffer Fuzzers.movements "cc and S are synonyms" <|
                \buffer movements ->
                    expectEqualModel buffer
                        (keySequence movements >> keySequence [ "c", "c" ])
                        (keySequence movements >> keySequence [ "S" ])
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
            , describe "Insert Mode"
                [ test "2x Backspace over multiple lines" <|
                    \_ ->
                        [ "i", "a", "Enter", "b", "Backspace", "Backspace" ]
                            |> initWithKeySequence
                            |> expectBuffer "a"
                , test "3x Backspace over multiple lines" <|
                    \_ ->
                        [ "i", "a", "Enter", "b", "Backspace", "Backspace", "Backspace" ]
                            |> initWithKeySequence
                            |> expectBuffer ""
                , test "Delete" <|
                    \_ ->
                        initModelWithBuffer "ab"
                            |> keySequence [ "i", "Delete" ]
                            |> expectBuffer "b"
                , test "Delete line end" <|
                    \_ ->
                        initModelWithBuffer "\na"
                            |> keySequence [ "i", "Delete" ]
                            |> expectBuffer "a"
                , test "Delete characters over multiple lines" <|
                    \_ ->
                        initModelWithBuffer "ab\ncd"
                            |> keySequence [ "i", "Delete", "Delete", "Delete", "Delete" ]
                            |> expectBuffer "d"
                , test "Insert 1 Tab" <|
                    \_ ->
                        [ "i", "Tab", "a" ]
                            |> initWithKeySequence
                            |> expectBuffer "    a"
                , test "Insert 2 Tabs" <|
                    \_ ->
                        [ "i", "Tab", "Tab", "a" ]
                            |> initWithKeySequence
                            |> expectBuffer "        a"
                ]
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
                , test "change a word replaces word" <|
                    \_ ->
                        initModelWithBuffer "abc"
                            |> keySequence [ "c", "i", "w" ]
                            |> Expect.all
                                [ expectBuffer ""
                                , expectMode Insert
                                ]
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
            , describe "shift"
                [ test "Right shift" <|
                    \_ ->
                        initModelWithBuffer "a\nb"
                            |> keySequence [ ">", ">" ]
                            |> Expect.all
                                [ expectBuffer (String.repeat config.shiftWidth " " ++ "a\nb")
                                , expectCursorAt "a"
                                ]
                , fuzz Fuzzers.spaces "Right shift will set cursor to first letter" <|
                    \spaces ->
                        initModelWithBuffer (spaces ++ "a")
                            |> keySequence [ ">", ">" ]
                            |> expectCursorAt "a"
                , fuzz Fuzzers.buffer "Right shift increases width by config.shiftWidth" <|
                    \buffer ->
                        initModelWithBuffer buffer
                            |> keySequence [ ">", ">" ]
                            |> expectBufferTo
                                (String.length >> Expect.equal (String.length buffer + config.shiftWidth))
                , test "Right shift inserts text at beginning of line" <|
                    \_ ->
                        initModelWithBuffer "a b"
                            |> keySequence [ "l", "l", ">", ">" ]
                            |> Expect.all
                                [ expectBuffer (shift ++ "a b")
                                , expectCursorAt "a"
                                ]
                , fuzz2 Fuzzers.buffer Fuzzers.movements "Right and left shift combines won't change buffer" <|
                    \buffer movements ->
                        initModelWithBuffer buffer
                            |> keySequence movements
                            |> keySequence [ ">", ">" ]
                            |> keySequence [ "<", "<" ]
                            |> expectBuffer buffer
                , fuzz Fuzzers.movements "Left shift sets cursor to first word" <|
                    \movements ->
                        initModelWithBuffer "     a   "
                            |> keySequence movements
                            |> keySequence [ "<", "<" ]
                            |> expectCursorAt "a"
                , test "Left shift deletes also fewer spaces than shiftWidth" <|
                    \_ ->
                        initModelWithBuffer " a"
                            |> keySequence [ "<", "<" ]
                            |> expectBuffer "a"
                , fuzz2 Fuzzers.buffer Fuzzers.spaces "Left shift deletes shiftWidth spaces at beginning" <|
                    \buffer spaces ->
                        initModelWithBuffer (shift ++ spaces ++ buffer)
                            |> keySequence [ "<", "<" ]
                            |> expectBuffer (spaces ++ buffer)
                ]
            , describe "Commandline mode"
                [ test ": Enters commandline" <|
                    \_ ->
                        [ ":" ]
                            |> initWithKeySequence
                            |> expectMode Command
                , test "Escape" <|
                    \_ ->
                        [ ":", "a", "b", "Escape" ]
                            |> initWithKeySequence
                            |> Expect.all [ expectMode Normal, expectCommandLine "" ]
                , test "Write command" <|
                    \_ ->
                        [ ":", "q", "!" ]
                            |> initWithKeySequence
                            |> expectCommandLine "q!"
                , test "Backspaces deletes chars" <|
                    \_ ->
                        [ ":", "a", "b", "Backspace" ]
                            |> initWithKeySequence
                            |> expectCommandLine "a"
                , test "Backspaces can quit, if command line empty" <|
                    \_ ->
                        [ ":", "a", "Backspace", "Backspace" ]
                            |> initWithKeySequence
                            |> Expect.all
                                [ expectCommandLine ""
                                , expectMode Normal
                                ]
                , test "Case sensitive input" <|
                    \_ ->
                        [ ":", "a", "A" ]
                            |> initWithKeySequence
                            |> expectCommandLine "aA"
                , test "bdelete clears buffer" <|
                    \_ ->
                        initModelWithBuffer "a\nb"
                            |> keySequence [ ":", "b", "d", "e", "l", "e", "t", "e", "Enter" ]
                            |> Expect.all
                                [ expectCursor (Cursor 0 0)
                                , expectMode Normal
                                , expectBuffer ""
                                ]
                , test "bd clears buffer" <|
                    \_ ->
                        initModelWithBuffer "a\nb"
                            |> keySequence [ ":", "b", "d", "Enter" ]
                            |> Expect.all
                                [ expectCursor (Cursor 0 0)
                                , expectMode Normal
                                , expectBuffer ""
                                ]
                , describe ":<line> jumps to line"
                    [ test "jump to first line with :0" <|
                        \_ ->
                            initModelWithBuffer " a\n  b"
                                |> keySequence [ "j", ":", "0", "Enter" ]
                                |> expectCursorAt "a"
                    , test "jump to first line with :1" <|
                        \_ ->
                            initModelWithBuffer " a\n  b"
                                |> keySequence [ "j", ":", "1", "Enter" ]
                                |> expectCursorAt "a"
                    , test "jump to second line with :2" <|
                        \_ ->
                            initModelWithBuffer " a\n  b"
                                |> keySequence [ ":", "2", "Enter" ]
                                |> expectCursorAt "b"
                    , test "jump to second/last line with :3" <|
                        \_ ->
                            initModelWithBuffer " a\n  b"
                                |> keySequence [ ":", "3", "Enter" ]
                                |> expectCursorAt "b"
                    , test "jump will move cursor to character" <|
                        \_ ->
                            initModelWithBuffer " a"
                                |> keySequence [ ":", "1", "Enter" ]
                                |> expectCursorAt "a"
                    , fuzz3 Fuzzers.buffer Fuzzers.movements (Fuzz.intRange 0 20) ":<lineNumber> jumps to line" <|
                        \buffer movements line ->
                            let
                                expectedLine =
                                    line - 1 |> clamp 0 (bufferToLinesCount (Buffer buffer) - 1)

                                lineAsKeys =
                                    String.split "" (String.fromInt line)
                            in
                            initModelWithBuffer buffer
                                |> keySequence movements
                                |> keySequence (":" :: lineAsKeys ++ [ "Enter" ])
                                |> expectCursorLine expectedLine
                    ]
                ]
            ]
        ]



-- constants


shift : String
shift =
    String.repeat config.shiftWidth " "
