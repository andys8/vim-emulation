module BufferTest exposing (all)

import Buffer exposing (..)
import Expect
import Fuzz exposing (Fuzzer)
import List
import Model exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Buffer"
        [ describe "lineToWORDs"
            [ test "empty" <|
                \_ ->
                    lineToWORDs 0 ""
                        |> Expect.equal [ WORD (Position 0 0) "" ]
            , test "one word" <|
                \_ ->
                    lineToWORDs 0 "a"
                        |> Expect.equal [ WORD (Position 0 0) "a" ]
            , test "two words" <|
                \_ ->
                    lineToWORDs 0 "abc def"
                        |> Expect.equal
                            [ WORD (Position 0 0) "abc"
                            , WORD (Position 0 4) "def"
                            ]
            , test "handle beginning, trailing and inner spaces" <|
                \_ ->
                    lineToWORDs 0 "  abc    def  "
                        |> Expect.equal
                            [ WORD (Position 0 2) "abc"
                            , WORD (Position 0 9) "def"
                            ]
            , test "parens are part of WORD" <|
                \_ ->
                    lineToWORDs 0 "(a)a"
                        |> Expect.equal [ WORD (Position 0 0) "(a)a" ]
            , test "parens with spaces are separate WORDs" <|
                \_ ->
                    lineToWORDs 0 "( a )"
                        |> Expect.equal
                            [ WORD (Position 0 0) "("
                            , WORD (Position 0 2) "a"
                            , WORD (Position 0 4) ")"
                            ]
            ]
        , describe "lineToWords"
            [ test "parens are independant words" <|
                \_ ->
                    lineToWords 0 "(a)a"
                        |> Expect.equal
                            [ Word (Position 0 0) "("
                            , Word (Position 0 1) "a"
                            , Word (Position 0 2) ")"
                            , Word (Position 0 3) "a"
                            ]
            , test "words are indepent even if there are no separating spaces" <|
                \_ ->
                    lineToWords 0 "(abc)"
                        |> Expect.equal
                            [ Word (Position 0 0) "("
                            , Word (Position 0 1) "abc"
                            , Word (Position 0 4) ")"
                            ]
            , test "parens with spaces" <|
                \_ ->
                    lineToWords 0 "( a )"
                        |> Expect.equal
                            [ Word (Position 0 0) "("
                            , Word (Position 0 2) "a"
                            , Word (Position 0 4) ")"
                            ]
            , test "different parens and special characters" <|
                \_ ->
                    lineToWords 0 "( abc ) (abc) {} <> . _-"
                        |> Expect.equal
                            [ Word (Position 0 0) "("
                            , Word (Position 0 2) "abc"
                            , Word (Position 0 6) ")"
                            , Word (Position 0 8) "("
                            , Word (Position 0 9) "abc"
                            , Word (Position 0 12) ")"
                            , Word (Position 0 14) "{}"
                            , Word (Position 0 17) "<>"
                            , Word (Position 0 20) "."
                            , Word (Position 0 22) "_"
                            , Word (Position 0 23) "-"
                            ]
            ]
        , describe "bufferToWords"
            [ test "empty" <|
                \_ ->
                    bufferToWords (Buffer "")
                        |> Expect.equal [ Word (Position 0 0) "" ]
            , test "multiple lines" <|
                \_ ->
                    bufferToWords (Buffer "l1\nl2")
                        |> Expect.equal
                            [ Word (Position 0 0) "l1"
                            , Word (Position 1 0) "l2"
                            ]
            , test "empty lines are considered words" <|
                \_ ->
                    bufferToWords (Buffer "\n")
                        |> Expect.equal
                            [ Word (Position 0 0) ""
                            , Word (Position 1 0) ""
                            ]
            ]
        , describe "wordsToWordEnds"
            [ test "empty lines are ignored when using WordEnd" <|
                \_ ->
                    bufferToWords (Buffer "l1\n\nl2")
                        |> wordsToWordEnds
                        |> Expect.equal
                            [ Word (Position 0 0) "l1"
                            , Word (Position 2 0) "l2"
                            ]
            ]
        , describe "bufferToWORDs"
            [ test "empty" <|
                \_ ->
                    bufferToWORDs (Buffer "")
                        |> Expect.equal [ WORD (Position 0 0) "" ]
            , test "multiple lines" <|
                \_ ->
                    bufferToWORDs (Buffer "l1\nl2")
                        |> Expect.equal
                            [ WORD (Position 0 0) "l1"
                            , WORD (Position 1 0) "l2"
                            ]
            ]
        , fuzz simpleStringFuzzer "bufferToWords and bufferToWORDS are the same for simple cases" <|
            \str ->
                let
                    buffer =
                        Buffer str

                    resWORDs =
                        bufferToWORDs buffer |> List.map (\(WORD _ s) -> s)

                    resWords =
                        bufferToWords buffer |> List.map (\(Word _ s) -> s)
                in
                Expect.equal resWORDs resWords
        ]


simpleStringFuzzer : Fuzzer String
simpleStringFuzzer =
    [ "a", "b", " ", "\n" ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
        |> Fuzz.list
        |> Fuzz.map (String.join "")
