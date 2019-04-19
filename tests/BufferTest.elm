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
                        |> Expect.equal []
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
                    lineToWORDs 0 "(a)"
                        |> Expect.equal [ WORD (Position 0 0) "(a)" ]
            , test "parens with spaces are separate WORDs" <|
                \_ ->
                    lineToWORDs 0 "( a )"
                        |> Expect.equal
                            [ WORD (Position 0 0) "("
                            , WORD (Position 0 2) "a"
                            , WORD (Position 0 4) ")"
                            ]
            ]
        , describe "bufferToWORDs"
            [ test "empty" <|
                \_ ->
                    bufferToWORDs (Buffer "")
                        |> Expect.equal []
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

                    contentWORD (WORD p s) =
                        ( p, s )

                    contentWord (Word p s) =
                        ( p, s )
                in
                Expect.equal
                    (bufferToWords buffer |> List.map contentWord)
                    (bufferToWORDs buffer |> List.map contentWORD)
        ]


simpleStringFuzzer : Fuzzer String
simpleStringFuzzer =
    [ "a", "b", " ", "\n" ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
        |> Fuzz.list
        |> Fuzz.map (String.join "")
