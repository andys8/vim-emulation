module ConfigTest exposing (all)

import Config exposing (config)
import Expect
import Test exposing (..)


all : Test
all =
    describe "Config"
        [ describe "shiftWidth"
            [ test "greater 0" <|
                \_ ->
                    config.shiftWidth
                        |> Expect.greaterThan 0
            , test "max 8" <|
                \_ ->
                    config.shiftWidth
                        |> Expect.atMost 8
            ]
        ]
