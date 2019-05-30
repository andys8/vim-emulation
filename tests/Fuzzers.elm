module Fuzzers exposing (buffer, simpleKey)

import Fuzz exposing (Fuzzer)


buffer : Fuzzer String
buffer =
    Fuzz.list Fuzz.string
        |> Fuzz.map (String.join "\n")


simpleKey : Fuzzer String
simpleKey =
    [ "a", "b", "c" ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
