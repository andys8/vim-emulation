module Fuzzers exposing (buffer, simpleKey, spaces)

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


spaces : Fuzzer String
spaces =
    Fuzz.intRange 0 20
        |> Fuzz.map (\i -> String.repeat i " ")
