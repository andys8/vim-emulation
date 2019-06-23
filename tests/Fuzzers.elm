module Fuzzers exposing (buffer, movement, movements, simpleKey, spaces)

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


movements : Fuzzer (List String)
movements =
    Fuzz.list movement


movement : Fuzzer String
movement =
    [ "h", "j", "k", "l" ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
