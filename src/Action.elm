module Action exposing (fromKeyStrokes)


type ParsedKeys
    = Action Action_
    | Movement Movement_


type Action_
    = Action_diw
    | Action_ciw
    | Action_yiw
    | Action_dd
    | Action_yy
    | Action_gg
    | Action_i
    | Action_I
    | Action_S
    | Action_a
    | Action_A
    | Action_p
    | Action_P
    | Action_o
    | Action_O
    | Action_Delete
    | Action_x
    | Action_X
    | Action_0
    | Action_Graph
    | Action_G
    | Action_w
    | Action_W
    | Action_b
    | Action_B
    | Action_e
    | Action_E
    | Action_Dollar



-- TODO: More actions are actually movements


type Movement_
    = Movement_h
    | Movement_j
    | Movement_k
    | Movement_l


fromKeyStrokes : List String -> Maybe ParsedKeys
fromKeyStrokes keyStrokes =
    case keyStrokes of
        "w" :: "i" :: "d" :: _ ->
            Just <| Action Action_diw

        "w" :: "i" :: "c" :: _ ->
            Just <| Action Action_ciw

        "w" :: "i" :: "y" :: _ ->
            Just <| Action Action_yiw

        "d" :: "d" :: _ ->
            Just <| Action Action_dd

        "y" :: "y" :: _ ->
            Just <| Action Action_yy

        "g" :: "g" :: _ ->
            Just <| Action Action_gg

        "i" :: keys ->
            -- Ignore when CommandOnTextObject (e.g. "ciw") is the goal
            if List.member (List.head keys |> Maybe.withDefault "") [ "y", "c", "d" ] then
                Nothing

            else
                Just <| Action Action_i

        "I" :: _ ->
            Just <| Action Action_I

        "S" :: _ ->
            Just <| Action Action_S

        "a" :: _ ->
            Just <| Action Action_a

        "A" :: _ ->
            Just <| Action Action_A

        "p" :: _ ->
            Just <| Action Action_p

        "P" :: _ ->
            Just <| Action Action_P

        "o" :: _ ->
            Just <| Action Action_o

        "O" :: _ ->
            Just <| Action Action_O

        "Delete" :: _ ->
            Just <| Action Action_Delete

        "x" :: _ ->
            Just <| Action Action_x

        "X" :: _ ->
            Just <| Action Action_X

        -- TODO
        -- ifThenElse
        --     (cursorChar > 0)
        --     [ DeleteChar cursorLine (cursorChar - 1), MoveCursor (Left 1) ]
        --     []
        "0" :: _ ->
            Just <| Action Action_0

        "^" :: _ ->
            Just <| Action Action_Graph

        "G" :: _ ->
            Just <| Action Action_G

        "w" :: _ ->
            Just <| Action Action_w

        "W" :: _ ->
            Just <| Action Action_W

        "b" :: _ ->
            Just <| Action Action_b

        "B" :: _ ->
            Just <| Action Action_B

        "e" :: _ ->
            Just <| Action Action_e

        "E" :: _ ->
            Just <| Action Action_E

        "$" :: _ ->
            Just <| Action Action_Dollar

        "h" :: _ ->
            Just <| Movement Movement_h

        "j" :: _ ->
            Just <| Movement Movement_j

        "k" :: _ ->
            Just <| Movement Movement_k

        "l" :: _ ->
            Just <| Movement Movement_l

        _ ->
            Nothing
