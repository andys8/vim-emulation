module Action exposing (Action(..), Change(..), Motion(..), fromKeyStrokes, isChangeAction)


type Action
    = ActionChange Change
    | ActionMotion Motion


type Change
    = Action_diw
    | Action_ciw
    | Action_dd
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
    | Action_Dot


type Motion
    = Action_h
    | Action_j
    | Action_k
    | Action_l
    | Action_yy
    | Action_0
    | Action_Graph
    | Action_gg
    | Action_G
    | Action_yiw
    | Action_w
    | Action_W
    | Action_b
    | Action_B
    | Action_e
    | Action_E
    | Action_Dollar


fromKeyStrokes : List String -> Maybe Action
fromKeyStrokes keyStrokes =
    case keyStrokes of
        "w" :: "i" :: "d" :: _ ->
            Just <| ActionChange Action_diw

        "w" :: "i" :: "c" :: _ ->
            Just <| ActionChange Action_ciw

        "w" :: "i" :: "y" :: _ ->
            Just <| ActionMotion Action_yiw

        "d" :: "d" :: _ ->
            Just <| ActionChange Action_dd

        "y" :: "y" :: _ ->
            Just <| ActionMotion Action_yy

        "g" :: "g" :: _ ->
            Just <| ActionMotion Action_gg

        -- Ignoring to prevent insert mode instead e.g. "diw"
        "i" :: "d" :: _ ->
            Nothing

        "i" :: "c" :: _ ->
            Nothing

        "i" :: "y" :: _ ->
            Nothing

        "i" :: _ ->
            Just <| ActionChange Action_i

        "I" :: _ ->
            Just <| ActionChange Action_I

        "." :: _ ->
            Just <| ActionChange Action_Dot

        "S" :: _ ->
            Just <| ActionChange Action_S

        "a" :: _ ->
            Just <| ActionChange Action_a

        "A" :: _ ->
            Just <| ActionChange Action_A

        "p" :: _ ->
            Just <| ActionChange Action_p

        "P" :: _ ->
            Just <| ActionChange Action_P

        "o" :: _ ->
            Just <| ActionChange Action_o

        "O" :: _ ->
            Just <| ActionChange Action_O

        "Delete" :: _ ->
            Just <| ActionChange Action_Delete

        "x" :: _ ->
            Just <| ActionChange Action_x

        "X" :: _ ->
            Just <| ActionChange Action_X

        "0" :: _ ->
            Just <| ActionMotion Action_0

        "^" :: _ ->
            Just <| ActionMotion Action_Graph

        "G" :: _ ->
            Just <| ActionMotion Action_G

        "w" :: _ ->
            Just <| ActionMotion Action_w

        "W" :: _ ->
            Just <| ActionMotion Action_W

        "b" :: _ ->
            Just <| ActionMotion Action_b

        "B" :: _ ->
            Just <| ActionMotion Action_B

        "e" :: _ ->
            Just <| ActionMotion Action_e

        "E" :: _ ->
            Just <| ActionMotion Action_E

        "$" :: _ ->
            Just <| ActionMotion Action_Dollar

        "h" :: _ ->
            Just <| ActionMotion Action_h

        "j" :: _ ->
            Just <| ActionMotion Action_j

        "k" :: _ ->
            Just <| ActionMotion Action_k

        "l" :: _ ->
            Just <| ActionMotion Action_l

        _ ->
            Nothing


isChangeAction : Action -> Bool
isChangeAction action =
    case action of
        ActionChange _ ->
            True

        ActionMotion _ ->
            False
