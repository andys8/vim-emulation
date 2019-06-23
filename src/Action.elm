module Action exposing (Action(..), ActionChange(..), ActionNoChange(..), fromKeyStrokes, isChangeAction)


type Action
    = ActionChangeType ActionChange
    | ActionNoChangeType ActionNoChange


type ActionChange
    = Action_diw
    | Action_ciw
    | Action_dd
      -- TODO: Better handle duplicates like this
    | Action_cc
    | Action_S
    | Action_i
    | Action_I
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
    | Action_RightShift
    | Action_LeftShift


type ActionNoChange
    = Action_h
    | Action_j
    | Action_k
    | Action_l
    | Action_yy_or_Y
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
    | Action_Colon



-- TODO: C change to the end of line
-- TODO: D delete to the end of line
-- TODO: s replace character and type
-- TODO: ~ switch case and advance cursor
-- TODO: J to join current line with next, without space
-- TODO: cc is the same as S


fromKeyStrokes : List String -> Maybe Action
fromKeyStrokes keyStrokes =
    case keyStrokes of
        "w" :: "i" :: "d" :: _ ->
            Just <| ActionChangeType Action_diw

        "w" :: "i" :: "c" :: _ ->
            Just <| ActionChangeType Action_ciw

        "w" :: "i" :: "y" :: _ ->
            Just <| ActionNoChangeType Action_yiw

        "d" :: "d" :: _ ->
            Just <| ActionChangeType Action_dd

        "c" :: "c" :: _ ->
            Just <| ActionChangeType Action_cc

        "y" :: "y" :: _ ->
            Just <| ActionNoChangeType Action_yy_or_Y

        "g" :: "g" :: _ ->
            Just <| ActionNoChangeType Action_gg

        ">" :: ">" :: _ ->
            Just <| ActionChangeType Action_RightShift

        "<" :: "<" :: _ ->
            Just <| ActionChangeType Action_LeftShift

        -- Ignoring to prevent insert mode instead e.g. "diw"
        "i" :: "d" :: _ ->
            Nothing

        "i" :: "c" :: _ ->
            Nothing

        "i" :: "y" :: _ ->
            Nothing

        "Y" :: _ ->
            Just <| ActionNoChangeType Action_yy_or_Y

        "i" :: _ ->
            Just <| ActionChangeType Action_i

        "I" :: _ ->
            Just <| ActionChangeType Action_I

        ":" :: _ ->
            Just <| ActionNoChangeType Action_Colon

        "." :: _ ->
            Just <| ActionChangeType Action_Dot

        "S" :: _ ->
            Just <| ActionChangeType Action_S

        "a" :: _ ->
            Just <| ActionChangeType Action_a

        "A" :: _ ->
            Just <| ActionChangeType Action_A

        "p" :: _ ->
            Just <| ActionChangeType Action_p

        "P" :: _ ->
            Just <| ActionChangeType Action_P

        "o" :: _ ->
            Just <| ActionChangeType Action_o

        "O" :: _ ->
            Just <| ActionChangeType Action_O

        "Delete" :: _ ->
            Just <| ActionChangeType Action_Delete

        "x" :: _ ->
            Just <| ActionChangeType Action_x

        "X" :: _ ->
            Just <| ActionChangeType Action_X

        "0" :: _ ->
            Just <| ActionNoChangeType Action_0

        "^" :: _ ->
            Just <| ActionNoChangeType Action_Graph

        "G" :: _ ->
            Just <| ActionNoChangeType Action_G

        "w" :: _ ->
            Just <| ActionNoChangeType Action_w

        "W" :: _ ->
            Just <| ActionNoChangeType Action_W

        "b" :: _ ->
            Just <| ActionNoChangeType Action_b

        "B" :: _ ->
            Just <| ActionNoChangeType Action_B

        "e" :: _ ->
            Just <| ActionNoChangeType Action_e

        "E" :: _ ->
            Just <| ActionNoChangeType Action_E

        "$" :: _ ->
            Just <| ActionNoChangeType Action_Dollar

        "h" :: _ ->
            Just <| ActionNoChangeType Action_h

        "j" :: _ ->
            Just <| ActionNoChangeType Action_j

        "k" :: _ ->
            Just <| ActionNoChangeType Action_k

        "l" :: _ ->
            Just <| ActionNoChangeType Action_l

        _ ->
            Nothing


isChangeAction : Action -> Bool
isChangeAction action =
    case action of
        ActionChangeType _ ->
            True

        ActionNoChangeType _ ->
            False
