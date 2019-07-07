module Action exposing
    ( Action(..)
    , ActionChange(..)
    , ActionInsert(..)
    , ActionMove(..)
    , fromKeyStrokes
    , toActionChange
    , toActionInsert
    , toActionMove
    )


type Action
    = ActionInsertType ActionInsert
    | ActionChangeType ActionChange
    | ActionMoveType ActionMove


{-| Leads to insert mode, which has to be considered for repeat (.)
-}
type ActionInsert
    = Action_ciw
    | Action_cc_or_S
    | Action_i
    | Action_I
    | Action_a
    | Action_A
    | Action_o
    | Action_O


{-| Changes the buffer, but without insert mode involved
-}
type ActionChange
    = Action_diw
    | Action_dd
    | Action_p
    | Action_P
    | Action_Delete
    | Action_x
    | Action_X
    | Action_Dot
    | Action_RightShift
    | Action_LeftShift


{-| Doesn't change the buffer at all
-}
type ActionMove
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


fromKeyStrokes : List String -> Maybe Action
fromKeyStrokes keyStrokes =
    case keyStrokes of
        "w" :: "i" :: "d" :: _ ->
            Just <| ActionChangeType Action_diw

        "w" :: "i" :: "c" :: _ ->
            Just <| ActionInsertType Action_ciw

        "w" :: "i" :: "y" :: _ ->
            Just <| ActionMoveType Action_yiw

        "d" :: "d" :: _ ->
            Just <| ActionChangeType Action_dd

        "c" :: "c" :: _ ->
            Just <| ActionInsertType Action_cc_or_S

        "y" :: "y" :: _ ->
            Just <| ActionMoveType Action_yy_or_Y

        "g" :: "g" :: _ ->
            Just <| ActionMoveType Action_gg

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
            Just <| ActionMoveType Action_yy_or_Y

        "i" :: _ ->
            Just <| ActionInsertType Action_i

        "I" :: _ ->
            Just <| ActionInsertType Action_I

        ":" :: _ ->
            Just <| ActionMoveType Action_Colon

        "." :: _ ->
            Just <| ActionChangeType Action_Dot

        "S" :: _ ->
            Just <| ActionInsertType Action_cc_or_S

        "a" :: _ ->
            Just <| ActionInsertType Action_a

        "A" :: _ ->
            Just <| ActionInsertType Action_A

        "p" :: _ ->
            Just <| ActionChangeType Action_p

        "P" :: _ ->
            Just <| ActionChangeType Action_P

        "o" :: _ ->
            Just <| ActionInsertType Action_o

        "O" :: _ ->
            Just <| ActionInsertType Action_O

        "Delete" :: _ ->
            Just <| ActionChangeType Action_Delete

        "x" :: _ ->
            Just <| ActionChangeType Action_x

        "X" :: _ ->
            Just <| ActionChangeType Action_X

        "0" :: _ ->
            Just <| ActionMoveType Action_0

        "^" :: _ ->
            Just <| ActionMoveType Action_Graph

        "G" :: _ ->
            Just <| ActionMoveType Action_G

        "w" :: _ ->
            Just <| ActionMoveType Action_w

        "W" :: _ ->
            Just <| ActionMoveType Action_W

        "b" :: _ ->
            Just <| ActionMoveType Action_b

        "B" :: _ ->
            Just <| ActionMoveType Action_B

        "e" :: _ ->
            Just <| ActionMoveType Action_e

        "E" :: _ ->
            Just <| ActionMoveType Action_E

        "$" :: _ ->
            Just <| ActionMoveType Action_Dollar

        "h" :: _ ->
            Just <| ActionMoveType Action_h

        "j" :: _ ->
            Just <| ActionMoveType Action_j

        "k" :: _ ->
            Just <| ActionMoveType Action_k

        "l" :: _ ->
            Just <| ActionMoveType Action_l

        _ ->
            Nothing



-- convert (could be prism)


toActionChange : Action -> Maybe ActionChange
toActionChange actionType =
    case actionType of
        ActionChangeType action ->
            Just action

        _ ->
            Nothing


toActionInsert : Action -> Maybe ActionInsert
toActionInsert actionType =
    case actionType of
        ActionInsertType action ->
            Just action

        _ ->
            Nothing


toActionMove : Action -> Maybe ActionMove
toActionMove actionType =
    case actionType of
        ActionMoveType action ->
            Just action

        _ ->
            Nothing
