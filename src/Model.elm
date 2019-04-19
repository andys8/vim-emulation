module Model exposing
    ( Buffer(..)
    , Cursor(..)
    , CursorDirection(..)
    , Mode(..)
    , Model
    , Msg(..)
    , Position(..)
    , WORD(..)
    , initModel
    )


type alias Model =
    { allKeyStrokes : List String
    , buffer : Buffer
    , mode : Mode
    , cursor : Cursor
    , keyStrokes : List String
    , register : String
    }


initModel : Model
initModel =
    { allKeyStrokes = []
    , buffer = Buffer ""
    , mode = Normal
    , cursor = Cursor 0 0
    , keyStrokes = []
    , register = ""
    }


type Msg
    = NoOp
    | KeyDown String
    | SetMode Mode
    | SetCursor Cursor
    | ActionExecuted
    | InsertNewLine Int
    | YankLine Int
    | DeleteLine Int
    | PasteBefore
    | PasteAfter
    | MoveCursor CursorDirection


type CursorDirection
    = Up
    | Down
    | Right
    | Left
    | LineBegin
    | LineEnd
    | FirstWORDinLine
    | NextWORD
    | PrevWORD


type Buffer
    = Buffer String


type Mode
    = Normal
    | Insert


type Cursor
    = Cursor Int Int


type Position
    = Position Int Int


type WORD
    = WORD Position String
