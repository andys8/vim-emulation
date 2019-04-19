module Model exposing
    ( Buffer(..)
    , Cursor(..)
    , CursorDirection(..)
    , Mode(..)
    , Model
    , Msg(..)
    , Position(..)
    , WORD(..)
    , Word(..)
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
    | DeleteChar Int Int
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
    | NextWord
    | NextWORD
    | PrevWord
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



-- TODO: Empty lines as words / WORDs


{-| A WORD consists of a sequence of non-blank characters, separated with white
space. An empty line is also considered to be a WORD.
-}
type WORD
    = WORD Position String


{-| A word consists of a sequence of letters, digits and underscores, or a
sequence of other non-blank characters, separated with white space (spaces,
tabs, <EOL>). An empty line is also considered to be a word.
-}
type Word
    = Word Position String
