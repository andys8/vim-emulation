module Model exposing
    ( Buffer(..)
    , Command(..)
    , Cursor(..)
    , CursorDirection(..)
    , Mode(..)
    , Model
    , Msg(..)
    , Position(..)
    , Register(..)
    , TextObject(..)
    , WORD(..)
    , Word(..)
    , initModel
    )


type alias Model =
    { buffer : Buffer
    , mode : Mode
    , cursor : Cursor
    , keyStrokes : List String
    , register : Register
    }


initModel : Model
initModel =
    { buffer = Buffer ""
    , mode = Normal
    , cursor = Cursor 0 0
    , keyStrokes = []
    , register = RegisterString ""
    }


type Msg
    = NoOp
    | ActionExecuted
    | ClearLine Int
    | DeleteChar Int Int
    | DeleteLine Int
    | ApplyCommandOnTextObject Command TextObject
    | InsertNewLine Int
    | KeyDown String
    | MoveCursor CursorDirection
    | PasteAfter
    | PasteBefore
    | SetCursor Cursor
    | SetMode Mode
    | YankLine Int


type CursorDirection
    = Up
    | Down
    | Left Int
    | Right Int
    | LineBegin
    | LineEnd
    | FirstWORDinLine
    | NextWord
    | NextWordEnd
    | NextWORD
    | NextWORDEnd
    | PrevWord
    | PrevWORD
    | FirstLine
    | LastLine


type Buffer
    = Buffer String


type Mode
    = Normal
    | Insert


type Cursor
    = Cursor Int Int


type Position
    = Position Int Int


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


{-| Register (clipboard) can hold a string or line
-}
type Register
    = RegisterString String
    | RegisterLine String


type TextObject
    = -- "inner word", select [count] words (see |word|). White space between words is counted too.
      InWord


type Command
    = DeleteCommand
    | YankCommand
    | ChangeCommand
