module Model exposing
    ( Buffer(..)
    , Command(..)
    , Cursor(..)
    , CursorDirection(..)
    , File(..)
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

import Action exposing (Action)


type alias Model =
    { buffer : Buffer
    , mode : Mode
    , cursor : Cursor
    , normalModeKeyStrokes : List String
    , insertModeKeyStrokes : List String
    , register : Register
    , actions : List Action
    , commandLine : String
    }


initModel : Model
initModel =
    { buffer = Buffer ""
    , mode = Normal
    , cursor = Cursor 0 0
    , normalModeKeyStrokes = []
    , insertModeKeyStrokes = []
    , register = RegisterString ""
    , actions = []
    , commandLine = ""
    }


type Msg
    = NoOp
    | ActionExecuted Action
    | ClearBuffer
    | ClearLine Int
    | DeleteChar Position
    | DeleteLine Int
    | ApplyCommandOnTextObject Command TextObject
    | InsertNewLine Int
    | InsertText Position String
    | DeleteTextPartial Position String
    | KeyDown String
    | MoveCursor CursorDirection
    | PasteAfter
    | PasteBefore
    | SaveFile File
    | SetCursor Cursor
    | SetMode Mode
    | YankLine Int
    | RepeatLastChangeAction
    | ExecuteCmd (Cmd Msg)


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
      -- TODO: PrevWordEnd and PrevWORDEnd missing  (ge, gE)
    | FirstLine
    | LastLine
    | ToLine Int


type Buffer
    = Buffer String


type Mode
    = Normal
    | Insert
    | Command


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


type File
    = File String
