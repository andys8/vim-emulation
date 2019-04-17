module Model exposing (Action(..), Buffer(..), Cursor(..), Mode(..), Model, Msg(..), initModel)


type alias Model =
    { allKeyStrokes : List String
    , buffer : Buffer
    , mode : Mode
    , cursor : Cursor
    , keyStrokes : List String
    }


initModel : Model
initModel =
    { allKeyStrokes = []
    , buffer = Buffer ""
    , mode = Normal
    , cursor = Cursor 0 0
    , keyStrokes = []
    }


type Msg
    = NoOp
    | KeyDown String
    | SetMode Mode
    | ExecuteAction Action
    | SetCursor Cursor
    | InsertNewLine Int


type Buffer
    = Buffer String


type Action
    = DeleteLine Int


type Mode
    = Normal
    | Insert


type Cursor
    = Cursor Int Int
