module Model exposing (Action(..), Cursor(..), Mode(..), Model, Msg(..), initModel)


type alias Model =
    { allKeyStrokes : List String
    , bufferContent : String
    , mode : Mode
    , cursor : Cursor
    , keyStrokes : List String
    }


initModel : Model
initModel =
    { allKeyStrokes = []
    , bufferContent = ""
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


type Action
    = DeleteLine Int


type Mode
    = Normal
    | Insert


type Cursor
    = Cursor Int Int
