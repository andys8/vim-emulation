module Buffer exposing
    ( bufferToLines
    , bufferToWORDs
    , currentBufferLine
    , cursorChar_
    , cursorFromWORD
    , cursorInNormalModeBuffer
    , cursorInNormalModeLine
    , cursorLine_
    , cursorMoveDown
    , cursorMoveLeft
    , cursorMoveLineBegin
    , cursorMoveRight
    , cursorMoveToEndOfLine
    , cursorMoveUp
    , isWORDafterCursor
    , isWORDbeforeCursor
    , lastCharIndexInLine
    , lineToWORDs
    , splitBufferContent
    , splitLine
    )

import List.Extra
import Model exposing (..)
import Regex


type alias SplitResult =
    { linesBefore : List String
    , before : String
    , middle : String
    , after : String
    , linesAfter : List String
    }


bufferToLines : Buffer -> List String
bufferToLines (Buffer buffer) =
    String.lines buffer


currentBufferLine : Cursor -> Buffer -> String
currentBufferLine cursor buffer =
    buffer
        |> bufferToLines
        |> List.Extra.getAt (cursorLine_ cursor)
        -- Note: Not sure if this is a good idea.
        -- Shouldn't be possible and maybe handling is overhead, but defaulting can lead to errors.
        |> Maybe.withDefault ""


bufferToWORDs : Buffer -> List WORD
bufferToWORDs =
    bufferToLines >> List.indexedMap lineToWORDs >> List.concat


lineToWORDs : Int -> String -> List WORD
lineToWORDs lineNumber line =
    let
        regex =
            Regex.fromString "\\S+" |> Maybe.withDefault Regex.never

        res =
            Regex.find regex line
                |> List.map (\{ index, match } -> WORD (Position lineNumber index) match)
    in
    res


lastCharIndexInLine : Cursor -> Buffer -> Int
lastCharIndexInLine cursor buffer =
    String.length (currentBufferLine cursor buffer) - 1


splitBufferContent : Cursor -> Buffer -> SplitResult
splitBufferContent ((Cursor cursorLine cursorChar) as cursor) buffer =
    let
        lines =
            bufferToLines buffer

        linesBefore =
            List.take cursorLine lines

        currentLine =
            currentBufferLine cursor buffer

        linesAfter =
            List.drop (cursorLine + 1) lines

        ( beforeCurrentLine, middleCurrentLine, afterCurrentLine ) =
            splitLine cursorChar currentLine
    in
    { linesBefore = linesBefore
    , before = beforeCurrentLine
    , middle = middleCurrentLine
    , after = afterCurrentLine
    , linesAfter = linesAfter
    }


splitLine : Int -> String -> ( String, String, String )
splitLine cursorChar content =
    let
        charAt =
            cursorChar

        before =
            String.slice 0 charAt content

        middle =
            String.slice charAt (charAt + 1) content

        after =
            String.slice (charAt + 1) (String.length content) content
    in
    ( before, middle, after )



--TODO: Maybe refactor to storing a maximum cursor width in the model


cursorInNormalModeLine : Int -> Cursor -> Cursor
cursorInNormalModeLine currentLineLength ((Cursor cursorLine cursorChar) as cursor) =
    if cursorChar >= currentLineLength then
        Cursor cursorLine (currentLineLength - 1)

    else
        cursor


cursorInNormalModeBuffer : Buffer -> Cursor -> Cursor
cursorInNormalModeBuffer buffer cursor =
    let
        currentLineLength =
            String.length (currentBufferLine cursor buffer)
    in
    cursorInNormalModeLine currentLineLength cursor


cursorMoveToEndOfLine : Buffer -> Cursor -> Cursor
cursorMoveToEndOfLine buffer cursor =
    let
        cursorChar =
            String.length <| currentBufferLine cursor buffer
    in
    Cursor (cursorLine_ cursor) cursorChar


isWORDafterCursor : Cursor -> WORD -> Bool
isWORDafterCursor (Cursor cursorLine cursorChar) (WORD (Position wordLine wordChar) _) =
    wordLine > cursorLine || (cursorLine == wordLine && wordChar > cursorChar)


isWORDbeforeCursor : Cursor -> WORD -> Bool
isWORDbeforeCursor (Cursor cursorLine cursorChar) (WORD (Position wordLine wordChar) _) =
    wordLine < cursorLine || (cursorLine == wordLine && wordChar < cursorChar)


cursorMoveRight : Cursor -> Cursor
cursorMoveRight (Cursor line char) =
    Cursor line (char + 1)


cursorMoveLeft : Cursor -> Cursor
cursorMoveLeft (Cursor line char) =
    Cursor line (char - 1)


cursorMoveUp : Cursor -> Cursor
cursorMoveUp (Cursor line char) =
    Cursor (line - 1) char


cursorMoveDown : Cursor -> Cursor
cursorMoveDown (Cursor line char) =
    Cursor (line + 1) char


cursorMoveLineBegin : Cursor -> Cursor
cursorMoveLineBegin (Cursor line _) =
    Cursor line 0


cursorFromWORD : WORD -> Cursor
cursorFromWORD (WORD positon _) =
    cursorFromPosition positon


cursorFromPosition : Position -> Cursor
cursorFromPosition (Position line char) =
    Cursor line char


cursorChar_ : Cursor -> Int
cursorChar_ (Cursor _ cursorChar) =
    cursorChar


cursorLine_ : Cursor -> Int
cursorLine_ (Cursor cursorLine _) =
    cursorLine
