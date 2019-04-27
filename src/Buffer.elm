module Buffer exposing
    ( WordPositionType(..)
    , bufferToLines
    , bufferToWORDs
    , bufferToWords
    , currentBufferLine
    , cursorChar_
    , cursorFromPosition
    , cursorInMode
    , cursorInModeLine
    , cursorLine_
    , cursorMoveDown
    , cursorMoveLeft
    , cursorMoveLineBegin
    , cursorMoveRight
    , cursorMoveToEndOfLine
    , cursorMoveUp
    , isPositionAfterCursor
    , isPositionBeforeCursor
    , lastCharIndexInLine
    , lineToWORDs
    , lineToWords
    , rejectEmptyWORDs
    , rejectEmptyWords
    , splitBufferContent
    , splitLine
    , wORDToPosition
    , wordToPosition
    )

import List.Extra
import Model exposing (..)
import Regex exposing (Regex)


type WordPositionType
    = WordBegin
    | WordEnd


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


bufferToWords : Buffer -> List Word
bufferToWords =
    bufferToLines >> List.indexedMap lineToWords >> List.concat


rejectEmptyWords : List Word -> List Word
rejectEmptyWords =
    List.filter (\(Word _ content) -> not (String.isEmpty content))


rejectEmptyWORDs : List WORD -> List WORD
rejectEmptyWORDs =
    List.filter (\(WORD _ content) -> not (String.isEmpty content))


wORDRegex : Regex
wORDRegex =
    "\\S+"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


wordRegex : Regex
wordRegex =
    "\\w+|[^\\w^\\s]+"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


lineToWORDs : Int -> String -> List WORD
lineToWORDs lineNumber line =
    if line == "" then
        [ WORD (Position lineNumber 0) "" ]

    else
        Regex.find wORDRegex line
            |> List.map (\{ index, match } -> WORD (Position lineNumber index) match)


lineToWords : Int -> String -> List Word
lineToWords lineNumber line =
    if line == "" then
        [ Word (Position lineNumber 0) "" ]

    else
        Regex.find wordRegex line
            |> List.map (\{ index, match } -> Word (Position lineNumber index) match)


lastCharIndexInLine : Cursor -> Buffer -> Int
lastCharIndexInLine cursor buffer =
    String.length (currentBufferLine cursor buffer) - 1



-- TODO: Could accept position instead of cursor


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


cursorInMode : Mode -> Buffer -> Cursor -> Cursor
cursorInMode mode buffer cursor =
    cursorInModeLine mode (currentBufferLine cursor buffer) cursor


cursorInModeLine : Mode -> String -> Cursor -> Cursor
cursorInModeLine mode lineContent ((Cursor line char) as cursor) =
    let
        lineLength =
            String.length lineContent
    in
    if mode == Normal && char >= lineLength && char > 0 then
        Cursor line (lineLength - 1)

    else
        cursor


cursorMoveToEndOfLine : Buffer -> Cursor -> Cursor
cursorMoveToEndOfLine buffer cursor =
    let
        cursorChar =
            String.length <| currentBufferLine cursor buffer
    in
    Cursor (cursorLine_ cursor) cursorChar


isPositionAfterCursor : Cursor -> Position -> Bool
isPositionAfterCursor (Cursor cursorLine cursorChar) (Position posLine posChar) =
    posLine > cursorLine || (cursorLine == posLine && posChar > cursorChar)


isPositionBeforeCursor : Cursor -> Position -> Bool
isPositionBeforeCursor (Cursor cursorLine cursorChar) (Position wordLine wordChar) =
    wordLine < cursorLine || (cursorLine == wordLine && wordChar < cursorChar)


wordToPosition : WordPositionType -> Word -> Position
wordToPosition positionType (Word position content) =
    positionForType positionType position content


wORDToPosition : WordPositionType -> WORD -> Position
wORDToPosition positionType (WORD position content) =
    positionForType positionType position content


positionForType : WordPositionType -> Position -> String -> Position
positionForType positionType (Position line char) content =
    case positionType of
        WordBegin ->
            Position line char

        WordEnd ->
            Position line (char + String.length content - 1)


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


cursorFromPosition : Position -> Cursor
cursorFromPosition (Position line char) =
    Cursor line char


cursorChar_ : Cursor -> Int
cursorChar_ (Cursor _ cursorChar) =
    cursorChar


cursorLine_ : Cursor -> Int
cursorLine_ (Cursor cursorLine _) =
    cursorLine
