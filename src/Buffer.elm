module Buffer exposing
    ( WordPositionType(..)
    , bufferToLines
    , bufferToLinesCount
    , bufferToString
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
    , cursorToPosition
    , isPositionAfterCursor
    , isPositionBeforeCursor
    , isPositionBeforeOrEqualToCursor
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



-- TODO: Think about introducing the concept of a ListCursor Char
-- See <https://cs-syd.eu/posts/2018-11-28-cursors-text>


type alias SplitResult =
    { before : String
    , middle : String
    , after : String
    }


bufferToString : Buffer -> String
bufferToString (Buffer buffer) =
    buffer


bufferToLines : Buffer -> List String
bufferToLines (Buffer buffer) =
    String.lines buffer


bufferToLinesCount : Buffer -> Int
bufferToLinesCount =
    bufferToLines >> List.length


currentBufferLine : Cursor -> Buffer -> String
currentBufferLine cursor =
    lineAtPosition (cursorToPosition cursor)


lineAtPosition : Position -> Buffer -> String
lineAtPosition (Position line _) buffer =
    buffer
        |> bufferToLines
        |> List.Extra.getAt line
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


splitBufferContent : Position -> Buffer -> SplitResult
splitBufferContent ((Position posLine posChar) as position) buffer =
    let
        lines =
            bufferToLines buffer

        linesBefore =
            List.take posLine lines

        currentLine =
            lineAtPosition position buffer

        linesAfter =
            List.drop (posLine + 1) lines

        splittedLine =
            splitLine posChar currentLine
    in
    { before = String.join "\n" (linesBefore ++ [ splittedLine.before ])
    , middle = splittedLine.middle
    , after = String.join "\n" (splittedLine.after :: linesAfter)
    }


splitLine : Int -> String -> SplitResult
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
    { before = before, middle = middle, after = after }


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


isPositionBeforeOrEqualToCursor : Cursor -> Position -> Bool
isPositionBeforeOrEqualToCursor (Cursor cursorLine cursorChar) (Position wordLine wordChar) =
    wordLine < cursorLine || (cursorLine == wordLine && wordChar <= cursorChar)


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


cursorToPosition : Cursor -> Position
cursorToPosition (Cursor line char) =
    Position line char
