module Buffer exposing
    ( bufferToLines
    , bufferToWORDs
    , bufferToWords
    , currentBufferLine
    , cursorChar_
    , cursorFromWORD
    , cursorFromWORDEnd
    , cursorFromWord
    , cursorFromWordEnd
    , cursorInNormalModeBuffer
    , cursorInNormalModeLine
    , cursorLine_
    , cursorMoveDown
    , cursorMoveLeft
    , cursorMoveLineBegin
    , cursorMoveRight
    , cursorMoveToEndOfLine
    , cursorMoveUp
    , isWORDEndAfterCursor
    , isWORDafterCursor
    , isWORDbeforeCursor
    , isWordAfterCursor
    , isWordBeforeCursor
    , isWordEndAfterCursor
    , lastCharIndexInLine
    , lineToWORDs
    , lineToWords
    , splitBufferContent
    , splitLine
    , wORDsToWORDEnds
    , wordsToWordEnds
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


bufferToWords : Buffer -> List Word
bufferToWords =
    bufferToLines >> List.indexedMap lineToWords >> List.concat


wordsToWordEnds : List Word -> List Word
wordsToWordEnds =
    List.filter (\(Word _ content) -> not (String.isEmpty content))


wORDsToWORDEnds : List WORD -> List WORD
wORDsToWORDEnds =
    List.filter (\(WORD _ content) -> not (String.isEmpty content))


lineToWORDs : Int -> String -> List WORD
lineToWORDs lineNumber line =
    let
        regex =
            Regex.fromString "\\S+" |> Maybe.withDefault Regex.never
    in
    if line == "" then
        [ WORD (Position lineNumber 0) "" ]

    else
        Regex.find regex line
            |> List.map (\{ index, match } -> WORD (Position lineNumber index) match)


lineToWords : Int -> String -> List Word
lineToWords lineNumber line =
    let
        regex =
            Regex.fromString "\\w+|[^\\w^\\s]+"
                |> Maybe.withDefault Regex.never
    in
    if line == "" then
        [ Word (Position lineNumber 0) "" ]

    else
        Regex.find regex line
            |> List.map (\{ index, match } -> Word (Position lineNumber index) match)


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


cursorInNormalModeBuffer : Buffer -> Cursor -> Cursor
cursorInNormalModeBuffer buffer cursor =
    cursorInNormalModeLine (currentBufferLine cursor buffer) cursor


cursorInNormalModeLine : String -> Cursor -> Cursor
cursorInNormalModeLine lineContent ((Cursor line char) as cursor) =
    let
        lineLength =
            String.length lineContent
    in
    if char >= lineLength && char > 0 then
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


isWORDafterCursor : Cursor -> WORD -> Bool
isWORDafterCursor (Cursor cursorLine cursorChar) (WORD (Position wordLine wordChar) _) =
    wordLine > cursorLine || (cursorLine == wordLine && wordChar > cursorChar)


isWORDbeforeCursor : Cursor -> WORD -> Bool
isWORDbeforeCursor (Cursor cursorLine cursorChar) (WORD (Position wordLine wordChar) _) =
    wordLine < cursorLine || (cursorLine == wordLine && wordChar < cursorChar)


isWordAfterCursor : Cursor -> Word -> Bool
isWordAfterCursor (Cursor cursorLine cursorChar) (Word (Position wordLine wordChar) _) =
    wordLine > cursorLine || (cursorLine == wordLine && wordChar > cursorChar)


isWordBeforeCursor : Cursor -> Word -> Bool
isWordBeforeCursor (Cursor cursorLine cursorChar) (Word (Position wordLine wordChar) _) =
    wordLine < cursorLine || (cursorLine == wordLine && wordChar < cursorChar)


isWordEndAfterCursor : Cursor -> Word -> Bool
isWordEndAfterCursor (Cursor cursorLine cursorChar) wordEnd =
    let
        (Cursor wordEndLine wordEndChar) =
            cursorFromWordEnd wordEnd
    in
    wordEndLine > cursorLine || (cursorLine == wordEndLine && wordEndChar > cursorChar)


isWORDEndAfterCursor : Cursor -> WORD -> Bool
isWORDEndAfterCursor (Cursor cursorLine cursorChar) wORDEnd =
    let
        (Cursor wORDEndLine wORDEndChar) =
            cursorFromWORDEnd wORDEnd
    in
    wORDEndLine > cursorLine || (cursorLine == wORDEndLine && wORDEndChar > cursorChar)


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


cursorFromWord : Word -> Cursor
cursorFromWord (Word positon _) =
    cursorFromPosition positon


cursorFromWordEnd : Word -> Cursor
cursorFromWordEnd (Word (Position line char) content) =
    Cursor line (char + String.length content - 1)


cursorFromWORDEnd : WORD -> Cursor
cursorFromWORDEnd (WORD (Position line char) content) =
    Cursor line (char + String.length content - 1)


cursorFromPosition : Position -> Cursor
cursorFromPosition (Position line char) =
    Cursor line char


cursorChar_ : Cursor -> Int
cursorChar_ (Cursor _ cursorChar) =
    cursorChar


cursorLine_ : Cursor -> Int
cursorLine_ (Cursor cursorLine _) =
    cursorLine
