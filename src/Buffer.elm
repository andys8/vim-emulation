module Buffer exposing
    ( currentBufferLine
    , cursorChar_
    , cursorInNormalModeBuffer
    , cursorInNormalModeLine
    , cursorLine_
    , cursorMoveDown
    , cursorMoveLeft
    , cursorMoveLineBegin
    , cursorMoveRight
    , cursorMoveUp
    , splitBufferContent
    , splitLine
    )

import List.Extra
import Model exposing (..)


currentBufferLine : Cursor -> String -> String
currentBufferLine cursor bufferContent =
    let
        (Cursor cursorLine _) =
            cursor
    in
    String.lines bufferContent
        |> List.Extra.getAt cursorLine
        -- Note: Not sure if this is a good idea.
        -- Shouldn't be possible and maybe handling is overhead, but defaulting can lead to errors.
        |> Maybe.withDefault ""


splitBufferContent :
    Cursor
    -> String
    ->
        { linesBefore : List String
        , before : String
        , middle : String
        , after : String
        , linesAfter : List String
        }
splitBufferContent ((Cursor cursorLine cursorChar) as cursor) bufferContent =
    let
        lines =
            String.lines bufferContent

        linesBefore =
            List.take cursorLine lines

        currentLine =
            currentBufferLine cursor bufferContent

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


cursorInNormalModeLine : Int -> Cursor -> Cursor
cursorInNormalModeLine currentLineLength ((Cursor cursorLine cursorChar) as cursor) =
    if cursorChar >= currentLineLength then
        Cursor cursorLine (currentLineLength - 1)

    else
        cursor


cursorInNormalModeBuffer : String -> Cursor -> Cursor
cursorInNormalModeBuffer bufferContent cursor =
    let
        currentLineLength =
            String.length (currentBufferLine cursor bufferContent)
    in
    cursorInNormalModeLine currentLineLength cursor


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


cursorChar_ : Cursor -> Int
cursorChar_ (Cursor _ cursorChar) =
    cursorChar


cursorLine_ : Cursor -> Int
cursorLine_ (Cursor cursorLine _) =
    cursorLine
