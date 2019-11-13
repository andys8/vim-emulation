module Config exposing (Colors, Config, config)

import Element exposing (Color, rgb255)


type alias Config =
    { shiftWidth : Int
    , colors : Colors
    }


config : Config
config =
    { shiftWidth = 4
    , colors = colors
    }


type alias Colors =
    { white : Color
    , black : Color
    , bufferNamesLineBg : Color
    , bufferNameBg : Color
    , bufferNameRightBg : Color
    , bufferBg : Color
    , bufferFont : Color
    , cursor : Color
    , lineNumberFont : Color
    , lineNumberCurrentFont : Color
    , airLineBg : Color
    , airLineNormalModeBg : Color
    , airLineInsertModeBg : Color
    }


colors : Colors
colors =
    { white = rgb255 255 255 255
    , black = rgb255 0 0 0
    , bufferNamesLineBg = rgb255 48 48 48
    , bufferNameBg = rgb255 175 135 255
    , bufferNameRightBg = rgb255 95 95 175
    , bufferBg = rgb255 40 42 54
    , bufferFont = rgb255 248 248 242
    , cursor = rgb255 255 221 51
    , lineNumberFont = rgb255 95 95 135
    , lineNumberCurrentFont = rgb255 255 255 135
    , airLineBg = rgb255 95 95 95
    , airLineNormalModeBg = rgb255 175 135 255
    , airLineInsertModeBg = rgb255 95 255 135
    }
