module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as Decode
import Platform.Sub as Sub


type alias Model =
    { keyBuffer : List String
    , mode : Mode
    }


type Msg
    = NoOp
    | KeyDown String


type Mode
    = Normal
    | Insert


initModel =
    { keyBuffer = [], mode = Normal }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


sub =
    Browser.Events.onKeyDown keyDecoder
        |> Sub.map KeyDown


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> sub
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown string ->
            ( { model | keyBuffer = model.keyBuffer ++ [ string ] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Element.layout [ height fill, width fill ] <|
        column
            [ width fill, height fill ]
            [ viewBufferNames
            , viewBuffer
            , viewAirline model

            -- , viewKeyBuffer model.keyBuffer
            ]


viewBufferNames =
    row [] [ text "File" ]


viewBuffer =
    column [ height fill, width fill ] [ text "Buffer" ]


viewAirline model =
    row [ alignBottom ] [ text (modeToString model.mode) ]


viewKeyBuffer keyBuffer =
    row [ alignBottom ] <| List.map text keyBuffer


modeToString mode =
    case mode of
        Insert ->
            "insert"

        Normal ->
            "Normal"
