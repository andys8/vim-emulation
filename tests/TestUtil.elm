module TestUtil exposing
    ( expectBuffer
    , expectBufferTo
    , expectCommandLine
    , expectCursor
    , expectCursorAt
    , expectCursorLine
    , expectEqualBuffers
    , expectMode
    , initModelWithBuffer
    , initWithKeySequence
    , keySequence
    )

import Buffer exposing (..)
import Expect exposing (Expectation)
import List
import Main exposing (update)
import Model exposing (Buffer(..), Cursor(..), Mode(..), Model, Msg(..), initModel)
import Test exposing (..)
import Update.Extra exposing (sequence)


initModelWithBuffer : String -> ( Model, Cmd Msg )
initModelWithBuffer bufferContent =
    ( { initModel | buffer = Buffer bufferContent }, Cmd.none )


initWithKeySequence : List String -> ( Model, Cmd Msg )
initWithKeySequence keys =
    ( initModel, Cmd.none )
        |> keySequence keys


keySequence : List String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
keySequence keys =
    sequence update (List.map KeyDown keys)


expectBuffer : String -> ( Model, Cmd Msg ) -> Expectation
expectBuffer bufferContent =
    Tuple.first >> .buffer >> Expect.equal (Buffer bufferContent)


expectCommandLine : String -> ( Model, Cmd Msg ) -> Expectation
expectCommandLine commandLine =
    Tuple.first >> .commandLine >> Expect.equal commandLine


expectMode : Mode -> ( Model, Cmd Msg ) -> Expectation
expectMode mode =
    Tuple.first >> .mode >> Expect.equal mode


expectCursor : Cursor -> ( Model, Cmd Msg ) -> Expectation
expectCursor cursor =
    Tuple.first >> .cursor >> Expect.equal cursor


expectCursorAt : String -> ( Model, Cmd Msg ) -> Expectation
expectCursorAt char ( model, _ ) =
    let
        cursor =
            cursorInMode model.mode model.buffer model.cursor

        { middle } =
            splitBufferContent (cursorToPosition cursor) model.buffer
    in
    Expect.equal char middle


expectCursorLine : Int -> ( Model, Cmd Msg ) -> Expectation
expectCursorLine cursorLine =
    Tuple.first >> .cursor >> cursorLine_ >> Expect.equal cursorLine


expectEqualBuffers :
    String
    -> (( Model, Cmd Msg ) -> ( Model, Cmd Msg ))
    -> (( Model, Cmd Msg ) -> ( Model, Cmd Msg ))
    -> Expectation
expectEqualBuffers buffer f1 f2 =
    let
        model =
            initModelWithBuffer buffer

        toBuffer =
            Tuple.first >> .buffer
    in
    Expect.equal
        (model |> f1 |> toBuffer)
        (model |> f2 |> toBuffer)


expectBufferTo : (String -> Expectation) -> ( Model, Cmd Msg ) -> Expectation
expectBufferTo expect =
    Tuple.first
        >> .buffer
        >> (\(Buffer b) -> b)
        >> expect
