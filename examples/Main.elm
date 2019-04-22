module Main exposing (Model, Msg(..), init, main, update, view)

import Html exposing (..)
import Browser
import PannableVideo exposing (..)


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


type alias Model =
    PannableVideo.State


type Msg
    = NoOp
    | H PannableVideo.Msg


init : ( State, Cmd msg )
init =
    ( PannableVideo.initialState, Cmd.none )




view : (Model, Cmd Msg) -> Html Msg
view (model, msg) =
    PannableVideo.pannableVideo (\m -> H m) model (simpleVideoInfo "test.mp4" ( 720, 1280 ))


update : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
update msg (model, mg) =
    case msg of
        H m ->
            ( PannableVideo.processEvent m model, Cmd.none )

        NoOp ->
            ( model
            , Cmd.none
            )
