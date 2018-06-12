module Main exposing (..)

import Html exposing (..)
import PannableVideo exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    PannableVideo.State


type Msg
    = NoOp
    | H PannableVideo.Msg


init : ( State, Cmd msg )
init =
    ( PannableVideo.initialState, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    PannableVideo.pannableVideo (\m -> H m) model (simpleVideoInfo "test.mp4" (720, 1280))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        H m ->
            ( PannableVideo.processEvent m model, Cmd.none )

        NoOp ->
            model ! []
