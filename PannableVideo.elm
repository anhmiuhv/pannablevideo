module PannableVideo exposing (Coordinate, Msg, State, initialState, pannableVideo, processEvent)

import Debug exposing (..)
import Html exposing (Attribute, Html, div, video)
import Html.Attributes exposing (controls, src, style)
import Touch exposing (Event, Touch, onEnd, onMove, onStart)


type alias Coordinate =
    { x : Float
    , y : Float
    }


type alias Scale =
    Float


type alias State =
    { coords : Coordinate
    , sz : Scale
    , previous : Coordinate
    , center : Coordinate
    , touches : List Touch
    , pinch : Bool
    , iden : Int
    }


initialState : State
initialState =
    { coords =
        { x = 0
        , y = 0
        }
    , sz = 1
    , previous =
        { x = 0
        , y = 0
        }
    , center =
        { x = 0
        , y = 0
        }
    , touches = []
    , pinch = False
    , iden = -1
    }


translateX : Float -> String
translateX x =
    "translateX(" ++ toString x ++ "px)"


translateY : Float -> String
translateY y =
    "translateY(" ++ toString y ++ "px)"


scaleS : Float -> String
scaleS sc =
    let
        s =
            toString sc
    in
        "scale(" ++ s ++ "," ++ s ++ ")"


pannableVideo : (Msg -> msg) -> State -> String -> Html msg
pannableVideo emitter state string =
    advancedPannableVideo emitter
        state
        [ src string
        ]
        []


advancedPannableVideo : (Msg -> msg) -> State -> List (Attribute msg) -> List (Html msg) -> Html msg
advancedPannableVideo emitter state attr html =
    let
        x =
            state.center.x + state.coords.x

        y =
            state.center.y + state.coords.y

        sc =
            state.sz
    in
        div [ style [ ( "overflow", "hidden" ), ( "width", "1280px" ), ( "height", "720px" ) ] ]
            [ video
                ([ style [ ( "transform", translateX x ++ translateY y ++ scaleS sc ) ]
                 , Touch.onStart (emitter << StartAt)
                 , Touch.onMove (emitter << MoveAt)
                 , Touch.onEnd (emitter << EndAt)
                 ]
                    ++ attr
                )
                html
            ]


(#+) : Coordinate -> Coordinate -> Coordinate
(#+) c1 c2 =
    { x = c1.x + c2.x, y = c1.y + c2.y }
infixr 9 #+


(#-) : Coordinate -> Coordinate -> Coordinate
(#-) c1 c2 =
    { x = c1.x - c2.x, y = c1.y - c2.y }
infixr 9 #-


processEvent : Msg -> State -> State
processEvent ms state =
    case ms of
        StartAt c ->
            let
                touchCache =
                    state.touches
                        ++ (if List.length c.targetTouches == 2 then
                                c.targetTouches
                            else
                                []
                           )

                iden =
                    if state.iden == -1 then
                        extractIden state c
                    else
                        state.iden
            in
                { state | iden = iden, previous = touchCoordinates state c, touches = touchCache }

        MoveAt c ->
            let
                state2 =
                    handlePinchZoom state c

                co =
                    if state2.pinch then
                        state2.coords
                    else
                        touchCoordinates state2 c #- state2.previous
                iden = if state2.pinch then -1
                    else state2.iden
            in
                { state2 | coords = co, iden = iden }

        EndAt c ->
            { state | center = state.center #+ state.coords, coords = origin }


origin : { x : Float, y : Float }
origin =
    { x = 0, y = 0 }


type Msg
    = StartAt Touch.Event
    | MoveAt Touch.Event
    | EndAt Touch.Event



----- Internal helpers ----


convert : ( Float, Float ) -> Coordinate
convert ( x, y ) =
    { x = x, y = y }


touchCoordinates : State -> Touch.Event -> Coordinate
touchCoordinates state touchEvent =
    findTouchWithId state.iden touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )
        |> convert


extractIden : State -> Touch.Event -> Int
extractIden state touchEvent =
    findTouchWithId state.iden touchEvent.changedTouches
        |> Maybe.map .identifier
        |> Maybe.withDefault -1


findTouchWithId : Int -> List Touch -> Maybe Touch
findTouchWithId iden touches =
    case List.filter (\m -> m.identifier == iden) touches of
        [] ->
            List.head touches

        a :: _ ->
            Just a


type DoNothing
    = Yes
    | No AveAndDist


type alias AveAndDist =
    { aver : Maybe Coordinate
    , dist : Maybe ( Float, Float )
    }


deltaFrom : State -> Touch.Event -> DoNothing
deltaFrom state ev =
    if List.length ev.changedTouches == 2 && List.length ev.targetTouches == 2 then
        let
            eventTouches =
                listToTuple ev.changedTouches

            pairTouches =
                pairOfTouch ev.changedTouches state.touches
        in
            No
                { dist = (Maybe.map2 touchesDifference eventTouches pairTouches)
                , aver = middlePoint eventTouches
                }
    else
        Yes


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( a, b ) ( c, d ) =
    sqrt ((a - c) ^ 2 + (b - d) ^ 2)


average : ( Float, Float ) -> ( Float, Float ) -> (Float, Float)
average ( a, b ) ( c, d ) =
    ( (a + c) / 2, (b + d) / 2 )

middlePoint : Maybe (Touch, Touch) -> Maybe Coordinate
middlePoint touches =
    Maybe.map
        (\(a, b) -> average a.clientPos b.clientPos
        |> convert)
        touches


touchesDifference : ( Touch, Touch ) -> ( Touch, Touch ) -> ( Float, Float )
touchesDifference ( a, b ) ( c, d ) =
    let
        fst =
            distance a.clientPos b.clientPos

        sec =
            distance c.clientPos d.clientPos
    in
        ( fst, sec )


pairOfTouch : List Touch -> List Touch -> Maybe ( Touch, Touch )
pairOfTouch eventTouches stateTouches =
    let
        results =
            List.filterMap (findEventWith stateTouches) eventTouches
    in
        listToTuple results


listToTuple : List Touch -> Maybe ( Touch, Touch )
listToTuple touches =
    let
        h =
            List.head touches

        t =
            List.tail touches
                |> Maybe.andThen List.head
    in
        if List.length touches == 2 then
            Maybe.map2 (,) h t
        else
            Nothing


findEventWith : List Touch -> Touch -> Maybe Touch
findEventWith touches touch =
    touches
        |> List.filter (\m -> m.identifier == touch.identifier)
        |> List.head


handlePinchZoom : State -> Touch.Event -> State
handlePinchZoom state ev =
    case deltaFrom state ev of
        No { dist, aver } ->
            case dist of
                Just ( a, c ) ->
                    { state | sz = a / c, pinch = True, coords = Maybe.withDefault state.coords aver #- state.previous }

                Nothing ->
                    { state | pinch = log "here" False, touches = [] }

        Yes ->
            { state | pinch = False }
