module PannableVideo exposing (Coordinate, Msg, State, initialState, pannableVideo, processEvent)

import Html exposing (Attribute, Html, div, video)
import Html.Attributes exposing (style)
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


pannableVideo : (Msg -> msg) -> State -> List (Attribute msg) -> List (Html msg) -> Html msg
pannableVideo emitter state attr html =
    let
        x =
            state.center.x + state.coords.x

        y =
            state.center.y + state.coords.y

        sc =
            state.sz
    in
    video
        ([ style [ ( "transform", translateX x ++ translateY y ++ scaleS sc ) ]
         , Touch.onStart (emitter << StartAt)
         , Touch.onMove (emitter << MoveAt)
         , Touch.onEnd (emitter << EndAt)
         ]
            ++ attr
        )
        html


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
            in
            { state | previous = touchCoordinates c }

        MoveAt c ->
            { state | coords = touchCoordinates c #- state.previous }

        EndAt c ->
            { state | center = state.center #+ state.coords, coords = origin }


origin : { x : Float, y : Float }
origin =
    { x = 0, y = 0 }


type Msg
    = StartAt Touch.Event
    | MoveAt Touch.Event
    | EndAt Touch.Event


convert : ( Float, Float ) -> Coordinate
convert ( x, y ) =
    { x = x, y = y }


touchCoordinates : Touch.Event -> Coordinate
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )
        |> convert


deltaFrom : State -> Touch.Event -> Maybe ( Float, Float )
deltaFrom state ev =
    if List.length ev.changedTouches == 2 && List.length ev.targetTouches == 2 then
        let
            eventTouches =
                listToTuple ev.changedTouches

            pairTouches =
                pairOfTouch ev.changedTouches state.touches
        in
        Maybe.map2 touchesDifference eventTouches pairTouches
    else
        Nothing


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( a, b ) ( c, d ) =
    sqrt ((a - c) ^ 2 + (b - d) ^ 2)


touchesDifference : ( Touch, Touch ) -> ( Touch, Touch ) -> ( Float, Float )
touchesDifference ( a, b ) ( c, d ) =
    let
        fst =
            distance a.clientPos c.clientPos

        sec =
            distance b.clientPos d.clientPos
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



-- handlePinchZoom : State -> Touch.Event -> { state: State, scale: Scale, delta: Coordinate }
-- handlePinchZoom state ev =
--         let
--             point1 = List.head List.filter (\m -> m.identifier == ev.changedTouches) state.touches
--         in
