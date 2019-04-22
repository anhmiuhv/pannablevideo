module PannableVideo exposing
    ( pannableVideo, advancedPannableVideo
    , State, VideoInfo, initialState, simpleVideoInfo
    , Msg, processEvent
    )

{-| This library provide a pannable and zoomable video element.

Look in [examples] to see how to use this library
[examples]: <https://github.com/anhmiuhv/pannablevideo/tree/master/examples>


# View

@docs pannableVideo, advancedPannableVideo


# Model

@docs State, VideoInfo, initialState, simpleVideoInfo


# Events

@docs Msg, processEvent

-}

import Html exposing (Attribute, Html, div, video)
import Html.Attributes exposing (controls, src, style)
import Html.Events.Extra.Touch exposing (Event, Touch, onEnd, onMove, onStart)


type alias Coordinate =
    { x : Float
    , y : Float
    }


type alias Scale =
    Float


{-| Represent the info of the video. This currently includes:

  - videoSrc : the source of the video
  - videoSize : the dimension of the video in pixel
  - minScale : minimum zoom scale
  - maxSCale : maximum zoom scale

-}
type alias VideoInfo =
    { videoSrc : String
    , videoSize : ( Int, Int )
    , minScale : Float
    , maxScale : Float
    }


{-| Internal state of the element. Should be part of model
-}
type State
    = State
        { coords : Coordinate
        , sz : Scale
        , previous : Coordinate
        , center : Coordinate
        , touches : List Touch
        , iden : Int
        , rangeX : Float
        , rangeY : Float
        }


{-| Simple VideoInfo object.

Take in: source of the video -> size in px

-}
simpleVideoInfo : String -> ( Int, Int ) -> VideoInfo
simpleVideoInfo src size =
    { videoSrc = src
    , videoSize = size
    , minScale = 1
    , maxScale = 5.0
    }


{-| The initial state to be put into init
-}
initialState : State
initialState =
    State
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
        , iden = -1
        , rangeX = 0.0
        , rangeY = 0.0
        }


translateX : Float -> String
translateX x =
    "translateX(" ++ String.fromFloat x ++ "px)"


translateY : Float -> String
translateY y =
    "translateY(" ++ String.fromFloat y ++ "px)"


scaleS : Float -> String
scaleS sc =
    let
        s =
            String.fromFloat sc
    in
    "scale(" ++ s ++ "," ++ s ++ ")"


{-| The zoomable, pannable video element. Look for State and VideoInfo below
-}
pannableVideo : (Msg -> msg) -> State -> VideoInfo -> Html msg
pannableVideo emitter state ({ videoSrc, videoSize } as info) =
    advancedPannableVideo emitter
        state
        info
        [ src videoSrc
        ]
        []


pixelToCSS : Int -> String
pixelToCSS px =
    String.fromInt px ++ "px"


{-| More advanced video element if you want to style, or do something I can't think of
-}
advancedPannableVideo : (Msg -> msg) -> State -> VideoInfo -> List (Attribute msg) -> List (Html msg) -> Html msg
advancedPannableVideo emitter (State state) info attr html =
    let
        x =
            clamp rangeMinX rangeX <| state.center.x + state.coords.x

        y =
            clamp rangeMinY rangeY <| state.center.y + state.coords.y

        sc =
            clamp info.minScale info.maxScale state.sz

        ( w, h ) =
            info.videoSize

        transform s =
            max 0 (round (toFloat s * state.sz) - s)
                |> toFloat
                |> (\a -> (/) a 2)
                |> round
                |> toFloat

        rangeX =
            transform w

        rangeMinX =
            0 - rangeX

        rangeY =
            transform h

        rangeMinY =
            0 - rangeY
    in
    div [ style "overflow" "hidden", style "width" (pixelToCSS w), style "height" (pixelToCSS h) ]
        [ video
            ([ style "transform" (translateX x ++ translateY y ++ scaleS sc)
             , onStart (emitter << StartAt)
             , onMove (emitter << MoveAt)
             , onEnd (emitter << EndAt)
             ]
                ++ attr
            )
            html
        ]


add : Coordinate -> Coordinate -> Coordinate
add c1 c2 =
    { x = c1.x + c2.x, y = c1.y + c2.y }

substract : Coordinate -> Coordinate -> Coordinate
substract c1 c2 =
    { x = c1.x - c2.x, y = c1.y - c2.y }


{-| Process Msg in the update function
-}
processEvent : Msg -> State -> State
processEvent ms (State state) =
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
                        extractIden (State state) c

                    else
                        state.iden
            in
            State { state | iden = iden, previous = touchCoordinates (State state) c, touches = touchCache }

        MoveAt c ->
            handlePinchZoom (State state) c

        EndAt c ->
            State { state | center = add state.center state.coords, coords = origin }


origin : { x : Float, y : Float }
origin =
    { x = 0, y = 0 }


{-| Internal Event Manager. Should be sent to processEvent in the update
-}
type Msg
    = StartAt Event
    | MoveAt Event
    | EndAt Event



----- Internal helpers ----


convert : ( Float, Float ) -> Coordinate
convert ( x, y ) =
    { x = x, y = y }


touchCoordinates : State -> Event -> Coordinate
touchCoordinates (State state) touchEvent =
    findTouchWithId state.iden touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )
        |> convert


extractIden : State -> Event -> Int
extractIden (State state) touchEvent =
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


deltaFrom : State -> Event -> DoNothing
deltaFrom (State state) ev =
    if List.length ev.changedTouches == 2 && List.length ev.targetTouches == 2 then
        let
            eventTouches =
                listToTuple ev.changedTouches

            pairTouches =
                pairOfTouch ev.changedTouches state.touches
        in
        No
            { dist = Maybe.map2 touchesDifference eventTouches pairTouches
            , aver = middlePoint eventTouches
            }

    else
        Yes


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( a, b ) ( c, d ) =
    sqrt ((a - c) ^ 2 + (b - d) ^ 2)


average : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
average ( a, b ) ( c, d ) =
    ( (a + c) / 2, (b + d) / 2 )


middlePoint : Maybe ( Touch, Touch ) -> Maybe Coordinate
middlePoint touches =
    Maybe.map
        (\( a, b ) ->
            average a.clientPos b.clientPos
                |> convert
        )
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
        Maybe.map2 (\a b -> ( a, b )) h t

    else
        Nothing


findEventWith : List Touch -> Touch -> Maybe Touch
findEventWith touches touch =
    touches
        |> List.filter (\m -> m.identifier == touch.identifier)
        |> List.head


handlePinchZoom : State -> Event -> State
handlePinchZoom (State state) ev =
    let
        co =
            if List.length ev.targetTouches == 2 then
                state.coords

            else
                substract (touchCoordinates (State state) ev) state.previous
    in
    case deltaFrom (State state) ev of
        No { dist, aver } ->
            case dist of
                Just ( a, c ) ->
                    State { state | iden = -1, sz = a / c, coords = substract (Maybe.withDefault state.coords aver) state.previous }

                Nothing ->
                    State { state | touches = [] }

        Yes ->
            State { state | coords = co }
