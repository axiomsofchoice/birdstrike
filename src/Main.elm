module Main exposing (..)

import AnimationFrame exposing (diffs)
import Char exposing (KeyCode, fromCode, toCode)
import Collage exposing (..)
import Color exposing (red)
import Element exposing (toHtml)
import Html exposing (Html, program)
import Keyboard exposing (downs, ups)
import Platform exposing (Program)
import Task
import Time exposing (Time)
import Window exposing (Size, resizes, size)


type alias Vec =
    { x : Float
    , y : Float
    }


type ArrowState
    = Flying
    | HitBird
    | HitGround


type alias Arrow =
    { pos : Vec
    , dir : Vec
    , state : ArrowState
    }


type Hit
    = BodyHit
    | NeckHit


type alias Bird =
    { pos : Vec
    , dir : Vec
    , bodyRadius : Float
    , neckWidth : Float
    , neckHeight : Float
    , hit : Hit
    }


type Loading
    = Ready
    | Loading Time


type alias Model =
    { score : Int
    , birds : List Bird
    , arrows : List Arrow
    , elevation : Float
    , arrowLoad : Loading
    , windSpeed : Float
    , windowSize : Size
    }


type KeyMsg
    = Down KeyCode
    | Up KeyCode


type Msg
    = Tick Time
    | WindowSize Size
    | Key KeyMsg


initialModel : Model
initialModel =
    { score = 0
    , birds = []
    , arrows = []
    , elevation = 0.0
    , arrowLoad = Ready
    , windSpeed = 0.0
    , windowSize = Size 0 0
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ diffs Tick
        , resizes WindowSize
        , downs (Key << Down)
        , ups (Key << Up)
        ]


main : Program Never Model Msg
main =
    program
        { init =
            ( initialModel
            , Task.perform WindowSize size
            )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


view : Model -> Html Msg
view model =
    let
        ball =
            circle 30
                |> filled red
                |> moveX model.windSpeed

        forms =
            [ ball ]
    in
        forms
            |> collage model.windowSize.width 400
            |> toHtml


updateKey : KeyMsg -> Model -> Model
updateKey key model =
    let
        elevationIncr =
            pi / 36
    in
        case key of
            -- Right arrow
            Down 39 ->
                { model
                    | elevation =
                        max 0 <| model.elevation - elevationIncr
                }

            -- Left arrow
            Down 37 ->
                { model
                    | elevation =
                        min (pi / 2) <| model.elevation + elevationIncr
                }

            -- Space bar down: load arrow
            Down 32 ->
                { model
                    | arrowLoad =
                        case model.arrowLoad of
                            Ready ->
                                Loading 0.0

                            state ->
                                state
                }

            -- Space bar up: shoot arrow
            Up 32 ->
                shootArrow model

            _ ->
                model


shootArrow : Model -> Model
shootArrow model =
    let
        dir =
            { x = 0, y = 0 }

        newArrow =
            { pos =
                { x = negate (toFloat model.windowSize.width) / 2
                , y = negate (toFloat model.windowSize.height) / 2
                }
            , dir = dir
            , state = Flying
            }
    in
        { model
            | arrowLoad = Ready
            , arrows = newArrow :: model.arrows
        }


updateTick : Time -> Model -> Model
updateTick dt model =
    { model
        | windSpeed = model.windSpeed + dt / 100
        , arrowLoad =
            case model.arrowLoad of
                Ready ->
                    Ready

                Loading t ->
                    Loading (t + dt)
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        -- Helper function to return a value without emitting a Cmd
        return m =
            ( m, Cmd.none )
    in
        case msg of
            Tick dt ->
                return <| updateTick dt model

            WindowSize ws ->
                return { model | windowSize = ws }

            Key key ->
                return <| updateKey key model
