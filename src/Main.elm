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
    | NotHit


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
    , windSpeed = 0.00001
    , windowSize = Size 0 0
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ diffs ( \ dt -> Tick (dt / animationRate))
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


groundLevel : Float -> Float
groundLevel height =
    -height / 3

buildArrowView : Arrow -> Form
buildArrowView arrow =
            rect 30 5
                |> filled red
                |> rotate (atan2 arrow.dir.y arrow.dir.x)
                |> moveX arrow.pos.x
                |> moveY arrow.pos.y

buildBirdView : Bird -> Form
buildBirdView bird = circle 30 |> filled red

view : Model -> Html Msg
view model =
    let
        arrowSprites = List.map buildArrowView model.arrows
        
        birdSprites = List.map buildBirdView model.birds

        forms =
            arrowSprites
            --arrowSprites ++ birdSprites
    in
        forms
            |> collage model.windowSize.width 400
            |> toHtml


-- Speed animation up or down to improve game play.


animationRate =
    100



-- The acceleration due to gravity.


gravity =
    9.8


updateArrow : Float -> Float -> Arrow -> Arrow
updateArrow dt windSpeed arrow =
    let
        arrowPos =
            arrow.pos
        arrowDir = arrow.dir
        newArrowPos = { arrowPos | x = arrowPos.x + (arrowDir.x - windSpeed) * dt
                                 , y = arrowPos.y + arrowDir.y * dt }
        newArrowDir = { arrowDir | y = arrowDir.y - gravity * dt }
    in
      { arrow | pos = newArrowPos, dir = newArrowDir }


updateBird : Float -> Bird -> Bird
updateBird dt bird =
    let
        birdPos =
            bird.pos
        birdDir = bird.dir

        newNormalBirdPos =
            { birdPos
                | x = birdPos.x + bird.dir.x * dt
                , y = birdPos.y + bird.dir.y * dt
            }

        deadBirdMotion = { bird | pos = newDeadBirdPos, dir = newDeadBirdDir }
        normalBirdMotion =
            { bird | pos = newNormalBirdPos }

        newDeadBirdPos = { birdPos | y = birdPos.y + birdDir.y * dt }

        newDeadBirdDir = { birdDir | y = birdDir.y - gravity * dt }
    in
        case bird.hit of
            -- Fall to the ground under gravity.
            BodyHit ->
                deadBirdMotion

            -- Also continue flying along in a straight line.
            NeckHit ->
                normalBirdMotion

            -- Just continue flying along in a straight line.
            NotHit ->
                normalBirdMotion


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
        , arrows = List.map (updateArrow dt model.windSpeed) model.arrows
        , birds = List.map (updateBird dt) model.birds
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
