module Main exposing (..)

import AnimationFrame exposing (diffs)
import Collage exposing (..)
import Color exposing (red)
import Element exposing (toHtml)
import Html exposing (Html, program)
import Platform exposing (Program)
import Time exposing (Time)


type alias Vec =
    { x : Float
    , y : Float
    }


type ArrowState
    = Stored
    | Loading Time
    | Flying
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


type alias Model =
    { score : Int
    , birds : List Bird
    , arrows : List Arrow
    , elevation : Float
    , windSpeed : Float
    }


type Msg
    = Tick Time


initialModel : Model
initialModel =
    { score = 0
    , birds = []
    , arrows = [
      { pos = { x = 0.0, y = 0.0 }
      , dir = { x = 0.0, y = 1.0 }
      , state = Stored }]
    , elevation = 0.0
    , windSpeed = 0.00001
    }

animationRate = 100.0 -- Speed animation up or down to improve game play.

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ diffs ( \ dt -> Tick (dt / animationRate)) ]

main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


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
            |> collage 400 400
            |> toHtml


gravity = 9.8        -- The acceleration due to gravity.

updateArrow : Float -> Float -> Arrow -> Arrow
updateArrow dt windSpeed arrow =
  let
   arrowPos = arrow.pos
   arrowDir = arrow.dir
   newArrowPos = { arrowPos | x = arrowPos.x + (arrowDir.x - windSpeed) * dt
                            , y = arrowPos.y + arrowDir.y * dt }
   newArrowDir = { arrowDir | y = arrowDir.y - gravity * dt }
  in
   { arrow | pos = newArrowPos, dir = newArrowDir }

updateBird : Float -> Bird -> Bird
updateBird dt bird =
  let
    birdPos = bird.pos
    birdDir = bird.dir
    newNormalBirdPos = {birdPos | x = birdPos.x + bird.dir.x * dt
                          , y = birdPos.y + bird.dir.y * dt }
    normalBirdMotion = { bird | pos = newNormalBirdPos }
    newDeadBirdPos = { birdPos | y = birdPos.y + birdDir.y * dt }
    newDeadBirdDir = { birdDir | y = birdDir.y - gravity * dt }
    deadBirdMotion = { bird | pos = newDeadBirdPos, dir = newDeadBirdDir }
  in
    case bird.hit of
      BodyHit -> deadBirdMotion   -- Fall to the ground under gravity.
      NeckHit -> normalBirdMotion -- Also continue flying along in a straight line.
      NotHit -> normalBirdMotion  -- Just continue flying along in a straight line.

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
             ( { model |
                  arrows = List.map (updateArrow dt model.windSpeed) model.arrows
                , birds = List.map (updateBird dt) model.birds}
             , Cmd.none
             )
