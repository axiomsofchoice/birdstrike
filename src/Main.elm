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
      , dir = { x = 0.0, y = 0.0 }
      , state = Stored }]
    , elevation = 0.0
    , windSpeed = 1.0
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ diffs Tick ]


main : Program Never Model Msg
main =
    program
        { init = ( initialModel, Cmd.none )
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
            |> collage 400 400
            |> toHtml


animationRate = 100 -- Speed animation up or down to improve game play.
gravity = 9.8       -- The acceleration due to gravity.

updateArrow dt windSpeed arrow =
  let
   arrowPos = arrow.pos
   newArrowPos = {arrowPos | x = arrowPos.x - windSpeed + dt / animationRate
                           , y = arrowPos.y - gravity + dt / animationRate }
  in
   { arrow | pos = newArrowPos }

updateBird dt bird = bird

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
             ( { model |
                  arrows = List.map (updateArrow dt model.windSpeed) model.arrows
                , birds = List.map (updateBird dt) model.birds}
             , Cmd.none
             )
