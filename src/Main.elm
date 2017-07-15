module Main exposing (..)

import AnimationFrame exposing (diffs)
import Collage exposing (..)
import Color exposing (red)
import Element exposing (toHtml)
import Html exposing (Html, program)
import Platform exposing (Program)
import Time exposing (Time)


type alias Model =
    { x : Float
    , y : Float
    }


type Msg
    = Tick Time


initialModel : Model
initialModel =
    { x = 0.0
    , y = 0.0
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
                |> moveX model.x
                |> moveY model.y

        forms =
            [ ball ]
    in
        forms
            |> collage 400 400
            |> toHtml


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model | x = model.x + dt / 100 }, Cmd.none )
