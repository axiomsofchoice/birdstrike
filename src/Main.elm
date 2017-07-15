module Main exposing (..)

import Collage exposing (..)
import Color exposing (red)
import Element exposing (toHtml)
import Html exposing (Html)


main : Html msg
main =
    let
        forms =
            [ filled red (circle 50) ]
    in
        forms
            |> collage 400 400
            |> toHtml
