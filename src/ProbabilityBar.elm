-- Probability Bar
-- Graphic element that illustrates win probability between two competitors
--
-- Matthew Swanson
--

module ProbabilityBar exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (height, width)
import Svg exposing (circle, rect, svg)
import Svg.Attributes exposing (color, cx, cy, fill, fillOpacity, r, rx, ry, stroke, strokeWidth, viewBox, x, y)

type alias ProbabilityBarStyle =
    { origin : String
    , width : Int
    , height : Int
    , p_1 : Float
    , p_2 : Float
    , color_1 : String
    , color_2 : String
    , outlineColor : String
    }

probabilityBar : ProbabilityBarStyle -> Html msg
probabilityBar style =
    let
        o = style.origin
        w = style.width
        h = style.height
        r = String.fromInt (h // 2)
        p_1 = style.p_1
        p_2 = style.p_2
        (o_1, w_1) = getFirstCompetitorPosition o w p_1
        (o_2, w_2) = getSecondCompetitorPosition o w p_2
        color_1 = style.color_1
        color_2 = style.color_2
    in
        svg
            [viewBox "-2 -2 258 34", width w, height w]
            [ rect [x o_1, y o, width w_1, height h, rx r, ry r, fill color_1] []
            , rect [x o_2, y o, width w_2, height h, rx r, ry r, fill color_2] []
            , probabilityBarOutline o w h r style.outlineColor
            ]

-- Linear interpolation
lerp : Float -> Float -> Float -> Float
lerp v0 v1 t = (1 - t) * v0 + t * v1

-- Takes an origin, width, and probability
-- Returns a new origin and a new width
getFirstCompetitorPosition : String -> Int -> Float -> (String, Int)
getFirstCompetitorPosition o w p =
    let
        o_int = Maybe.withDefault 0 (String.toInt o)
        y_max = lerp (toFloat o_int) (toFloat (o_int + w)) p
    in
        (String.fromInt o_int, (round y_max) - o_int)

-- Takes an origin, width, and probability
-- Returns a new origin and a new width
getSecondCompetitorPosition : String -> Int -> Float -> (String, Int)
getSecondCompetitorPosition o w p =
    let
        o_int = Maybe.withDefault 0 (String.toInt o)
        y_min = lerp (toFloat o_int) (toFloat (o_int + w)) p
    in
        (String.fromInt (round y_min), o_int + w)

probabilityBarWidth : Int
probabilityBarWidth = 256

probabilityBarHeight : Int
probabilityBarHeight = 32

probabilityBarRadius : String
probabilityBarRadius = String.fromInt (probabilityBarHeight // 2)

probabilityBarOrigin : String
probabilityBarOrigin = String.fromInt (0 + 2)

probabilityBarOutline : String -> Int -> Int -> String -> String -> Html msg
probabilityBarOutline o w h r outlineColor =
    rect
        [ x o
        , y o
        , width w
        , height h
        , rx r
        , ry r
        , fillOpacity "0"
        , stroke outlineColor
        , strokeWidth "2px"]
        []
