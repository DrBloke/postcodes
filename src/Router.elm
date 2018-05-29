module Router exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onWithOptions)
import Json.Decode as Decode
import Navigation
import UrlParser exposing (..)
import Types exposing (..)


--constants


homePath =
    "/"


postcodePath : String -> String
postcodePath postcode =
    postcode



--parsers


parseLocation : Navigation.Location -> Route
parseLocation location =
    case (UrlParser.parsePath matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute



--matchers


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map PostcodeRoute (UrlParser.string)
        ]
