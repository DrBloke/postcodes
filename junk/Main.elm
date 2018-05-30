module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href, type_, value, src, placeholder)
import Html.Events exposing (onWithOptions, onClick, onInput)
import Router exposing (..)
import Navigation
import UrlParser exposing (..)
import Json.Decode as Decode
import Types exposing (..)
import RemoteData exposing (WebData, RemoteData(..))
import Http exposing (get, toTask, decodeUri)
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, required)
import Task
import Platform.Cmd exposing (batch)
import Parser exposing (..)


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--init


initialModel : Route -> Model
initialModel route =
    { route = route
    , history = []
    , locationInfo = NotAsked
    , postcode = "CB3 0FA"
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            parseLocation location
    in
        initialModel currentRoute
            |> update (OnLocationChange location)



-- Update


type Msg
    = ChangeLocation String
    | OnLocationChange Navigation.Location
    | LocationInfoResponse (WebData LocationInfo)
    | ChangePostcode Postcode
    | GetDataFromWeb


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLocation path ->
            ( { model
                | history = path :: model.history
              }
            , Navigation.newUrl path
            )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                { model
                    | route = newRoute
                    , postcode = Maybe.withDefault "" <| decodeUri <| Maybe.withDefault "" (parsePath UrlParser.string location)
                }
                    |> update GetDataFromWeb

        --web response
        LocationInfoResponse response ->
            ( { model | locationInfo = response }, Cmd.none )

        ChangePostcode postcode ->
            ( { model | postcode = postcode }, Cmd.none )

        GetDataFromWeb ->
            ( model
            , getLocationInfo model.postcode
            )



--web request


getLocationInfo : Postcode -> Cmd Msg
getLocationInfo postcode =
    Http.get
        ("http://api.postcodes.io/postcodes/"
            ++ postcode
        )
        decodeLocationInfo
        |> RemoteData.sendRequest
        |> Cmd.map LocationInfoResponse



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ type_ "text", placeholder "Postcode", onInput ChangePostcode, value model.postcode ] []
            , button [ onClick <| ChangeLocation model.postcode ] [ text "Get info" ]
            ]
        , div []
            [ page model.route model.locationInfo
            , nav model.postcode
            , ul [] (history model.history) --see Router module
            ]
        ]


page : Route -> WebData LocationInfo -> Html Msg
page route locationInfo =
    case route of
        HomeRoute ->
            text "Home"

        PostcodeRoute postcode ->
            case locationInfo of
                NotAsked ->
                    text "Initialising."

                Loading ->
                    text "Loading."

                Failure err ->
                    text ("Error: " ++ toString err)

                Success locationInfo ->
                    viewLocationInfo locationInfo

        NotFoundRoute ->
            text "Not Found"


viewLocationInfo : LocationInfo -> Html msg
viewLocationInfo locationInfo =
    div []
        [ text locationInfo.result.country
        , br [] []
        , text <| Maybe.withDefault "No region" locationInfo.result.region
        ]



-----


nav : String -> Html Msg
nav postcode =
    div []
        [ a [ href homePath, onLinkClick (ChangeLocation homePath) ] [ text "Home" ]
        , text " "
        , a [ href <| postcodePath postcode, onLinkClick (ChangeLocation <| postcodePath postcode) ] [ text "Postcode" ]
        , text (" ")
        ]


onLinkClick : msg -> Attribute msg
onLinkClick message =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
        onWithOptions "click" options (Decode.succeed message)



--save history of routes visited (use in autofill)


history : List String -> List (Html Msg)
history history =
    List.map historyLinks history


historyLinks : String -> Html Msg
historyLinks address =
    li [] [ a [ href (address), onLinkClick (ChangeLocation (address)) ] [ text address ] ]



--subs


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
