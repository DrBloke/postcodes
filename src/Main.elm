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
import PostcodeParser exposing (..)


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
    , nearestInfo = NotAsked
    , postcode = "CB3 0FA"
    , validPostcode = parsePostcode "CB3 0FA"
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
    | NearestInfoResponse (WebData NearestInfo)
    | ChangePostcode Postcode
    | GetLocationInfoFromWeb
    | GetNearestInfoFromWeb


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLocation path ->
            if path == "" || path == "/" then
                ( { model | route = HomeRoute }, Cmd.none )
            else
                let
                    validatedPostcode =
                        parsePostcode path
                in
                    case validatedPostcode of
                        Ok postcode ->
                            ( { model
                                | history = postcode :: model.history
                              }
                            , Navigation.newUrl postcode
                            )

                        Err e ->
                            ( { model | route = InvalidRoute e }, Cmd.none )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location

                --url will often have %20 in and this needs to be changed back to space
                decodedPostcodeUrl =
                    Maybe.withDefault "" <| decodeUri <| Maybe.withDefault "" (parsePath UrlParser.string location)
            in
                { model
                    | route = newRoute
                    , postcode = decodedPostcodeUrl
                    , validPostcode = parsePostcode decodedPostcodeUrl
                }
                    |> update GetLocationInfoFromWeb

        --web response
        LocationInfoResponse response ->
            { model | locationInfo = response }
                |> update GetNearestInfoFromWeb

        NearestInfoResponse response ->
            ( { model | nearestInfo = response }, Cmd.none )

        GetLocationInfoFromWeb ->
            case model.validPostcode of
                Ok postcode ->
                    ( model
                    , getLocationInfo model.postcode
                    )

                Err e ->
                    ( model, Cmd.none )

        {--It isn't actually necessary to make two web requests. The second one
        just gets all the information over again. I chose to do it this way
        as that's what the exercise requested, and because it illustrates how
        you might do two subsequent requests. The import bit is in the function
        page under PostcodeRoute. Here the two responses are combined.--}
        GetNearestInfoFromWeb ->
            case model.validPostcode of
                Ok postcode ->
                    ( model
                    , getNearestInfo model.postcode
                    )

                Err e ->
                    ( model, Cmd.none )

        --this could be used for autocomplete
        ChangePostcode postcode ->
            ( { model | postcode = postcode }, Cmd.none )



--web requests


getLocationInfo : String -> Cmd Msg
getLocationInfo postcode =
    Http.get
        ("http://api.postcodes.io/postcodes/"
            ++ postcode
        )
        decodeLocationInfo
        |> RemoteData.sendRequest
        |> Cmd.map LocationInfoResponse


getNearestInfo : String -> Cmd Msg
getNearestInfo postcode =
    Http.get
        ("http://api.postcodes.io/postcodes/"
            ++ postcode
            ++ "/nearest"
        )
        decodeNearestInfo
        |> RemoteData.sendRequest
        |> Cmd.map NearestInfoResponse



-- VIEWS


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ a [ href homePath, onLinkClick (ChangeLocation homePath) ] [ text "Home" ]
            , br [] []
            , br [] []
            , input [ type_ "text", placeholder "Postcode", onInput ChangePostcode, value model.postcode ] []
            , button [ onClick <| ChangeLocation model.postcode ] [ text "Get info" ]
            , br [] []
            , br [] []
            ]
        , page model --this bit depends on the current route
        , viewHistory model.history
        ]


page : Model -> Html Msg
page model =
    case model.route of
        HomeRoute ->
            div []
                [ text "Enter a postcode above"
                ]

        PostcodeRoute postcode ->
            let
                combinedResponse =
                    RemoteData.map2 (,) model.locationInfo model.nearestInfo
            in
                case combinedResponse of
                    NotAsked ->
                        text "Initialising."

                    Loading ->
                        div [] [ text "Loading." ]

                    Failure err ->
                        viewServerError err

                    Success combinedInfo ->
                        viewInfo combinedInfo

        --combined info is ( LocationInfo, NearestInfo )
        InvalidRoute e ->
            case (List.head e.context) of
                Just context ->
                    text <| "The " ++ context.description ++ " of your postcode is incorrect."

                Nothing ->
                    text "There is a problem with your postcode."

        NotFoundRoute ->
            text "Not Found"


viewInfo : ( LocationInfo, NearestInfo ) -> Html msg
viewInfo ( locationInfo, nearestInfo ) =
    div []
        [ text locationInfo.result.country
        , br [] []
        , text <| Maybe.withDefault "No region" locationInfo.result.region
        , br [] []
        , text <| "Nearest postcodes: " ++ String.join "; " (List.drop 1 <| List.map (\a -> a.postcode) nearestInfo.result)
        ]


viewServerError : Http.Error -> Html msg
viewServerError err =
    case err of
        Http.BadStatus response ->
            if response.status.code == 404 then
                div []
                    [ text ("Although valid, that postcode does not seem to exist.") ]
            else
                div []
                    [ text
                        ("There was a problem. The server said: "
                            ++ response.status.message
                        )
                    ]

        _ ->
            div [] [ text ("There was a problem contacting the server.") ]



-----


nav : String -> Html Msg
nav postcode =
    div []
        [ a [ href homePath, onLinkClick (ChangeLocation homePath) ] [ text "Home" ]
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


viewHistory : List String -> Html Msg
viewHistory history =
    ul [] (List.map historyLinks history)


historyLinks : String -> Html Msg
historyLinks address =
    li [] [ a [ href (address), onLinkClick (ChangeLocation (address)) ] [ text address ] ]



--subs


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
