module Types exposing (..)

import RemoteData exposing (WebData, RemoteData(..))
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, required)


--model


type alias Model =
    { route : Route
    , history : List String
    , locationInfo : WebData LocationInfo
    , postcode : String
    }


type alias Postcode =
    String



--route type


type Route
    = HomeRoute
    | PostcodeRoute String
    | NotFoundRoute



--web data


type alias LocationInfo =
    { status : Int
    , result : LocationInfoResult
    }


type alias LocationInfoResult =
    { postcode : String
    , country : String
    , region : Maybe String
    }



--decoders


decodeLocationInfo : Json.Decode.Decoder LocationInfo
decodeLocationInfo =
    Json.Decode.Pipeline.decode LocationInfo
        |> Json.Decode.Pipeline.required "status" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "result" (decodeLocationInfoResult)


decodeLocationInfoResult : Json.Decode.Decoder LocationInfoResult
decodeLocationInfoResult =
    Json.Decode.Pipeline.decode LocationInfoResult
        |> Json.Decode.Pipeline.required "postcode" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "country" (Json.Decode.string)
        |> Json.Decode.Pipeline.required "region" (Json.Decode.nullable string)
