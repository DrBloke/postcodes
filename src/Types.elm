module Types exposing (..)

import RemoteData exposing (WebData, RemoteData(..))
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (decode, required)
import Parser exposing (..)
import Result


--model


type alias Model =
    { route : Route
    , history : List String
    , locationInfo : WebData LocationInfo
    , nearestInfo : WebData NearestInfo
    , postcode : Postcode
    , validPostcode : Result Error Postcode
    }


type alias Postcode =
    String



--route type


type Route
    = HomeRoute
    | PostcodeRoute String
    | InvalidRoute Error
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


type alias NearestInfo =
    { status : Int
    , result : List NearestInfoResult
    }


type alias NearestInfoResult =
    { postcode : String
    }



--decoders
--location info


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



---nearest


decodeNearestInfo : Json.Decode.Decoder NearestInfo
decodeNearestInfo =
    Json.Decode.Pipeline.decode NearestInfo
        |> Json.Decode.Pipeline.required "status" (Json.Decode.int)
        |> Json.Decode.Pipeline.required "result" (Json.Decode.list decodeNearestInfoResult)


decodeNearestInfoResult : Json.Decode.Decoder NearestInfoResult
decodeNearestInfoResult =
    Json.Decode.Pipeline.decode NearestInfoResult
        |> Json.Decode.Pipeline.required "postcode" (Json.Decode.string)
