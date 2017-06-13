module Api.Phabricator exposing (..)

import Http exposing (stringBody)
import Json.Decode as D exposing (Decoder)
import Query exposing (queryEscape)
import Api.Api exposing (apiUrl)


searchById : List Int -> Http.Request (List PhabTask)
searchById ids =
    let
        idsParam =
            ids
                |> List.map toString
                |> String.join "|"
                |> queryEscape
    in
        Http.get (apiUrl ++ "/phabricator?ids=" ++ idsParam) decodePhabTasks


type alias PhabTask =
    { id : Int
    , name : String
    , status : String
    }


decodePhabTask : Decoder PhabTask
decodePhabTask =
    D.map3 PhabTask
        (D.field "id" D.int)
        (D.at [ "fields", "name" ] D.string)
        (D.at [ "fields", "status", "name" ] D.string)


decodePhabTasks : Decoder (List PhabTask)
decodePhabTasks =
    (D.at [ "result", "data" ] (D.list decodePhabTask))
