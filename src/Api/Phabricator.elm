module Api.Phabricator exposing (..)

import Http exposing (stringBody)
import Json.Decode as D exposing (Decoder)
import Query exposing (joinUrlEncoded)


myPhabToken : String
myPhabToken =
    "api-xiun6obnzo5de5iehv5kxctypljz"


apiUrl : String
apiUrl =
    "https://cors-proxy-mhwanfbyyu.now.sh/https://phabricator.wikimedia.org/api/"


actions : { search : String }
actions =
    { search = "maniphest.search"
    }


searchById : List Int -> Http.Request (List PhabTask)
searchById ids =
    let
        idPair : Int -> Int -> ( String, String )
        idPair i id =
            ( "constraints[ids][" ++ (toString i) ++ "]", toString id )

        body =
            stringBody "application/x-www-form-urlencoded; charset=UTF-8" <|
                joinUrlEncoded
                    ([ ( "api.token", myPhabToken ) ] ++ List.indexedMap idPair ids)
    in
        Http.post (apiUrl ++ actions.search) body decodePhabTasks


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
