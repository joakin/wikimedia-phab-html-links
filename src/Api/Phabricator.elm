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



-- {
--   "result": {
--     "data": [
--       {
--         "id": 321,
--         "type": "TASK",
--         "phid": "PHID-TASK-oq34j4ynau5g2cvb34cf",
--         "fields": {
--           "name": "Allow tracking bugs",
--           "authorPHID": "PHID-USER-nyhmztvvw4luz5mlcuvg",
--           "ownerPHID": "PHID-USER-hgn5uw2jafgjgfvxibhh",
--           "status": {
--             "value": "invalid",
--             "name": "Invalid",
--             "color": null
--           },
--           "priority": {
--             "value": 90,
--             "subpriority": 0.034384364493165,
--             "name": "Needs Triage",
--             "color": "violet"
--           },
--           "points": null,
--           "subtype": "default",
--           "spacePHID": null,
--           "dateCreated": 1406158292,
--           "dateModified": 1413674973,
--           "policy": {
--             "view": "public",
--             "interact": "users",
--             "edit": "users"
--           },
--           "custom.security_topic": null,
--           "custom.external_reference": "fl500"
--         },
--         "attachments": {}
--       },
--       {
--         "id": 123,
--         "type": "TASK",
--         "phid": "PHID-TASK-y2px7ogikirl56nqk7dk",
--         "fields": {
--           "name": "Turn on \"diffusion.allow-http-auth\"",
--           "authorPHID": "PHID-USER-pntojlcbclwhuip53bix",
--           "ownerPHID": "PHID-USER-lluzkul4z7us4sxkayss",
--           "status": {
--             "value": "declined",
--             "name": "Declined",
--             "color": null
--           },
--           "priority": {
--             "value": 50,
--             "subpriority": -0.087025037258797,
--             "name": "Normal",
--             "color": "orange"
--           },
--           "points": null,
--           "subtype": "default",
--           "spacePHID": null,
--           "dateCreated": 1398289650,
--           "dateModified": 1486670523,
--           "policy": {
--             "view": "public",
--             "interact": "users",
--             "edit": "users"
--           },
--           "custom.security_topic": null,
--           "custom.external_reference": "fl203"
--         },
--         "attachments": {}
--       }
--     ],
--     "maps": {},
--     "query": {
--       "queryKey": null
--     },
--     "cursor": {
--       "limit": 100,
--       "after": null,
--       "before": null,
--       "order": null
--     }
--   },
--   "error_code": null,
--   "error_info": null
-- }
