module App exposing (..)

import Html exposing (Html, text, div, textarea, p, a)
import Html.Attributes exposing (map, style, href, placeholder)
import Html.Events exposing (onInput)
import Time
import Control exposing (Control)
import Control.Debounce as Debounce
import Dict exposing (Dict)
import Regex exposing (Regex, regex, find, HowMany(All))
import RemoteData exposing (RemoteData(..))
import Http exposing (stringBody)
import Json.Decode as D exposing (Decoder)


type alias Flags =
    { noop : String }


type alias Model =
    { token : String
    , linksText : String
    , state : Control.State Msg
    , taskLinks : Dict String (RemoteData String PhabTask)
    }


myToken : String
myToken =
    "api-xiun6obnzo5de5iehv5kxctypljz"


apiUrl : String
apiUrl =
    "https://cors-proxy-mhwanfbyyu.now.sh/https://phabricator.wikimedia.org/api/maniphest.search"


init : Flags -> ( Model, Cmd Msg )
init path =
    { token = myToken
    , linksText = ""
    , state = Control.initialState
    , taskLinks = Dict.empty
    }
        ! []


type Msg
    = ChangeLinksText String
    | Deb (Control Msg)
    | SearchResponse (Result Http.Error (List PhabTask))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLinksText txt ->
            let
                ( newModel, listIds ) =
                    model
                        |> (\m -> { m | linksText = txt })
                        |> updateTaskLinksFromLinksText
                        |> updateNotAskedAndExtractIds

                cmds =
                    if List.isEmpty listIds then
                        Cmd.none
                    else
                        Http.send SearchResponse <| getPhabricatorIdsInfo model.token listIds
            in
                ( newModel, cmds )

        Deb debMsg ->
            Control.update (\s -> { model | state = s }) model.state debMsg

        SearchResponse (Ok tasks) ->
            ( updateReceivedTasks tasks model, Cmd.none )

        SearchResponse (Err err) ->
            let
                _ =
                    Debug.log "Http error: " err
            in
                ( model, Cmd.none )


getPhabricatorIdsInfo : String -> List Int -> Http.Request (List PhabTask)
getPhabricatorIdsInfo token ids =
    let
        idPair : Int -> Int -> ( String, String )
        idPair i id =
            ( "constraints[ids][" ++ (toString i) ++ "]", toString id )

        body =
            stringBody "application/x-www-form-urlencoded; charset=UTF-8" <|
                joinUrlEncoded
                    ([ ( "api.token", token ) ] ++ List.indexedMap idPair ids)
    in
        Http.post apiUrl body decodePhabTasks


taskRegex : Regex
taskRegex =
    regex "T\\d+"


updateTaskLinksFromLinksText : Model -> Model
updateTaskLinksFromLinksText model =
    let
        matches =
            List.map .match <| find All taskRegex model.linksText

        filteredTaskLinks =
            Dict.filter (\k v -> List.member k matches) model.taskLinks

        taskLinks =
            List.foldl (\m d -> Dict.update m (updateIfNothing NotAsked) d)
                filteredTaskLinks
                matches
    in
        { model | taskLinks = taskLinks }


updateIfNothing : RemoteData e a -> Maybe (RemoteData e a) -> Maybe (RemoteData e a)
updateIfNothing update entry =
    case entry of
        Just _ ->
            entry

        Nothing ->
            Just update


updateNotAskedAndExtractIds : Model -> ( Model, List Int )
updateNotAskedAndExtractIds ({ taskLinks } as model) =
    let
        taskLinksList : List ( String, RemoteData String PhabTask )
        taskLinksList =
            Dict.toList taskLinks

        ( tl, ids ) =
            List.foldl updateAndGetId ( taskLinks, [] ) taskLinksList
    in
        ( { model | taskLinks = tl }, ids )


updateAndGetId :
    ( String, RemoteData String PhabTask )
    -> ( Dict String (RemoteData String PhabTask), List Int )
    -> ( Dict String (RemoteData String PhabTask), List Int )
updateAndGetId ( key, data ) ( dict, ids ) =
    -- Check if data is NotAsked
    case data of
        NotAsked ->
            case getId key of
                Ok num ->
                    ( Dict.insert key Loading dict, ids ++ [ num ] )

                Err str ->
                    ( Dict.insert key (Failure str) dict, ids )

        _ ->
            ( dict, ids )


updateReceivedTasks : List PhabTask -> Model -> Model
updateReceivedTasks tasks ({ taskLinks } as model) =
    let
        newTaskLinks =
            List.foldl
                (\t d ->
                    Dict.insert ("T" ++ toString t.id) (RemoteData.succeed t) d
                )
                taskLinks
                tasks
    in
        { model | taskLinks = newTaskLinks }


getId : String -> Result String Int
getId phabId =
    phabId |> String.dropLeft 1 |> String.toInt


debounce : Msg -> Msg
debounce =
    Debounce.trailing Deb (0.5 * Time.second)


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "min-height", "100%" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            , ( "justify-content", "center" )
            ]
        ]
        [ textarea
            [ style
                [ ( "flex", "1" )
                , ( "width", "90%" )
                , ( "display", "block" )
                , ( "padding", "1em" )
                , ( "box-sizing", "border-box" )
                ]
            , map debounce <| onInput ChangeLinksText
            , placeholder "Write task numbers or task links here, like T12345"
            ]
            [ text model.linksText
            ]
        , div
            [ style
                [ ( "flex", "1" )
                , ( "box-sizing", "border-box" )
                , ( "padding", "1em" )
                , ( "width", "90%" )
                ]
            ]
            (model.taskLinks
                |> Dict.toList
                |> List.map (\( k, v ) -> taskLink k v)
            )
        ]


taskLink : String -> RemoteData String PhabTask -> Html Msg
taskLink id data =
    p []
        [ (case data of
            Success task ->
                a
                    [ href <| "https://phabricator.wikimedia.org/T" ++ (toString task.id) ]
                    [ text <| "T" ++ (toString task.id) ++ ": " ++ task.name ]

            Loading ->
                text <| id ++ ": Loading data"

            _ ->
                text ""
          )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


joinUrlEncoded : List ( String, String ) -> String
joinUrlEncoded args =
    String.join "&" (List.map queryPair args)


queryPair : ( String, String ) -> String
queryPair ( key, value ) =
    queryEscape key ++ "=" ++ queryEscape value


queryEscape : String -> String
queryEscape =
    Http.encodeUri >> replace "%20" "+"


replace : String -> String -> String -> String
replace old new =
    String.split old >> String.join new


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
