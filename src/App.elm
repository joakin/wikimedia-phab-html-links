module App exposing (..)

import Html exposing (Html, text, div, textarea, p, a)
import Html.Attributes exposing (map, style, href, placeholder)
import Html.Events exposing (onInput)
import Http
import Time
import Control exposing (Control)
import Control.Debounce as Debounce
import Dict exposing (Dict)
import Regex exposing (Regex, regex, find, HowMany(All))
import RemoteData exposing (RemoteData(..))
import Api.Phabricator as Phab exposing (PhabTask)


type alias Flags =
    { noop : String }


type Doodad
    = Phab (RemoteData String PhabTask)


type alias Model =
    { phabricatorToken : String
    , linksText : String
    , state : Control.State Msg
    , doodads : Dict String Doodad
    , outputText : String
    }


init : Flags -> ( Model, Cmd Msg )
init path =
    { phabricatorToken = Phab.myPhabToken
    , linksText = ""
    , state = Control.initialState
    , doodads = Dict.empty
    , outputText = ""
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
                        Http.send SearchResponse <| Phab.searchById model.phabricatorToken listIds
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


taskRegex : Regex
taskRegex =
    regex "T\\d+"


updateTaskLinksFromLinksText : Model -> Model
updateTaskLinksFromLinksText model =
    let
        matches =
            List.map .match <| find All taskRegex model.linksText

        activeTaskLinks =
            keepOnlyKeys matches model.doodads

        doodads =
            List.foldl (\match dict -> Dict.update match (mapNothing <| Phab NotAsked) dict)
                activeTaskLinks
                matches
    in
        { model | doodads = doodads }


keepOnlyKeys : List String -> Dict String a -> Dict String a
keepOnlyKeys keys dict =
    Dict.filter (\key _ -> List.member key keys) dict


mapNothing : a -> Maybe a -> Maybe a
mapNothing value entry =
    case entry of
        Just _ ->
            entry

        Nothing ->
            Just value


updateNotAskedAndExtractIds : Model -> ( Model, List Int )
updateNotAskedAndExtractIds ({ doodads } as model) =
    let
        doodadsList =
            Dict.toList doodads

        ( tl, ids ) =
            List.foldl updateAndGetId ( doodads, [] ) doodadsList
    in
        ( { model | doodads = tl }, ids )


updateAndGetId :
    ( String, Doodad )
    -> ( Dict String Doodad, List Int )
    -> ( Dict String Doodad, List Int )
updateAndGetId ( key, doodad ) ( dict, ids ) =
    case doodad of
        Phab data ->
            -- Set NotAsked as Loading and collect ids
            case data of
                NotAsked ->
                    case getPhabId key of
                        Ok num ->
                            ( Dict.insert key (Phab Loading) dict, ids ++ [ num ] )

                        Err str ->
                            ( Dict.insert key (Phab (Failure str)) dict, ids )

                _ ->
                    ( dict, ids )


updateReceivedTasks : List PhabTask -> Model -> Model
updateReceivedTasks tasks ({ doodads } as model) =
    { model | doodads = List.foldl insertTask doodads tasks }


insertTask : PhabTask -> Dict String Doodad -> Dict String Doodad
insertTask task dict =
    Dict.insert ("T" ++ toString task.id) (Phab (RemoteData.succeed task)) dict


getPhabId : String -> Result String Int
getPhabId phabId =
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
            (model.doodads
                |> Dict.toList
                |> List.map (\( k, v ) -> viewDoodad k v)
            )
        ]


viewDoodad : String -> Doodad -> Html Msg
viewDoodad id doodad =
    p []
        [ (case doodad of
            Phab data ->
                case data of
                    Success task ->
                        a
                            [ href <| "https://phabricator.wikimedia.org/T" ++ (toString task.id) ]
                            [ text <| "T" ++ (toString task.id) ++ ": " ++ task.name ]

                    Loading ->
                        text <| id ++ ": Loading data"

                    _ ->
                        text id
          )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
