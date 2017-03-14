module Doodads.Phabricator
    exposing
        ( PhabDoodad
        , match
        , key
        , fetch
        , render
        , renderText
        )

import Regex exposing (Regex, regex, find, Match, HowMany(All))
import RemoteData exposing (RemoteData(..))
import Http
import Api.Phabricator exposing (PhabTask, searchById)
import Html exposing (Html, a, text)
import Html.Attributes exposing (href)


-- Public API


type alias PhabDoodad =
    { match : Match
    , status : RemoteData String PhabTask
    }


match : String -> List PhabDoodad
match text =
    find All taskRegex text
        |> List.map fromMatch


key : PhabDoodad -> String
key =
    .match << .match


fetch : (List PhabDoodad -> msg) -> List PhabDoodad -> ( List PhabDoodad, Cmd msg )
fetch tagger doodads =
    let
        doodadsToFetch =
            doodads
                |> List.filter (\d -> d.status == NotAsked)
                |> List.map loadingStatus

        ids =
            getFetchIds doodadsToFetch
    in
        if List.isEmpty ids then
            ( doodadsToFetch, Cmd.none )
        else
            ( doodadsToFetch
            , searchById ids
                |> Http.send (tagger << (mapHttpResponse doodadsToFetch))
            )


render : PhabDoodad -> Html a
render doodad =
    case doodad.status of
        Success task ->
            a
                [ href <| "https://phabricator.wikimedia.org/T" ++ (toString task.id) ]
                [ text <| phabIdToString task.id ++ ": " ++ task.name ]

        Loading ->
            text <| (key doodad) ++ ": Loading data"

        NotAsked ->
            text <| (key doodad) ++ ": To be fetched"

        Failure err ->
            text <| (key doodad) ++ ": Error: " ++ err


renderText : PhabDoodad -> String
renderText doodad =
    case doodad.status of
        Success task ->
            let
                title =
                    phabIdToString task.id ++ ": " ++ task.name

                url =
                    "https://phabricator.wikimedia.org/" ++ (phabIdToString task.id)
            in
                "[" ++ title ++ "](" ++ url ++ ")"

        Loading ->
            (key doodad) ++ "(Loading data)"

        NotAsked ->
            (key doodad) ++ "(To be fetched)"

        Failure err ->
            (key doodad) ++ "(Error: " ++ err ++ ")"



-- Utility functions


taskRegex : Regex
taskRegex =
    regex "\\bT\\d+\\b"


fromMatch : Match -> PhabDoodad
fromMatch m =
    PhabDoodad m NotAsked


mapHttpResponse : List PhabDoodad -> Result Http.Error (List PhabTask) -> List PhabDoodad
mapHttpResponse originalDoodads res =
    case res of
        Ok tasks ->
            List.map (updateFromTasks tasks) originalDoodads

        Err err ->
            List.map (failureStatus "Query failed") originalDoodads


updateFromTasks : List PhabTask -> PhabDoodad -> PhabDoodad
updateFromTasks tasks doodad =
    case (taskForKey (key doodad) tasks) of
        Nothing ->
            failureStatus "Wasn't found when querying" doodad

        Just task ->
            successStatus task doodad


taskForKey : String -> List PhabTask -> Maybe PhabTask
taskForKey k tasks =
    tasks
        |> List.filter (\task -> phabIdToString task.id == k)
        |> List.head


successStatus : PhabTask -> PhabDoodad -> PhabDoodad
successStatus task doodad =
    { doodad | status = Success task }


failureStatus : String -> PhabDoodad -> PhabDoodad
failureStatus err doodad =
    { doodad | status = Failure err }


loadingStatus : PhabDoodad -> PhabDoodad
loadingStatus doodad =
    { doodad | status = Loading }


getFetchIds : List PhabDoodad -> List Int
getFetchIds doodads =
    doodads
        |> List.filterMap (getPhabId << key)


getPhabId : String -> Maybe Int
getPhabId phabId =
    phabId
        |> String.dropLeft 1
        |> String.toInt
        |> Result.toMaybe


phabIdToString : Int -> String
phabIdToString id =
    "T" ++ toString id
