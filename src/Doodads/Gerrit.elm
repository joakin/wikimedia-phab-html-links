module Doodads.Gerrit
    exposing
        ( GerritDoodad
        , match
        , key
        , fetch
        , render
        , renderText
        )

import Regex exposing (Regex, regex, find, Match, HowMany(All))
import RemoteData exposing (RemoteData(..))
import Http
import Api.Gerrit as Api exposing (GerritPatch)
import Html exposing (Html, a, text)
import Html.Attributes exposing (href)
import Json.Decode as D


-- Public API


type GerritMatch
    = PatchId
    | ChangeId


type alias GerritDoodad =
    { kind : GerritMatch
    , match : Match
    , status : RemoteData String GerritPatch
    }


match : String -> List GerritDoodad
match text =
    find All patchRegex text
        |> List.map fromMatch


key : GerritDoodad -> String
key =
    .match << .match


fetch : (List GerritDoodad -> msg) -> List GerritDoodad -> ( List GerritDoodad, Cmd msg )
fetch tagger doodads =
    let
        doodadsToFetch =
            doodads
                |> List.filter (\d -> d.status == NotAsked)
                |> List.map loadingStatus
    in
        if List.isEmpty doodadsToFetch then
            ( doodadsToFetch, Cmd.none )
        else
            ( doodadsToFetch
            , doodadsToFetch
                |> List.map
                    (\d ->
                        d
                            |> fetchDoodad
                            |> Http.send (tagger << (mapHttpResponse d))
                    )
                |> Cmd.batch
            )


url : String -> String
url id =
    "https://gerrit.wikimedia.org/r/#/q/" ++ id


title : GerritPatch -> String
title patch =
    -- Use 7 digits on the changeid to not be re-matched by other regex
    -- (patchRegex matches 8)
    (String.left 7 patch.changeId)
        ++ " ["
        ++ String.left 1 patch.status
        ++ String.toLower (String.dropLeft 1 patch.status)
        ++ "]: "
        ++ patch.subject


render : GerritDoodad -> Html a
render doodad =
    case doodad.status of
        Success patch ->
            a
                [ href <| url patch.changeId ]
                [ text <| title patch ]

        Loading ->
            text <| (key doodad) ++ ": Loading data"

        NotAsked ->
            text <| (key doodad) ++ ": To be fetched"

        Failure err ->
            text <| (key doodad) ++ ": Error: " ++ err


renderText : GerritDoodad -> String
renderText doodad =
    case doodad.status of
        Success patch ->
            "[" ++ title patch ++ "](" ++ url patch.changeId ++ ")"

        Loading ->
            (key doodad) ++ "(Loading data)"

        NotAsked ->
            (key doodad) ++ "(To be fetched)"

        Failure err ->
            (key doodad) ++ "(Error: " ++ err ++ ")"



-- Utility functions


patchRegex : Regex
patchRegex =
    regex "\\b(I[0-9a-f]{8,})|(\\d{6})\\b"


fromMatch : Match -> GerritDoodad
fromMatch m =
    let
        kind =
            case String.left 1 m.match of
                "I" ->
                    ChangeId

                _ ->
                    PatchId
    in
        GerritDoodad kind m NotAsked


fetchDoodad : GerritDoodad -> Http.Request String
fetchDoodad doodad =
    case doodad.kind of
        PatchId ->
            Api.getId <| key doodad

        ChangeId ->
            Api.searchChangeId <| key doodad


mapHttpResponse : GerritDoodad -> Result error String -> List GerritDoodad
mapHttpResponse doodad res =
    [ case res of
        Ok str ->
            case doodad.kind of
                PatchId ->
                    case Api.decodeGerritPatchResponse Api.decodeGerritPatch str of
                        Ok patch ->
                            successStatus patch doodad

                        Err err ->
                            failureStatus "Error parsing" doodad

                ChangeId ->
                    case Api.decodeGerritPatchResponse (D.list Api.decodeGerritPatch) str of
                        Ok [] ->
                            failureStatus "Not found" doodad

                        Ok (p :: ps) ->
                            successStatus p doodad

                        Err err ->
                            failureStatus "Error parsing" doodad

        Err err ->
            failureStatus "Request failed" doodad
    ]


successStatus : GerritPatch -> GerritDoodad -> GerritDoodad
successStatus patch doodad =
    { doodad | status = Success patch }


failureStatus : String -> GerritDoodad -> GerritDoodad
failureStatus err doodad =
    { doodad | status = Failure err }


loadingStatus : GerritDoodad -> GerritDoodad
loadingStatus doodad =
    { doodad | status = Loading }
