module Api.Gerrit exposing (..)

import Http exposing (stringBody)
import Json.Decode as D exposing (Decoder, decodeString)
import Query exposing (queryEscape)
import Api.Api exposing (apiUrl)


path : String
path =
    "/gerrit"


byIdUrl : String -> String
byIdUrl id =
    apiUrl ++ path ++ "?id=" ++ queryEscape id


byChangeIdUrl : String -> String
byChangeIdUrl changeId =
    apiUrl ++ path ++ "?changeId=" ++ queryEscape changeId


getId : String -> Http.Request String
getId id =
    Http.getString <| byIdUrl id


searchChangeId : String -> Http.Request String
searchChangeId changeId =
    Http.getString <| byChangeIdUrl changeId


type alias GerritPatch =
    { id : String
    , project : String
    , branch : String
    , changeId : String
    , subject : String
    , status : String
    , created : String
    , updated : String
    }


decodeGerritPatchResponse : Decoder a -> String -> Result String a
decodeGerritPatchResponse decoder res =
    res
        |> String.dropLeft 5
        |> decodeString decoder


decodeGerritPatch : Decoder GerritPatch
decodeGerritPatch =
    D.map8 GerritPatch
        (D.field "id" D.string)
        (D.field "project" D.string)
        (D.field "branch" D.string)
        (D.field "change_id" D.string)
        (D.field "subject" D.string)
        (D.field "status" D.string)
        (D.field "created" D.string)
        (D.field "updated" D.string)
