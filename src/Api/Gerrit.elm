module Api.Gerrit exposing (..)

import Http exposing (stringBody)
import Json.Decode as D exposing (Decoder, decodeString)
import Query exposing (joinUrlEncoded)


apiUrl : String
apiUrl =
    "https://cors-proxy-mhwanfbyyu.now.sh/https://gerrit.wikimedia.org/r/changes/"


byIdUrl : String -> String
byIdUrl id =
    apiUrl ++ id


byChangeIdUrl : String -> String
byChangeIdUrl changeId =
    apiUrl ++ "?" ++ joinUrlEncoded [ ( "q", changeId ) ]


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
    , topic : String
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
        (D.field "topic" D.string)
        (D.field "change_id" D.string)
        (D.field "subject" D.string)
        (D.field "status" D.string)
        (D.field "created" D.string)
        |> D.andThen
            (\f ->
                D.map f
                    (D.field "updated" D.string)
            )
