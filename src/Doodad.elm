module Doodad
    exposing
        ( Doodad
        , match
        , key
        , fetch
        , render
        , renderInText
        )

import Html exposing (Html, p)
import Doodads.Phabricator as Phab exposing (PhabDoodad)
import Doodads.Gerrit as Gerrit exposing (GerritDoodad)
import Regex exposing (Regex, HowMany(All), regex, escape, replace)


type Doodad
    = Phab PhabDoodad
    | Gerrit GerritDoodad


match : String -> List Doodad
match text =
    List.map Phab (Phab.match text)
        ++ List.map Gerrit (Gerrit.match text)


key : Doodad -> String
key doodad =
    case doodad of
        Phab d ->
            Phab.key d

        Gerrit d ->
            Gerrit.key d


fetch : (List Doodad -> msg) -> List Doodad -> ( List Doodad, Cmd msg )
fetch tagger doodads =
    let
        ( phabs, gerrits ) =
            List.foldl
                (\d ( phabs, gerrits ) ->
                    case d of
                        Phab p ->
                            ( p :: phabs, gerrits )

                        Gerrit g ->
                            ( phabs, g :: gerrits )
                )
                ( [], [] )
                doodads

        ( newPhabs, phabCmd ) =
            Phab.fetch (tagger << List.map Phab) phabs

        ( newGerrits, gerritCmd ) =
            Gerrit.fetch (tagger << List.map Gerrit) gerrits
    in
        (List.map Phab newPhabs
            ++ List.map Gerrit newGerrits
        )
            ! [ phabCmd, gerritCmd ]


renderInText : String -> Doodad -> String -> String
renderInText key doodad text =
    let
        rx =
            regex <| "\\b" ++ (escape key) ++ "\\b"

        renderText =
            (\_ ->
                case doodad of
                    Phab d ->
                        Phab.renderText d

                    Gerrit d ->
                        Gerrit.renderText d
            )
    in
        replace All rx renderText text


render : String -> Doodad -> Html msg
render key doodad =
    p []
        [ (case doodad of
            Phab data ->
                Phab.render data

            Gerrit data ->
                Gerrit.render data
          )
        ]
