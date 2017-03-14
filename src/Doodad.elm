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
import Regex exposing (Regex, HowMany(All), regex, escape, replace)


type Doodad
    = Phab PhabDoodad


match : String -> List Doodad
match text =
    List.map Phab (Phab.match text)


key : Doodad -> String
key doodad =
    case doodad of
        Phab d ->
            Phab.key d


fetch : (List Doodad -> msg) -> List Doodad -> ( List Doodad, Cmd msg )
fetch tagger doodads =
    let
        ( phabs, _ ) =
            List.foldl
                (\d ( phabs, _ ) ->
                    case d of
                        Phab p ->
                            ( p :: phabs, Nothing )
                )
                ( [], Nothing )
                doodads

        ( newPhabs, phabCmds ) =
            Phab.fetch (tagger << List.map Phab) phabs
    in
        (List.map Phab newPhabs) ! [ phabCmds ]


renderInText : String -> Doodad -> String -> String
renderInText key doodad text =
    let
        rx =
            regex (escape key)

        renderText =
            (\_ ->
                case doodad of
                    Phab d ->
                        Phab.renderText d
            )
    in
        replace All rx renderText text


render : String -> Doodad -> Html msg
render key doodad =
    p []
        [ (case doodad of
            Phab data ->
                Phab.render data
          )
        ]
