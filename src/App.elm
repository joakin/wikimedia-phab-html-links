module App exposing (..)

import Html exposing (Html, text, div, textarea, p, a)
import Html.Attributes exposing (map, style, href, placeholder)
import Html.Events exposing (onInput)
import Time
import Control exposing (Control)
import Control.Debounce as Debounce
import Dict exposing (Dict)
import Doodads.Phabricator as Phab exposing (PhabDoodad)


type alias Flags =
    { noop : String }


type Doodad
    = Phab PhabDoodad


type alias Model =
    { linksText : String
    , state : Control.State Msg
    , doodads : Dict String Doodad
    , outputText : String
    }


init : Flags -> ( Model, Cmd Msg )
init path =
    { linksText = ""
    , state = Control.initialState
    , doodads = Dict.empty
    , outputText = ""
    }
        ! []


type Msg
    = ChangeLinksText String
    | Deb (Control Msg)
    | UpdateDoodads (List Doodad)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLinksText txt ->
            let
                ( doodads, cmds ) =
                    process txt model.doodads

                model_ =
                    { model
                        | linksText = txt
                        , doodads = doodads
                    }
            in
                ( model_, cmds )

        Deb debMsg ->
            Control.update (\s -> { model | state = s }) model.state debMsg

        UpdateDoodads ds ->
            { model | doodads = updateExistingEntries ds model.doodads } ! []


process : String -> Dict String Doodad -> ( Dict String Doodad, Cmd Msg )
process text doodads =
    let
        matches =
            match text

        -- Filter and keep only active doodads in dict
        activeDoodads =
            keepOnlyKeys (List.map key matches) doodads

        -- Update new not-processed doodads in the dictionary
        newDoodads =
            updateEmptyEntries matches activeDoodads

        ( updatedDoodadsList, cmd ) =
            fetch UpdateDoodads (Dict.values newDoodads)

        processedDoodads =
            updateExistingEntries updatedDoodadsList newDoodads
    in
        ( processedDoodads, cmd )


match : String -> List Doodad
match text =
    List.map Phab (Phab.match text)


fetch : (List Doodad -> Msg) -> List Doodad -> ( List Doodad, Cmd Msg )
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
            Phab.fetch (UpdateDoodads << List.map Phab) phabs
    in
        (List.map Phab newPhabs) ! [ phabCmds ]


key : Doodad -> String
key doodad =
    case doodad of
        Phab d ->
            Phab.key d


updateEmptyEntries : List Doodad -> Dict String Doodad -> Dict String Doodad
updateEmptyEntries doodads dict =
    -- Update new not-processed doodads into the dictionary
    List.foldl
        (\doodad dict ->
            Dict.update (key doodad) (mapNothing <| doodad) dict
        )
        dict
        doodads


updateExistingEntries : List Doodad -> Dict String Doodad -> Dict String Doodad
updateExistingEntries doodads dict =
    -- Update only existing doodads in the dictionary
    List.foldl
        (\doodad dict ->
            Dict.update (key doodad) (Maybe.map (\_ -> doodad)) dict
        )
        dict
        doodads


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
                Phab.render data
          )
        ]
