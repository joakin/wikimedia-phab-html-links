module App exposing (..)

import Html exposing (Html, text, div, textarea, p, a)
import Html.Attributes exposing (map, style, href, placeholder)
import Html.Events exposing (onInput)
import Time
import Control exposing (Control)
import Control.Debounce as Debounce
import Dict exposing (Dict)
import DictHelpers exposing (updateEmptyEntries, updateExistingEntries, keepOnlyKeys)
import Doodad exposing (Doodad)
import Markdown
import Task


type alias Flags =
    { noop : String }


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
    | ProcessText ()
    | Deb (Control Msg)
    | UpdateDoodads (List Doodad)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLinksText txt ->
            { model | linksText = txt }
                ! [ Task.succeed ()
                        |> Task.perform (debounce << ProcessText)
                  ]

        ProcessText _ ->
            let
                ( doodads, cmds ) =
                    process model.linksText model.doodads

                model_ =
                    { model | doodads = doodads }
            in
                ( model_, cmds )

        Deb debMsg ->
            Control.update (\s -> { model | state = s }) model.state debMsg

        UpdateDoodads ds ->
            { model
                | doodads =
                    updateExistingEntries Doodad.key ds model.doodads
            }
                ! []


process : String -> Dict String Doodad -> ( Dict String Doodad, Cmd Msg )
process text doodads =
    let
        -- Find doodad matches from text
        matches =
            Doodad.match text

        -- Filter and keep only active doodads in dict
        activeDoodads =
            keepOnlyKeys (List.map Doodad.key matches) doodads

        -- Update new not-processed doodads in the dictionary
        newDoodads =
            updateEmptyEntries Doodad.key matches activeDoodads

        -- Trigger data processing/fetching and get updated doodads list
        ( updatedDoodadsList, cmd ) =
            Doodad.fetch UpdateDoodads (Dict.values newDoodads)

        -- Update processed doodads in the dictionary
        processedDoodads =
            updateExistingEntries Doodad.key updatedDoodadsList newDoodads
    in
        ( processedDoodads, cmd )


renderDoodadsInText : Dict String Doodad -> String -> String
renderDoodadsInText doodads text =
    Dict.foldl Doodad.renderInText text doodads


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
                [ ( "flex", "2 2 auto" )
                , ( "width", "90%" )
                , ( "display", "block" )
                , ( "padding", "1em" )
                , ( "box-sizing", "border-box" )
                , ( "overflow", "auto" )
                ]
            , onInput ChangeLinksText
            , placeholder "Write task numbers or task links here, like T12345"
            ]
            [ text model.linksText
            ]
          -- , div
          --     [ style
          --         [ ( "flex", "1 1 auto" )
          --         , ( "box-sizing", "border-box" )
          --         , ( "padding", "1em" )
          --         , ( "width", "90%" )
          --         , ( "background-color", "#fafafa" )
          --         , ( "overflow", "auto" )
          --         ]
          --     ]
          --     (model.doodads
          --         |> Dict.toList
          --         |> List.map (\( k, v ) -> Doodad.render k v)
          --     )
        , Markdown.toHtml
            [ style
                [ ( "flex", "3 3 auto" )
                , ( "box-sizing", "border-box" )
                , ( "padding", "1em" )
                , ( "width", "90%" )
                , ( "background-color", "#f3f3f3" )
                , ( "overflow", "auto" )
                ]
            ]
            (renderDoodadsInText model.doodads model.linksText)
        ]
