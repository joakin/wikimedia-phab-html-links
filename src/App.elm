module App exposing (..)

import Html exposing (Html, text, div, textarea, p, a)
import Html.Attributes exposing (class, href, placeholder, defaultValue)
import Html.Events exposing (onInput)
import Http
import Time
import Control exposing (Control)
import Control.Debounce as Debounce
import Dict exposing (Dict)
import DictHelpers exposing (updateEmptyEntries, updateExistingEntries, keepOnlyKeys)
import Doodad exposing (Doodad)
import Markdown
import Task


type alias Flags =
    { text : String }


type alias Model =
    { rawText : String
    , state : Control.State Msg
    , doodads : Dict String Doodad
    , outputText : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { rawText = flags.text
    , state = Control.initialState
    , doodads = Dict.empty
    , outputText = ""
    }
        ! [ if String.isEmpty flags.text then
                Cmd.none
            else
                Task.succeed () |> Task.perform ProcessText
          ]


type Msg
    = ChangeLinksText String
    | ProcessText ()
    | Deb (Control Msg)
    | UpdateDoodads (List Doodad)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ rawText, doodads, state } as model) =
    case msg of
        ChangeLinksText txt ->
            { model | rawText = txt }
                ! [ Task.succeed ()
                        |> Task.perform (debounce << ProcessText)
                  ]

        ProcessText _ ->
            let
                ( newDoodads, cmds ) =
                    process rawText doodads

                model_ =
                    { model | doodads = newDoodads }
            in
                ( model_, cmds )

        Deb debMsg ->
            Control.update (\s -> { model | state = s }) state debMsg

        UpdateDoodads ds ->
            { model
                | doodads =
                    updateExistingEntries Doodad.key ds doodads
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
view { rawText, doodads } =
    div
        [ class "app" ]
        [ div
            [ class "editor-wrapper" ]
            [ textarea
                [ class "editor"
                , onInput ChangeLinksText
                , placeholder "Write task numbers or task links here, like T12345"
                , defaultValue rawText
                ]
                []
            , a
                [ class "permalink"
                , href <| "./?t=" ++ Http.encodeUri rawText
                ]
                [ text "permalink" ]
            ]
        , Markdown.toHtml
            [ class "document" ]
            (renderDoodadsInText doodads rawText)
        ]



--     (doodads
--         |> Dict.toList
--         |> List.map (\( k, v ) -> Doodad.render k v)
--     )
