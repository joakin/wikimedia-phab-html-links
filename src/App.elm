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
import Ports


type alias Flags =
    { text : String }


type alias Model =
    { rawText : String
    , urlEncodedText : String
    , state : Control.State Msg
    , doodads : Dict String Doodad
    , outputText : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { rawText = flags.text
    , urlEncodedText = textToURL flags.text
    , state = Control.initialState
    , doodads = Dict.empty
    , outputText = ""
    }
        ! [ if String.isEmpty flags.text then
                Cmd.none
            else
                Task.succeed () |> Task.perform (ProcessText flags.text)
          ]


type Msg
    = ChangeLinksText String
    | ProcessText String ()
    | Deb (Control Msg)
    | UpdateDoodads (List Doodad)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ rawText, urlEncodedText, doodads, state } as model) =
    case msg of
        ChangeLinksText txt ->
            model
                ! [ Task.succeed ()
                        |> Task.perform (debounce << (ProcessText txt))
                  ]

        ProcessText txt _ ->
            let
                ( newDoodads, cmds ) =
                    process txt doodads

                model_ =
                    { model
                        | rawText = txt
                        , urlEncodedText = textToURL txt
                        , doodads = newDoodads
                    }
            in
                ( model_
                , Cmd.batch
                    [ cmds
                    , Ports.replaceURL model_.urlEncodedText
                    ]
                )

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


placeholder_ : String
placeholder_ =
    """Write task numbers, gerrit change id or patch numbers here.

Examples:

    T12345

    I55d76b7db168f3745

    337837

You can also write markdown!

"""


view : Model -> Html Msg
view { rawText, urlEncodedText, doodads } =
    div
        [ class "app" ]
        [ div
            [ class "editor-wrapper" ]
            [ textarea
                [ class "editor"
                , onInput ChangeLinksText
                , placeholder placeholder_
                , defaultValue rawText
                ]
                []
            , a
                [ class "permalink"
                , href <| urlEncodedText
                ]
                [ text "permalink" ]
            ]
        , Markdown.toHtml
            [ class "document" ]
            (renderDoodadsInText doodads rawText)
        ]


textToURL : String -> String
textToURL txt =
    "#" ++ Http.encodeUri txt



--     (doodads
--         |> Dict.toList
--         |> List.map (\( k, v ) -> Doodad.render k v)
--     )
