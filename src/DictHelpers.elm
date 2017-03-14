module DictHelpers exposing (updateEmptyEntries, updateExistingEntries, keepOnlyKeys)

import Dict exposing (Dict)


updateEmptyEntries : (entry -> comparable) -> List entry -> Dict comparable entry -> Dict comparable entry
updateEmptyEntries key items dict =
    -- Update new not-processed items into the dictionary
    List.foldl
        (\item dict ->
            Dict.update (key item) (mapNothing <| item) dict
        )
        dict
        items


updateExistingEntries : (entry -> comparable) -> List entry -> Dict comparable entry -> Dict comparable entry
updateExistingEntries key items dict =
    -- Update only existing items in the dictionary
    List.foldl
        (\entry dict ->
            Dict.update (key entry) (Maybe.map (\_ -> entry)) dict
        )
        dict
        items


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
