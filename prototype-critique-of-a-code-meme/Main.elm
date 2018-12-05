module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, button, div, li, p, span, text, ul)
import Html.Events exposing (onClick)
import Http
import Json.Decode


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Model


type alias Model =
    { persons : Cache Http.Error PersonCollection
    }


type alias PersonCollection =
    Collection PersonCache


type alias Collection a =
    { entities : Entities a
    , displayOrder : List String
    }


type alias PersonEntities =
    Entities PersonCache


type alias Entities a =
    Dict String a


type alias PersonCache =
    Cache Http.Error Person


type alias Person =
    { name : String
    , url : String
    , hairColor : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { persons = Empty }, Cmd.none )


decodeHairColor : Json.Decode.Decoder (Maybe String)
decodeHairColor =
    Json.Decode.field "hair_color" Json.Decode.string
        |> Json.Decode.map Just


decodeName : Json.Decode.Decoder String
decodeName =
    Json.Decode.field "name" Json.Decode.string


decodeUrl : Json.Decode.Decoder String
decodeUrl =
    Json.Decode.field "url" Json.Decode.string


decodePersonWithoutHairColor : Json.Decode.Decoder Person
decodePersonWithoutHairColor =
    Json.Decode.map3
        Person
        decodeName
        decodeUrl
        (Json.Decode.succeed Nothing)


decodePersonWithHairColor : Json.Decode.Decoder Person
decodePersonWithHairColor =
    Json.Decode.map3
        Person
        decodeName
        decodeUrl
        decodeHairColor


decodePersonCollection : Json.Decode.Decoder (List Person)
decodePersonCollection =
    Json.Decode.field "results" (Json.Decode.list decodePersonWithoutHairColor)


decodeError : Json.Decode.Decoder String
decodeError =
    Json.Decode.field "detail" Json.Decode.string


updateEmptyPersonCollection : List Person -> PersonCollection
updateEmptyPersonCollection persons =
    { entities = createEntities persons
    , displayOrder = toDisplayOrder persons
    }


updateFilledPersonCollection : List Person -> PersonCollection -> PersonCollection
updateFilledPersonCollection persons current =
    { entities = updateEntities persons current.entities
    , displayOrder = toDisplayOrder persons
    }


patchFilledPersonCollection : String -> CacheEvent Http.Error Person a -> PersonCollection -> PersonCollection
patchFilledPersonCollection url cacheEvent personCollection =
    let
        entities =
            personCollection.entities
    in
        { personCollection
            | entities =
                Dict.get url entities
                    |> Maybe.map (updatePersonCache url cacheEvent)
                    |> Maybe.map (flippedDictInsert url entities)
                    |> Maybe.withDefault entities
        }


flippedDictInsert : String -> Dict String a -> a -> Dict String a
flippedDictInsert key =
    flip (Dict.insert key)


updatePersonCache : String -> CacheEvent Http.Error Person a -> PersonCache -> PersonCache
updatePersonCache url cacheEvent currentCache =
    updateCache
        { updateEmpty = identity
        , updateFilled = identity2
        , patchFilled = identity2
        }
        cacheEvent
        currentCache


toDisplayOrder : List Person -> List String
toDisplayOrder =
    List.map .url


createEntities : List { a | url : String } -> Dict String (Cache b { a | url : String })
createEntities =
    toUrlDict >> toFilledCache


toUrlDict : List { a | url : String } -> Dict String { a | url : String }
toUrlDict =
    List.map keyByUrl
        >> Dict.fromList


toFilledCache : Dict String a -> Dict String (Cache b a)
toFilledCache =
    Dict.map (\k v -> Filled v)


updateEntities : List Person -> PersonEntities -> PersonEntities
updateEntities updates currentEntities =
    let
        getCurrentPersonCache =
            flip Dict.get currentEntities
                >> Maybe.withDefault Empty
    in
        toUrlDict updates
            |> Dict.map
                (\url personUpdate ->
                    updateCache
                        { updateEmpty = identity
                        , updateFilled = identity2
                        , patchFilled = identity2
                        }
                        (Update personUpdate)
                        (getCurrentPersonCache url)
                )
            |> replaceCurrentWithUpdate currentEntities


replaceCurrentWithUpdate : Dict String a -> Dict String a -> Dict String a
replaceCurrentWithUpdate x y =
    Dict.merge mergeOnlyInLeft mergeInBoth mergeOnlyInRight x y Dict.empty


mergeOnlyInLeft : String -> a -> Dict String a -> Dict String a
mergeOnlyInLeft =
    Dict.insert


mergeOnlyInRight : String -> a -> Dict String a -> Dict String a
mergeOnlyInRight =
    Dict.insert


mergeInBoth : String -> a -> b -> Dict String b -> Dict String b
mergeInBoth key left right result =
    Dict.insert key right result



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Update


type Msg
    = CollectionErrorRequest
    | CollectionErrorResponse (Result Http.Error (List Person))
    | CollectionRequest
    | CollectionResponse (Result Http.Error (List Person))
    | ItemRequest String
    | ItemResponse String (Result Http.Error Person)
    | ItemErrorRequest String
    | ItemErrorResponse String (Result Http.Error Person)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CollectionRequest ->
            ( { model
                | persons =
                    updateCache
                        { updateEmpty = updateEmptyPersonCollection
                        , updateFilled = updateFilledPersonCollection
                        , patchFilled = identity2
                        }
                        Sync
                        model.persons
              }
            , getCollection
            )

        CollectionResponse (Err error) ->
            ( { model
                | persons =
                    updateCache
                        { updateEmpty = updateEmptyPersonCollection
                        , updateFilled = updateFilledPersonCollection
                        , patchFilled = identity2
                        }
                        (Error error)
                        model.persons
              }
            , Cmd.none
            )

        CollectionResponse (Ok persons) ->
            ( { model
                | persons =
                    updateCache
                        { updateEmpty = updateEmptyPersonCollection
                        , updateFilled = updateFilledPersonCollection
                        , patchFilled = identity2
                        }
                        (Update persons)
                        model.persons
              }
            , Cmd.none
            )

        CollectionErrorRequest ->
            ( { model
                | persons =
                    updateCache
                        { updateEmpty = updateEmptyPersonCollection
                        , updateFilled = updateFilledPersonCollection
                        , patchFilled = identity2
                        }
                        Sync
                        model.persons
              }
            , getCollectionError
            )

        CollectionErrorResponse (Ok persons) ->
            ( { model
                | persons =
                    updateCache
                        { updateEmpty = updateEmptyPersonCollection
                        , updateFilled = updateFilledPersonCollection
                        , patchFilled = identity2
                        }
                        (Update persons)
                        model.persons
              }
            , Cmd.none
            )

        CollectionErrorResponse (Err error) ->
            ( { model
                | persons =
                    updateCache
                        { updateEmpty = updateEmptyPersonCollection
                        , updateFilled = updateFilledPersonCollection
                        , patchFilled = identity2
                        }
                        (Error error)
                        model.persons
              }
            , Cmd.none
            )

        ItemRequest url ->
            ( { model
                | persons =
                    updateCache
                        { updateEmpty = updateEmptyPersonCollection
                        , updateFilled = identity2
                        , patchFilled = patchFilledPersonCollection url
                        }
                        (Patch Sync)
                        model.persons
              }
            , getItem url
            )

        ItemResponse url (Ok person) ->
            ( { model
                | persons =
                    updateCache
                        { updateEmpty = updateEmptyPersonCollection
                        , updateFilled = identity2
                        , patchFilled = patchFilledPersonCollection url
                        }
                        (Patch (Update person))
                        model.persons
              }
            , Cmd.none
            )

        ItemResponse url (Err error) ->
            ( { model
                | persons =
                    updateCache
                        { updateEmpty = updateEmptyPersonCollection
                        , updateFilled = identity2
                        , patchFilled = patchFilledPersonCollection url
                        }
                        (Patch (Error error))
                        model.persons
              }
            , Cmd.none
            )

        ItemErrorRequest url ->
            ( { model
                | persons =
                    updateCache
                        { updateEmpty = updateEmptyPersonCollection
                        , updateFilled = identity2
                        , patchFilled = patchFilledPersonCollection url
                        }
                        (Patch Sync)
                        model.persons
              }
            , getItemError url
            )

        ItemErrorResponse url (Ok person) ->
            ( { model
                | persons =
                    updateCache
                        { updateEmpty = updateEmptyPersonCollection
                        , updateFilled = identity2
                        , patchFilled = patchFilledPersonCollection url
                        }
                        (Patch (Update person))
                        model.persons
              }
            , Cmd.none
            )

        ItemErrorResponse url (Err error) ->
            ( { model
                | persons =
                    updateCache
                        { updateEmpty = updateEmptyPersonCollection
                        , updateFilled = identity2
                        , patchFilled = patchFilledPersonCollection url
                        }
                        (Patch (Error error))
                        model.persons
              }
            , Cmd.none
            )



-- View


type Visibility a
    = Show a
    | Hide


view : Model -> Html Msg
view model =
    div
        []
        [ button
            [ onClick CollectionRequest
            ]
            [ text "Get data"
            ]
        , button
            [ onClick CollectionErrorRequest
            ]
            [ text "Get error"
            ]
        , loadingView model.persons
        , errorView model.persons
        , listView model.persons
        ]


visibilityToHtml : (a -> Html b) -> Visibility a -> Html b
visibilityToHtml toHtml visibility =
    case visibility of
        Show x ->
            toHtml x

        Hide ->
            text ""


buttonVisibility : PersonCache -> Visibility Person
buttonVisibility cache =
    case cache of
        Empty ->
            Hide

        EmptyInvalid _ ->
            Hide

        EmptyInvalidSyncing _ ->
            Hide

        EmptySyncing ->
            Hide

        Filled p ->
            Show p

        FilledInvalid _ p ->
            Show p

        FilledInvalidSyncing _ p ->
            Show p

        FilledSyncing p ->
            Show p


buttonHtml : (String -> Msg) -> String -> Person -> Html Msg
buttonHtml msgCtr label { name, url } =
    button
        [ onClick (msgCtr url)
        ]
        [ text label
        ]


buttonView : (String -> Msg) -> String -> PersonCache -> Html Msg
buttonView msgCtor label =
    buttonVisibility >> visibilityToHtml (buttonHtml msgCtor label)


loadingVisibility : Cache Http.Error a -> Visibility ()
loadingVisibility cache =
    case cache of
        Empty ->
            Hide

        EmptyInvalid _ ->
            Hide

        EmptyInvalidSyncing _ ->
            Show ()

        EmptySyncing ->
            Show ()

        Filled _ ->
            Hide

        FilledInvalid _ _ ->
            Hide

        FilledInvalidSyncing _ _ ->
            Show ()

        FilledSyncing _ ->
            Show ()


loadingHtml : Html Msg
loadingHtml =
    p
        []
        [ text "Loading"
        ]


loadingView : Cache Http.Error a -> Html Msg
loadingView =
    loadingVisibility >> visibilityToHtml (\() -> loadingHtml)


errorVisibility : Cache Http.Error a -> Visibility String
errorVisibility cache =
    case cache of
        Empty ->
            Hide

        EmptyInvalid _ ->
            Show "Error"

        EmptyInvalidSyncing _ ->
            Show "Error"

        EmptySyncing ->
            Hide

        Filled _ ->
            Hide

        FilledInvalid _ _ ->
            Show "Error"

        FilledInvalidSyncing _ _ ->
            Show "Error"

        FilledSyncing _ ->
            Hide


errorHtml : String -> Html Msg
errorHtml err =
    p
        []
        [ text err ]


errorView : Cache Http.Error a -> Html Msg
errorView =
    errorVisibility >> visibilityToHtml errorHtml


hairColorVisibility : PersonCache -> Visibility (Maybe String)
hairColorVisibility person =
    case person of
        Empty ->
            Hide

        EmptyInvalid _ ->
            Hide

        EmptyInvalidSyncing _ ->
            Hide

        EmptySyncing ->
            Hide

        Filled { hairColor } ->
            Show hairColor

        FilledInvalid _ { hairColor } ->
            Show hairColor

        FilledInvalidSyncing _ { hairColor } ->
            Show hairColor

        FilledSyncing { hairColor } ->
            Show hairColor


hairColorHtml : Maybe String -> Html Msg
hairColorHtml hairColor =
    case hairColor of
        Just h ->
            p
                []
                [ text h ]

        Nothing ->
            text ""


hairColorView : PersonCache -> Html Msg
hairColorView =
    hairColorVisibility >> visibilityToHtml hairColorHtml


nameVisibility : PersonCache -> Visibility String
nameVisibility person =
    case person of
        Empty ->
            Hide

        EmptyInvalid _ ->
            Hide

        EmptyInvalidSyncing _ ->
            Hide

        EmptySyncing ->
            Hide

        Filled { name } ->
            Show name

        FilledInvalid _ { name } ->
            Show name

        FilledInvalidSyncing _ { name } ->
            Show name

        FilledSyncing { name } ->
            Show name


nameHtml : String -> Html Msg
nameHtml name =
    p
        []
        [ text name ]


nameView : PersonCache -> Html Msg
nameView =
    nameVisibility >> visibilityToHtml nameHtml


itemView : PersonCache -> Html Msg
itemView person =
    li
        []
        [ p
            []
            [ nameView person
            , hairColorView person
            , errorView person
            , loadingView person
            ]
        , buttonView ItemRequest "Get details" person
        , buttonView ItemErrorRequest "Get error" person
        ]


listVisibility : Cache Http.Error PersonCollection -> Visibility PersonCollection
listVisibility cache =
    case cache of
        Empty ->
            Hide

        EmptyInvalid _ ->
            Hide

        EmptyInvalidSyncing error ->
            Hide

        EmptySyncing ->
            Hide

        Filled collection ->
            Show collection

        FilledInvalid _ collection ->
            Show collection

        FilledInvalidSyncing error collection ->
            Show collection

        FilledSyncing collection ->
            Show collection


listHtml : Collection PersonCache -> Html Msg
listHtml { entities, displayOrder } =
    let
        getPersons =
            flip Dict.get entities
    in
        List.map getPersons displayOrder
            |> filterNothings
            |> List.map itemView
            |> ul []


listView : Cache Http.Error PersonCollection -> Html Msg
listView =
    listVisibility >> visibilityToHtml listHtml



-- Cache


type Cache a b
    = Empty
    | EmptyInvalid a
    | EmptySyncing
    | EmptyInvalidSyncing a
    | Filled b
    | FilledSyncing b
    | FilledInvalid a b
    | FilledInvalidSyncing a b


type CacheEvent a b c
    = Sync
    | Error a
    | Update b
    | Patch c


type alias Transitions a b c =
    { updateEmpty : a -> b
    , updateFilled : a -> b -> b
    , patchFilled : c -> b -> b
    }


updateCache : Transitions c b d -> CacheEvent a c d -> Cache a b -> Cache a b
updateCache transitions event current =
    case current of
        Empty ->
            case event of
                Sync ->
                    EmptySyncing

                Error nextError ->
                    EmptyInvalid nextError

                Update nextData ->
                    Filled <| transitions.updateEmpty nextData

                Patch _ ->
                    current

        EmptyInvalid currentError ->
            case event of
                Sync ->
                    EmptyInvalidSyncing currentError

                Error nextError ->
                    EmptyInvalid nextError

                Update nextData ->
                    Filled <| transitions.updateEmpty nextData

                Patch _ ->
                    current

        EmptySyncing ->
            case event of
                Sync ->
                    EmptySyncing

                Error nextError ->
                    EmptyInvalid nextError

                Update nextData ->
                    Filled <| transitions.updateEmpty nextData

                Patch _ ->
                    current

        EmptyInvalidSyncing currentError ->
            case event of
                Sync ->
                    EmptyInvalidSyncing currentError

                Error nextError ->
                    EmptyInvalid nextError

                Update nextData ->
                    Filled <| transitions.updateEmpty nextData

                Patch _ ->
                    current

        Filled currentData ->
            case event of
                Sync ->
                    FilledSyncing currentData

                Error nextError ->
                    FilledInvalid nextError currentData

                Update nextData ->
                    Filled <| transitions.updateFilled nextData currentData

                Patch patch ->
                    Filled <| transitions.patchFilled patch currentData

        FilledInvalid currentError currentData ->
            case event of
                Sync ->
                    FilledInvalidSyncing currentError currentData

                Error nextError ->
                    FilledInvalid nextError currentData

                Update nextData ->
                    Filled <| transitions.updateFilled nextData currentData

                Patch patch ->
                    Filled <| transitions.patchFilled patch currentData

        FilledSyncing currentData ->
            case event of
                Sync ->
                    FilledSyncing currentData

                Error nextError ->
                    FilledInvalid nextError currentData

                Update nextData ->
                    Filled <| transitions.updateFilled nextData currentData

                Patch patch ->
                    Filled <| transitions.patchFilled patch currentData

        FilledInvalidSyncing currentError currentData ->
            case event of
                Sync ->
                    FilledInvalidSyncing currentError currentData

                Error nextError ->
                    FilledInvalid nextError currentData

                Update nextData ->
                    Filled <| transitions.updateFilled nextData currentData

                Patch patch ->
                    Filled <| transitions.patchFilled patch currentData



-- Http


getItem : String -> Cmd Msg
getItem url =
    Http.send (ItemResponse url) <|
        Http.get url decodePersonWithHairColor


getItemError : String -> Cmd Msg
getItemError url =
    Http.send (ItemResponse url) <|
        Http.get "https://swapi.co/api/foo/" decodePersonWithHairColor


getCollection : Cmd Msg
getCollection =
    Http.send CollectionResponse <|
        Http.get "https://swapi.co/api/people/" decodePersonCollection


getCollectionError : Cmd Msg
getCollectionError =
    Http.send CollectionResponse <|
        Http.get "https://swapi.co/api/foo/" decodePersonCollection



-- Helpers


filterNothing : Maybe a -> List a -> List a
filterNothing mX xs =
    case mX of
        Just x ->
            x :: xs

        Nothing ->
            xs


filterNothings : List (Maybe a) -> List a
filterNothings =
    List.foldl filterNothing []


identity2 : a -> b -> b
identity2 _ =
    identity


keyByUrl : { a | url : String } -> ( String, { a | url : String } )
keyByUrl x =
    ( x.url, x )
