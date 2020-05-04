module Main exposing (main)

import Array exposing (Array)
import Browser exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Http exposing (Error(..), Expect, expectJson, expectStringResponse)
import Json.Decode as Decode exposing (map2)
import Json.Encode as Encode


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias UUID =
    { value : String }


type alias ResourceLocation =
    { value : String }


type alias Model =
    { listArray : Array CardList
    , listIdToCardList : Dict String (Array Card)
    , beingDragged : Maybe ( Int, Int )
    , typingListTitle : String
    , typingCardTitle : String
    }


type alias CardList =
    { id : CardListId
    , title : CardListTitle
    }


type alias CardListId =
    { value : UUID }


type alias CardListTitle =
    { value : String }


type alias Card =
    { id : CardId
    , listId : CardListId
    , title : CardTitle
    }


type alias CardId =
    { value : UUID }


type alias CardTitle =
    { value : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Array.empty Dict.empty Nothing "" ""
    , Cmd.batch
        [ Http.get
            { url = "http://localhost:9000/v1/card-lists"
            , expect =
                expectJson
                    (\result ->
                        case result of
                            Ok res ->
                                GotCardLists res

                            Err error ->
                                Debug.log (Debug.toString error)
                                    DoNothing
                    )
                    cardListsDecoder
            }
        , Http.get
            { url = "http://localhost:9000/v1/cards"
            , expect =
                expectJson
                    (\result ->
                        case result of
                            Ok res ->
                                GotCards res

                            Err error ->
                                Debug.log (Debug.toString error)
                                    DoNothing
                    )
                    cardsDecoder
            }
        ]
    )


type Msg
    = ChangeListTitle String
    | ChangeCardTitle String
    | AddList
    | GetCardListResource ResourceLocation
    | GotCardList CardList
    | GotCardLists (List CardList)
    | AddCard CardListId
    | GetCardResource ResourceLocation
    | GotCard Card
    | GotCards (List Card)
    | DoNothing



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeListTitle typingListTitle ->
            ( { model
                | typingListTitle = typingListTitle
              }
            , Cmd.none
            )

        ChangeCardTitle typingCardTitle ->
            ( { model
                | typingCardTitle = typingCardTitle
              }
            , Cmd.none
            )

        AddList ->
            ( { model
                | typingListTitle = ""
              }
            , Http.post
                { url = "http://localhost:9000/v1/card-lists"
                , body =
                    Http.jsonBody <|
                        Encode.object
                            [ ( "title", Encode.string model.typingListTitle ) ]
                , expect =
                    expectStringResponse
                        (\result ->
                            case result of
                                Ok location ->
                                    GetCardListResource location

                                Err error ->
                                    Debug.log error
                                        DoNothing
                        )
                        extractResourceLocation
                }
            )

        AddCard listId ->
            ( { model
                | typingCardTitle = ""
              }
            , Http.post
                { url = "http://localhost:9000/v1/card-lists/" ++ listId.value.value ++ "/cards"
                , body =
                    Http.jsonBody <|
                        Encode.object
                            [ ( "title", Encode.string model.typingCardTitle ) ]
                , expect =
                    expectStringResponse
                        (\result ->
                            case result of
                                Ok location ->
                                    GetCardResource location

                                Err error ->
                                    Debug.log error
                                        DoNothing
                        )
                        extractResourceLocation
                }
            )

        GetCardListResource location ->
            ( model
            , Http.get
                { url = "http://localhost:9000" ++ location.value
                , expect =
                    expectJson
                        (\result ->
                            case result of
                                Ok res ->
                                    GotCardList res

                                Err error ->
                                    Debug.log (Debug.toString error)
                                        DoNothing
                        )
                        cardListDecoder
                }
            )

        GetCardResource location ->
            ( model
            , Http.get
                { url = "http://localhost:9000" ++ location.value
                , expect =
                    expectJson
                        (\result ->
                            case result of
                                Ok res ->
                                    GotCard res

                                Err error ->
                                    Debug.log (Debug.toString error)
                                        DoNothing
                        )
                        cardDecoder
                }
            )

        GotCardList cardList ->
            ( { model
                | listArray =
                    Array.push cardList model.listArray
              }
            , Cmd.none
            )

        GotCard card ->
            ( { model
                | listIdToCardList =
                    case Dict.get card.listId.value.value model.listIdToCardList of
                        Just list ->
                            Dict.insert card.listId.value.value (Array.push card list) model.listIdToCardList

                        Nothing ->
                            Dict.insert card.listId.value.value (Array.push card Array.empty) model.listIdToCardList
              }
            , Cmd.none
            )

        GotCardLists cardLists ->
            ( { model
                | listArray =
                    Array.fromList cardLists
              }
            , Cmd.none
            )

        GotCards cards ->
            ( { model
                | listIdToCardList =
                    List.foldl
                        (\card acc ->
                            let
                                maybeList =
                                    Dict.get card.listId.value.value acc
                            in
                            case maybeList of
                                Just list ->
                                    Dict.insert card.listId.value.value (Array.push card list) acc

                                Nothing ->
                                    Dict.insert card.listId.value.value Array.empty acc
                        )
                        Dict.empty
                        cards
              }
            , Cmd.none
            )

        DoNothing ->
            ( model
            , Cmd.none
            )


expectJson : (Result Http.Error a -> msg) -> Decode.Decoder a -> Expect msg
expectJson toMsg decoder =
    expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err <| BadBody (Decode.errorToString err)


extractResourceLocation : Http.Response String -> Result String ResourceLocation
extractResourceLocation resp =
    case resp of
        Http.GoodStatus_ metadata body ->
            case Dict.get "content-location" metadata.headers of
                Just contentLocation ->
                    Ok (contentLocation |> ResourceLocation)

                Nothing ->
                    Err "Content-Location is no where"

        _ ->
            Err "err"



-- DECODER


cardListDecoder : Decode.Decoder CardList
cardListDecoder =
    let
        uuid =
            Decode.map UUID (Decode.field "id" Decode.string)

        title =
            Decode.map CardListTitle (Decode.field "title" Decode.string)

        cardListId =
            Decode.map CardListId uuid
    in
    Decode.map2 CardList cardListId title


cardListsDecoder : Decode.Decoder (List CardList)
cardListsDecoder =
    Decode.list cardListDecoder


cardDecoder : Decode.Decoder Card
cardDecoder =
    let
        cardUuid =
            Decode.map UUID (Decode.field "id" Decode.string)

        cardListUuid =
            Decode.map UUID (Decode.field "listId" Decode.string)

        title =
            Decode.map CardTitle (Decode.field "title" Decode.string)

        cardId =
            Decode.map CardId cardUuid

        cardListId =
            Decode.map CardListId cardListUuid
    in
    Decode.map3 Card cardId cardListId title


cardsDecoder : Decode.Decoder (List Card)
cardsDecoder =
    Decode.list cardDecoder



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container is-fluid", style "overflow" "auto", style "height" "100vh", style "width" "100vw" ]
        [ div [ class "columns" ] <|
            Array.toList <|
                Array.push (listInputForm model) <|
                    Array.fromList (List.map (viewCardList model) <| Array.toList model.listArray)
        ]


viewCardList : Model -> CardList -> Html Msg
viewCardList model cardList =
    div [ class "column is-one-fifth" ]
        [ div []
            [ div [ class "card", style "background-color" "#ebecf0" ]
                [ div [ class "card-header" ] [ p [ class "card-header-title" ] [ text cardList.title.value ] ]
                , div []
                    (List.map (\list -> viewCard list) <|
                        Array.toList <|
                            Maybe.withDefault Array.empty <|
                                Dict.get cardList.id.value.value model.listIdToCardList
                    )
                , div [ class "field" ]
                    [ div [ class "control card-content" ]
                        [ input
                            [ class "input"
                            , type_ "text"
                            , placeholder "Add a Card"
                            , value model.typingCardTitle
                            , Events.onInput ChangeCardTitle
                            ]
                            []
                        ]
                    , div [ class "card-content" ] [ input [ class "button is-small", style "background-color" "#5aac44", style "color" "#FFF", type_ "submit", value "Add Card", Events.onClick (AddCard cardList.id) ] [] ]
                    ]
                ]
            ]
        ]


viewCard : Card -> Html msg
viewCard card =
    div [ class "card-content"] [ text card.title.value ]


listInputForm : Model -> Html Msg
listInputForm model =
    div [ class "column is-one-fifth" ]
        [ input
            [ class "input"
            , type_ "text"
            , placeholder "Enter a title for this card..."
            , value model.typingListTitle
            , Events.onInput ChangeListTitle
            ]
            []
        , input [ class "button is-small", type_ "submit", value "Add another list", Events.onClick AddList ] []
        ]



-- EVENTS


onDragStart msg =
    Events.on "dragstart" <|
        Decode.succeed msg


onDragOver msg =
    Events.preventDefaultOn "dragover" <|
        Decode.succeed ( msg, True )


onDrop msg =
    Events.preventDefaultOn "drop" <|
        Decode.succeed ( msg, True )
