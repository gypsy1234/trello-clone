module Main exposing (..)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Html.Events as Events
import Array

main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }

-- MODEL

type alias Model =
    { todoList: List ToDo
    , newToDo: String
    , beingDragged: Maybe ToDo
    , nextId: Int
    }

type alias ToDo =
    { id: ToDoId
    , value: ToDoValue
    }

type alias ToDoId =
    { value: Int }

type alias ToDoValue =
    { value: String }

init : Model
init = Model [] "" Nothing 0


type Msg =
    AddCard
    | Change String
    | Drag ToDo
    | DragOver
    | Drop ToDo

-- UPDATE

update: Msg -> Model -> Model
update msg model =
  case msg of
    AddCard ->
         { model | todoList = List.reverse ( { id = ToDoId model.nextId, value = ToDoValue model.newToDo } :: List.reverse model.todoList)
                 , newToDo = ""
                 , nextId = model.nextId + 1
          }

    Change newToDo ->
        { model | newToDo = newToDo}

    Drag toDo ->
        { model | beingDragged = Just toDo }

    DragOver ->
        model

    Drop toId->
        case model.beingDragged of
            Nothing ->
                model

            Just fromId ->
                { model
                    | beingDragged = Nothing
                    , todoList = moveItem fromId toId model.todoList
                }

getIndex : a -> Int -> List a -> Maybe Int
getIndex target start list =
     case list of
        [] ->
            Nothing
        head :: tail ->
            if head == target then Just start
            else getIndex target (start + 1) tail


moveItem : ToDo -> ToDo -> List ToDo -> List ToDo
moveItem from to list =
    let
         maybeFromIndex =
             getIndex from 0 list
         maybeToIndex =
              getIndex to 0 list
         draggedDown =
             case (maybeFromIndex, maybeToIndex) of
                 (Just fromIndex, Just toIndex) -> fromIndex < toIndex
                 (_, _) -> True
    in
        List.foldr (\todo acc ->
            if todo == to && draggedDown then to :: from :: acc
            else if todo == to then from :: to :: acc
            else if todo == from then acc
            else todo :: acc
            ) [] list

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] (List.map (\todo -> div [ draggable "true", Drag todo |> onDragStart, Drop todo |> onDrop, onDragOver DragOver ] [ text todo.value.value ]) model.todoList)
    , input [ type_ "textarea", placeholder "このカードにタイトルを入力", value model.newToDo, Events.onInput Change] []
    , input [ type_ "submit", value "カードを追加", Events.onClick AddCard] []
    ]

-- EVENTS

onDragStart msg =
    Events.on "dragstart"
        <| Decode.succeed msg

onDragOver msg =
    Events.preventDefaultOn "dragover"
        <| Decode.succeed (msg, True)


onDrop msg =
    Events.preventDefaultOn "drop"
        <| Decode.succeed (msg, True)
