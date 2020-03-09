module Main exposing (..)

import Array exposing (Array)
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode
import Html.Events as Events

main =
  Browser.sandbox
    { init = init
    , update = update
    , view = view
    }

-- MODEL

type alias Model =
    { todoArray: Array ToDo
    , newToDo: String
    , beingDragged: Maybe Int
    }

type alias ToDo =
    { value: ToDoValue }

type alias ToDoValue =
    { value: String }

init : Model
init = Model Array.empty "" Nothing


type Msg =
    AddCard
    | Change String
    | Drag Int
    | DragOver
    | Drop Int

-- UPDATE

update: Msg -> Model -> Model
update msg model =
  case msg of
    AddCard ->
         { model | todoArray = Array.push (ToDo <| ToDoValue model.newToDo) model.todoArray
                 , newToDo = ""
          }

    Change newToDo ->
        { model | newToDo = newToDo}

    Drag index ->
        { model | beingDragged = Just index }

    DragOver ->
        model

    Drop toIndex ->
        case model.beingDragged of
            Nothing ->
                model

            Just fromIndex ->
                { model
                    | beingDragged = Nothing
                    , todoArray = case (moveItem fromIndex toIndex model.todoArray) of
                        Ok array -> array
                        Err _ -> model.todoArray
                }

moveItem : Int -> Int -> Array item -> Result String (Array item)
moveItem fromIndex toIndex array =
    let
         draggedDown =
            fromIndex < toIndex
         maybeFromItem =
            Array.get fromIndex array

         move from =
              List.foldr (\(index, todo) acc ->
                  if index == toIndex && draggedDown then todo :: from :: acc
                  else if index == toIndex then from :: todo :: acc
                  else if index == fromIndex then acc
                  else todo :: acc
                  ) [] (Array.toIndexedList array)
              |> Array.fromList
    in
        Result.fromMaybe "error" <| Maybe.map move maybeFromItem

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] (List.map (\(index, todo) -> div [ draggable "true", Drag index |> onDragStart, Drop index |> onDrop, onDragOver DragOver ] [ text todo.value.value ]) <| Array.toIndexedList model.todoArray)
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
