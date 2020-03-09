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
    { listArray: Array ToDoList
    , beingDragged: Maybe (Int, Int)
    }

type alias ToDoList =
    { value: Array ToDo
    , newToDo: String
    }

type alias ToDo =
    { value: ToDoValue }

type alias ToDoValue =
    { value: String }

init : Model
init = Model Array.empty Nothing


type Msg =
    AddList
    | AddCard Int
    | Change Int String
    | Drag Int Int
    | DragOver
    | Drop Int Int

-- UPDATE

update: Msg -> Model -> Model
update msg model =
  case msg of
    AddList ->
        { model | listArray =
           Array.push (ToDoList Array.empty "") model.listArray
        }

    AddCard listIndex ->
         { model | listArray =
            case Array.get listIndex model.listArray of
                Nothing ->
                    model.listArray
                Just list ->
                    Array.set listIndex (ToDoList (Array.push (ToDo <| ToDoValue list.newToDo) list.value) "") model.listArray
          }

    Change listIndex newToDo ->
        { model | listArray =
            case Array.get listIndex model.listArray of
                Nothing ->
                    model.listArray
                Just list ->
                    Array.set listIndex (ToDoList list.value newToDo) model.listArray

        }

    Drag listIndex todoIndex ->
        { model | beingDragged = Just (listIndex, todoIndex) }

    DragOver ->
        model

    Drop toListIndex toToDoIndex ->
        case model.beingDragged of
            Nothing ->
                model

            Just (fromListIndex, fromToDoIndex) ->
                if toListIndex /= fromListIndex then
                    { model | beingDragged = Nothing }
                else
                    { model
                        | beingDragged = Nothing
                        , listArray =
                            case Array.get toListIndex model.listArray of
                                Nothing ->
                                    model.listArray
                                Just list ->
                                    case (moveItem fromToDoIndex toToDoIndex list.value) of
                                        Ok array ->
                                            Array.set fromListIndex (ToDoList array "") model.listArray
                                        Err _ ->
                                            model.listArray
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
    [ div [] (List.map (\(index, _) -> todoListView model index) <| Array.toIndexedList model.listArray)
    , input [ type_ "submit", value "リストを追加", Events.onClick AddList] []
    ]

todoListView : Model -> Int -> Html Msg
todoListView model listIndex =
  div []
    [ div [] (List.map (\(index, todo) -> div [ draggable "true", Drag listIndex index |> onDragStart, Drop listIndex index |> onDrop, onDragOver DragOver ] [ text todo.value.value ])
        <| Array.toIndexedList <| (\list -> list.value) <| Maybe.withDefault (ToDoList Array.empty "") <| Array.get listIndex model.listArray)
    , input [ type_ "textarea", placeholder "このカードにタイトルを入力", value <| (\list -> list.newToDo) <| Maybe.withDefault (ToDoList Array.empty "") <| Array.get listIndex model.listArray
        , Events.onInput (Change listIndex)] []
    , input [ type_ "submit", value "カードを追加", Events.onClick (AddCard listIndex)] []
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
