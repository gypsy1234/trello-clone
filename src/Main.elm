module Main exposing (..)

import Browser
import Html exposing (Html, div, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
    { todoList: List String
    , newToDo: String
    }

init : Model
init =
    Model [] ""

type Msg =
    AddCard |
    Change String

-- UPDATE

update: Msg -> Model -> Model
update msg model =
  case msg of
    AddCard ->
        { model | todoList = List.reverse (model.newToDo :: List.reverse model.todoList)
         , newToDo = ""
        }
    Change newToDo ->
        { model | newToDo = newToDo}


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] (List.map (\l -> div [] [ text l ]) model.todoList)
    , input [ type_ "textarea", placeholder "このカードにタイトルを入力", value model.newToDo, onInput Change] []
    , input [ type_ "submit", value "カードを追加", onClick AddCard] []
    ]
