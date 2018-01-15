
-- import

import Html exposing (..)
import Html.Attributes exposing (..)
import Array exposing (..)

import Type exposing (..)
import View exposing (..)
import Update exposing (..)
-- import Subscriptions exposing (..)

-- main

main : Program Never Model Msg
main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = always Sub.none
  }

-- types


-- init

init : ( Model, Cmd.Cmd Msg )
init =
  ( { page = PageStart
    , questions = init_default_questions
    , players = init_default_players
    }
  , Cmd.none
  )

-- update

update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
  case msg of
    GoToStartPage model ->
      ( go_to_start_page model, Cmd.none )
    GoToPreviousPage model ->
      ( go_to_previous_page model, Cmd.none )
    GoToNextPage model ->
      ( go_to_next_page model, Cmd.none )
    UpdatePlayerName player_id player_name ->
      ( update_player model player_id ( update_player_name player_name ), Cmd.none )

-- subscriptions

-- view

view : Model -> Html Msg
view model =
    render_page model
