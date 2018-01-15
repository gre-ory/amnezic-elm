
-- import

import Html exposing (..)
import Html.Attributes exposing (..)
import Array exposing (..)
import Keyboard exposing (..)

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
  , subscriptions = subscriptions
  }

-- init

init : ( Model, Cmd.Cmd Msg )
init = ( init_model, Cmd.none )

-- update

update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
  case msg of
    -- navivation
    GoToStartPage model ->
      ( go_to_start_page model, Cmd.none )
    GoToPreviousPage model ->
      ( go_to_previous_page model, Cmd.none )
    GoToNextPage model ->
      ( go_to_next_page model, Cmd.none )
    -- update
    UpdatePlayerName player_id player_name ->
      ( update_player model player_id ( update_player_name player_name ), Cmd.none )
    -- keyboard
    OnKey key_code ->
      case get_key key_code of
        Space -> ( go_to_start_page model, Cmd.none )
        ArrowLeft -> ( go_to_previous_page model, Cmd.none )
        ArrowRight -> ( go_to_next_page model, Cmd.none )
        _ -> ( model, Cmd.none )

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
      Keyboard.downs OnKey
    ]

-- view

view : Model -> Html Msg
view model =
    render_page model
