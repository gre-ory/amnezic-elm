
module View exposing (..)

-- import

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)

import Type exposing (..)

-- page

render_page : Model -> Html Msg
render_page model =
  case model.page of
    PageStart -> render_default_page model "start" "start: not yet implemented..."
    PageSetUpPlayers -> render_setup_players model
    PageSetUpThemes -> render_default_page model "set-up-themes" "set-up themes: not yet implemented..."
    PageSetUpQuestions -> render_default_page model "set-up-questions" "set-up questions: not yet implemented..."
    PageQuestions -> render_default_page model "questions" "questions: not yet implemented..."
    PageScore -> render_default_page model "score" "score: not yet implemented..."
    PageEnd -> render_default_page model "end" "end: not yet implemented..."

render_default_page : Model -> String -> String -> Html Msg
render_default_page model page_class page_content =
  div [ ] [
    render_header model,
    fieldset [ class ( "page " ++ page_class ) ] [
      text page_content
    ],
    render_footer model
  ]

render_header : Model -> Html Msg
render_header model =
  fieldset [ ] [
    render_start_button model,
    render_previous_button model,
    render_next_button model
  ]

render_footer : Model -> Html Msg
render_footer model =
  fieldset [ ] [
    text "@amnezic"
  ]

render_start_button : Model -> Html Msg
render_start_button model =
  button [ onClick ( GoToStartPage model ) ] [
    text "start"
  ]

render_previous_button : Model -> Html Msg
render_previous_button model =
  button [ onClick ( GoToPreviousPage model ) ] [
    text "previous"
  ]

render_next_button : Model -> Html Msg
render_next_button model =
  button [ onClick ( GoToNextPage model ) ] [
    text "next"
  ]

-- players

render_setup_players : Model -> Html Msg
render_setup_players model =
  div [] [
    render_header model,
    fieldset [ class ( "page set-up-players" ) ] [
      div [ class "players" ]
        ( Array.toList <| Array.indexedMap render_setup_player model.players )
    ],
    render_footer model
  ]

render_setup_player : Int -> Player -> Html Msg
render_setup_player player_id player =
  let
    classes =
      if player_id == 1 then
        "player first"
      else
        "player"
  in
    div [ class classes ] [
      text ( ( toString player_id ) ++ " -- " ++ player.name ++ " -- " ++ ( toString player.score ) ++ " -- " ),
      input [ placeholder "player name", onInput ( UpdatePlayerName player_id ) ] [
        text player.name
      ]
    ]

-- questions

render_questions : Model -> Html Msg
render_questions model =
  div [ class "questions" ]
    ( Array.toList <| Array.indexedMap render_question model.questions )

-- question

render_question : Int -> Question -> Html Msg
render_question question_index question =
  let
    classes =
      if question_index == 1 then
        "question first"
      else
        "question"
  in
    button [ class classes ] [
      text question.title
    ]
