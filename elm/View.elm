
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
  case model.state.page of
    PageStart -> render_default_page model "start"
    PageThemes -> render_default_page model "themes"
    PagePlayers -> render_players_page model "players"
    PageQuestions -> render_questions_page model "questions"
    PageScore -> render_default_page model "score"
    PageEnd -> render_default_page model "end"

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

render_error : String -> Html Msg
render_error message =
  div [ ] [
    text message
  ]

render_page_skeleton : Model -> String -> Html Msg -> Html Msg
render_page_skeleton model page_id html_content =
  div [ ] [
    render_header model,
    fieldset [ class ( "page " ++ page_id ) ] [
      html_content
    ],
    render_footer model
  ]

render_default_page : Model -> String -> Html Msg
render_default_page model page_id =
  render_page_skeleton model page_id ( text ( "page " ++ page_id ++ " not yet implemented!" ) )

render_players_page : Model -> String -> Html Msg
render_players_page model page_id =
  render_page_skeleton model page_id ( render_players model )

render_questions_page : Model -> String -> Html Msg
render_questions_page model page_id =
  render_page_skeleton model page_id ( render_questions model )

-- players

render_players : Model -> Html Msg
render_players model =
  div [ ]
    ( Array.toList <| Array.indexedMap render_player model.players )

render_player : Int -> Player -> Html Msg
render_player player_id player =
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
        text ( toString player.name )
      ]
    ]

-- questions

render_questions : Model -> Html Msg
render_questions model =
  case get_question model of
    Just question -> render_question model question
    Nothing -> render_error "unknown question!"

-- question

render_question : Model -> Question -> Html Msg
render_question model question =
  let
    classes =
      if model.state.question_id == 1 then
        "question first"
      else
        "question"
  in
    div [ class classes ] [
      fieldset [ ] [
        text question.theme
      ],
      fieldset [ ] [
        text question.audio
      ],
      fieldset [ ]
        ( Array.toList <| Array.indexedMap ( render_choice model ) question.choices ),
      fieldset [ ] [
        render_selected_cards model.state
      ]
    ]

-- choice

render_choice : Model -> Int -> Choice -> Html Msg
render_choice model choice_id choice =
  let
    choice_type =
      if choice.correct then
        "correct"
      else
        "distractor"
    choice_class = "choice"
  in
    case model.state.step of
      StepNotReady ->
        div [ class choice_class ] [
          text "loading question..."
        ]
      StepShowChoices ->
        div [ class choice_class ] [
          div [ class "answer" ] [
            text choice.answer
          ],
          div [ class "hint" ] [
            text " --- "
          ]
        ]
      StepShowHints ->
        div [ class choice_class ] [
          div [ class "answer" ] [
            text choice.answer
          ],
          div [ class "hint" ] [
            text choice.hint
          ]
        ]
      StepShowCorrect ->
        div [ class ( choice_class ++ " " ++ choice_type ) ] [
          div [ class "answer" ] [
            text choice.answer
          ],
          div [ class "hint" ] [
            text choice.hint
          ]
        ]
      StepShowCards ->
        div [ class ( choice_class ++ " " ++ choice_type ) ] [
          render_cards model choice_id,
          div [ class "answer" ] [
            text choice.answer
          ],
          div [ class "hint" ] [
            text choice.hint
          ]
        ]
      StepShowScore ->
        div [ class ( choice_class ++ " " ++ choice_type ) ] [
          render_cards model choice_id,
          div [ class "answer" ] [
            text choice.answer
          ],
          div [ class "hint" ] [
            text choice.hint
          ]
        ]

-- card

render_id_to_nb : Int -> Html Msg
render_id_to_nb id =
  text ( toString( id + 1 ) )

render_cards : Model -> Int -> Html Msg
render_cards model choice_id =
  div [ class "cards" ]
    ( Array.toList <| Array.indexedMap ( render_card model choice_id ) model.players )

render_card : Model -> Int -> Int -> Player -> Html Msg
render_card model choice_id player_id player =
  div [ class "card", onClick ( SelectCard choice_id player_id ) ] [
    render_id_to_nb( choice_id ),
    text " ",
    render_id_to_nb( player_id )
  ]

render_selected_cards : State -> Html Msg
render_selected_cards state =
  div [ class "selected_cards" ]
    ( Array.toList <| Array.indexedMap ( render_selected_card ) state.selected_cards )

render_selected_card : Int -> SelectedCard -> Html Msg
render_selected_card selected_card_id selected_card =
  div [ class "selected_card", onClick ( UnselectCard selected_card.choice_id selected_card.player_id ) ] [
    text "#",
    render_id_to_nb( selected_card_id ),
    text " ",
    render_id_to_nb( selected_card.choice_id ),
    text " ",
    render_id_to_nb( selected_card.player_id ),
    text " ",
    text ( toString ( selected_card.correct ) )
  ]
