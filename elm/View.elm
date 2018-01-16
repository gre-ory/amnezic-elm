
module View exposing (..)

-- import

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)

import Type exposing (..)

-- helper

render_id_to_nb : Int -> Html Msg
render_id_to_nb id =
  text ( id_to_nb id )

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

render_default_page : Model -> String -> Html Msg
render_default_page model page_id =
  render_page_skeleton model page_id ( text ( "page " ++ page_id ++ " not yet implemented!" ) )

render_players_page : Model -> String -> Html Msg
render_players_page model page_id =
  render_page_skeleton model page_id ( render_players model )

render_questions_page : Model -> String -> Html Msg
render_questions_page model page_id =
  render_page_skeleton model page_id ( render_questions model )

-- button

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

-- skeleton

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

-- player

render_players : Model -> Html Msg
render_players model =
  div [ ]
    ( Array.toList <| Array.indexedMap render_player model.players )

render_player : Int -> Player -> Html Msg
render_player player_id player =
  let
    classes = "player"
  in
    div [ class classes ] [
      render_id_to_nb( player_id ),
      text ( " -- " ++ player.name ++ " -- " ++ ( toString player.score ) ++ " -- " ),
      input [ placeholder "player name", onInput ( UpdatePlayerName player_id ) ] [
        text ( toString player.name )
      ]
    ]

-- question

render_questions : Model -> Html Msg
render_questions model =
  case get_question model of
    Just question -> render_question model question
    Nothing -> render_error "unknown question!"

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
      ],
      fieldset [ ] [
        render_player_scores model
      ]
    ]

-- choice

render_correct_class : Maybe Bool -> String
render_correct_class maybe_is_correct =
  case maybe_is_correct of
    Just is_correct ->
      if is_correct then
        "correct"
      else
        "incorrect"
    Nothing ->
      ""

render_choice_class : Model -> Int -> String
render_choice_class model choice_id =
  case get_choice model choice_id of
    Just choice -> render_correct_class ( Just choice.correct )
    Nothing -> render_correct_class Nothing

render_choice : Model -> Int -> Choice -> Html Msg
render_choice model choice_id choice =
  let
    classes =
      if show_result model.state.step then
        "choice " ++ ( render_choice_class model choice_id )
      else
        "choice"
  in
    case model.state.step of
      StepNotReady ->
        div [ class classes ] [
          text "loading question..."
        ]
      StepShowChoices ->
        div [ class classes ] [
          div [ class "answer" ] [
            text choice.answer
          ],
          div [ class "hint" ] [
            text " --- "
          ]
        ]
      StepShowHints ->
        div [ class classes ] [
          div [ class "answer" ] [
            text choice.answer
          ],
          div [ class "hint" ] [
            text choice.hint
          ]
        ]
      StepShowCorrect ->
        div [ class classes ] [
          div [ class "answer" ] [
            text choice.answer
          ],
          div [ class "hint" ] [
            text choice.hint
          ]
        ]
      StepShowCards ->
        div [ class classes ] [
          render_cards model choice_id,
          div [ class "answer" ] [
            text choice.answer
          ],
          div [ class "hint" ] [
            text choice.hint
          ]
        ]
      StepShowScore ->
        div [ class classes ] [
          div [ class "answer" ] [
            text choice.answer
          ],
          div [ class "hint" ] [
            text choice.hint
          ]
        ]

-- card

render_cards : Model -> Int -> Html Msg
render_cards model choice_id =
  div [ class "cards" ]
    ( Array.toList <| Array.indexedMap ( render_card model choice_id ) model.players )

render_card : Model -> Int -> Int -> Player -> Html Msg
render_card model choice_id player_id player =
  let
    selected = has_selected_card choice_id player_id model
    classes =
      if selected then
        "card selected " ++ ( render_choice_class model choice_id )
      else
        "card " ++ ( render_choice_class model choice_id )
  in
    if selected then
      div [ class classes ] [
        render_id_to_nb( choice_id ),
        text " ",
        render_id_to_nb( player_id )
      ]
    else
      div [ class classes, onClick ( SelectCard choice_id player_id ) ] [
        render_id_to_nb( choice_id ),
        text " ",
        render_id_to_nb( player_id )
      ]

-- selected card

render_selected_cards : State -> Html Msg
render_selected_cards state =
  div [ class "selected_cards" ]
    ( Array.toList <| Array.indexedMap ( render_selected_card ) state.selected_cards )

render_selected_card : Int -> SelectedCard -> Html Msg
render_selected_card selected_card_id selected_card =
  let
    classes = "selected_card " ++ ( render_correct_class ( Just selected_card.correct ) )
  in
    div [ class classes, onClick ( UnselectCard selected_card.choice_id selected_card.player_id ) ] [
      text "#",
      render_id_to_nb( selected_card_id ),
      text " ",
      render_id_to_nb( selected_card.choice_id ),
      text " ",
      render_id_to_nb( selected_card.player_id ),
      text " ",
      text ( toString ( selected_card.correct ) )
    ]

-- player score

render_player_scores : Model -> Html Msg
render_player_scores model =
  div [ class "scores" ]
    ( List.indexedMap render_player_score <| List.reverse <| List.sortBy .score <| Array.toList model.players )

render_player_score : Int -> Player -> Html Msg
render_player_score rank_id player =
  div [ class ( "player rank-" ++ id_to_nb( rank_id ) ) ] [
    div [ ] [
      text player.name
    ],
    div [ ] [
      text ( toString player.score )
    ]
  ]
