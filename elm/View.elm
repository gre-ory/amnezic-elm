
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

myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

-- questions

render_questions : Model -> Html Msg
render_questions model =
  case Array.get model.state.question_id model.questions of
    Just question -> render_question model.state question
    Nothing -> render_error "unknown question!"

-- question

render_question : State -> Question -> Html Msg
render_question state question =
  let
    classes =
      if state.question_id == 1 then
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
      fieldset [ ] [
        ul [ ]
          ( Array.toList <| Array.indexedMap ( render_choice state ) question.choices )
      ]
    ]

-- choice

render_choice : State -> Int -> Choice -> Html Msg
render_choice state choice_id choice =
  let
    choice_type =
      if choice.correct then
        "correct"
      else
        "distractor"
    choice_class = "choice " ++ choice_type
  in
    case state.step of
      StepNotReady ->
        li [ class choice_class ] [
          text "loading question..."
        ]
      StepReady ->
        li [ class choice_class ] [
          text "question loaded..."
        ]
      StepShowChoices ->
        li [ class choice_class ] [
          text choice.answer
        ]
      StepShowHints ->
        li [ class choice_class ] [
          text choice.answer,
          text " --- ",
          text choice.hint
        ]
      StepShowCorrect ->
        li [ class choice_class ] [
          text choice.answer,
          text " --- ",
          text choice_type,
          text " --- ",
          text choice.hint
        ]
      StepShowCards ->
        li [ class choice_class ] [
          text "cards..."
        ]
      StepShowScore ->
        li [ class choice_class ] [
          text "score..."
        ]
      StepEnd ->
        li [ class choice_class ] [
          text "next question..."
        ]
