
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
  text ( toString( id_to_nb id ) )

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
  render_page_skeleton model page_id [ ]

render_players_page : Model -> String -> Html Msg
render_players_page model page_id =
  render_page_skeleton model page_id ( render_players model )

render_questions_page : Model -> String -> Html Msg
render_questions_page model page_id =
  render_page_skeleton model page_id ( render_questions model )

-- skeleton

render_error : String -> Html Msg
render_error message =
  text message

render_icon : String -> Html Msg
render_icon icon_class =
  if not ( String.isEmpty icon_class ) then
    i [ class ( "icon fas fa-" ++ icon_class ) ] [ ]
  else
    span [ ] [ ]

render_nav_item : Model -> Page -> String -> String -> Html Msg
render_nav_item model target_page icon_class page_name =
  if model.state.page == target_page then
    li [ class "navbar-item active" ] [
      div [ class "navbar-link active", title page_name ] [
        render_icon icon_class,
        text page_name
      ]
    ]
  else if can_go_to_page model target_page then
    li [ class "navbar-item" ] [
      a [ class "navbar-link enabled", href "#", onClick ( GoToPage target_page ), title page_name ] [
        render_icon icon_class,
        text page_name
      ]
    ]
  else
    li [ class "navbar-item" ] [
      div [ class "navbar-link disabled", title page_name ] [
        render_icon icon_class,
        text page_name
      ]
    ]

render_header : Model -> List ( Html Msg )
render_header model = [
    nav [ class "navbar" ] [
      div [ class "navbar-brand" ] [ text "@mnez!c" ],
      ul [ class "navbar-items" ] [
        render_nav_item model PageStart "reply" "Start",
        render_nav_item model PageThemes "sliders-h" "Themes",
        render_nav_item model PagePlayers "users" "Players",
        render_nav_item model PageQuestions "question" "Question",
        render_nav_item model PageScore "star" "Score",
        render_nav_item model PageEnd "" "End"
      ],
      div [ class "navbar-nav" ] [
        a [ class "navbar-link enabled", href "#", onClick MoveForward, title "Next step" ] [
          render_icon "angle-right",
          text "Next"
        ]
      ]
    ]
  ]

render_footer : Model -> List ( Html Msg )
render_footer model = [
    text "@amnezic"
  ]

render_page_skeleton : Model -> String -> List ( Html Msg ) -> Html Msg
render_page_skeleton model page_id html_elements =
  let
    html_content =
      if List.isEmpty html_elements then
        [ render_error ( "page " ++ page_id ++ " not yet implemented!" ) ]
      else
        html_elements
  in
    div [ class ( "page " ++ page_id ) ] [
      div [ class "page-header" ] ( render_header model ),
      div [ class "page-content" ] ( html_content ),
      div [ class "page-footer" ] ( render_footer model )
    ]

-- button

render_start_button : Model -> Html Msg
render_start_button model =
  let
    maybe_on_click = if can_go_to_page model PageStart then Just ( GoToPage PageStart ) else Nothing
  in
    render_button "repeat" "navigation start" "Start" maybe_on_click

render_previous_button : Model -> Html Msg
render_previous_button model =
  let
    maybe_on_click = if can_go_to_previous_page model then Just ( GoToPreviousPage ) else Nothing
  in
    render_button "angle-left" "navigation previous" "Previous" maybe_on_click

render_next_button : Model -> Html Msg
render_next_button model =
  let
    maybe_on_click = if can_go_to_next_page model then Just ( GoToNextPage ) else Nothing
  in
    render_button "angle-right" "navigation next" "Next" maybe_on_click

render_add_player_button : Model -> Html Msg
render_add_player_button model =
  let
    maybe_on_click = if ( can_add_player model ) then Just ( AddPlayer ) else Nothing
  in
    render_button "plus" "player add" "Add player" maybe_on_click

render_deactivate_player_button : Model -> Int -> Html Msg
render_deactivate_player_button model player_id =
  if ( can_deactivate_player model player_id ) then
    render_button "pause" "player deactivate" "Deactivate player" ( Just ( DeactivatePlayer player_id ) )
  else
    span [] []

render_activate_player_button : Model -> Int -> Html Msg
render_activate_player_button model player_id =
  if ( can_activate_player model player_id ) then
    render_button "play" "player activate" "Activate player" ( Just ( ActivatePlayer player_id ) )
  else
    span [] []

render_delete_player_button : Model -> Int -> Html Msg
render_delete_player_button model player_id =
  let
    maybe_on_click = if ( can_delete_player model player_id ) then Just ( DeletePlayer player_id ) else Nothing
  in
    render_button "trash" "player delete" "Delete player" maybe_on_click

render_button : String -> String -> String -> Maybe Msg -> Html Msg
render_button icon_class button_class button_text maybe_on_click =
  case maybe_on_click of
    Just on_click ->
      button [ class ( "button " ++ button_class ), onClick on_click, title button_text ] [
        render_icon icon_class,
        text button_text
      ]
    Nothing ->
      button [ class ( "button disabled " ++ button_class ), title button_text ] [
        render_icon icon_class,
        text button_text
      ]

-- player

render_players : Model -> List ( Html Msg )
render_players model =
  let
    warning_notification =
      if not ( all_player_has_card_suit model ) then
        span [ class "alert alert-warning" ] [ text "please select one card for each player!" ]
      else
        span [ ] [ ]
  in
    [
      div [ class "row" ] [
        warning_notification
      ],
      div [ class "row" ]
        ( Array.toList <| Array.indexedMap ( render_player model ) model.players ),
      div [ class "row" ] [
        render_add_player_button model
      ]
    ]


render_player : Model -> Int -> Player -> Html Msg
render_player model player_id player =
  let
    classes = "player"
  in
    div [ class classes ] [
      render_id_to_nb( player_id ),
      text ( " -- " ++ player.name ++ " -- " ++ ( toString player.score ) ++ " -- " ),
      input [ placeholder "player name", onInput ( UpdatePlayerName player_id ), value player.name ] [ ],
      render_deactivate_player_button model player_id,
      render_activate_player_button model player_id,
      render_delete_player_button model player_id,
      text " -- ",
      render_card_suits model player_id
    ]

-- card suits

get_card_suit_class : CardSuit -> String
get_card_suit_class card_suit =
  case card_suit of
    Heart -> "heart"
    Diamond -> "diamond"
    Club -> "club"
    Spade -> "spade"
    TarotHeart -> "tarot-heart"
    TarotDiamond -> "tarot-diamond"
    TarotClub -> "tarot-club"
    TarotSpade -> "tarot-spade"
    TarotTrump -> "tarot-trump"
    UnoRed -> "uno-red"
    UnoBlue -> "uno-blue"
    UnoGreen -> "uno-green"
    UnoYellow -> "uno-yellow"

render_card_suits : Model -> Int -> Html Msg
render_card_suits model player_id =
  div [ ]
    ( Array.toList <| Array.indexedMap ( render_card_suit model player_id ) model.card_suits )

render_card_suit : Model -> Int -> Int -> CardSuit -> Html Msg
render_card_suit model player_id card_suit_id card_suit =
  case get_player model player_id of
    Just player ->
      let
        is_owned_by_player =
          case player.maybe_card_suit_id of
            Just player_card_suit_id -> ( player_card_suit_id == card_suit_id )
            Nothing -> False
        is_owned_by_other = if is_owned_by_player then False else ( is_card_suit_already_selected card_suit_id model )
        on_click =
          if is_owned_by_player then
            UnselectCardSuit player_id
          else if is_owned_by_other  then
            NothingToDo
          else
            SelectCardSuit player_id card_suit_id
        extra_class =
          if is_owned_by_player then
            "selected-by-player"
          else if is_owned_by_other then
            "not-selectable"
          else
            "selectable"
      in
        ( get_render_card_fn card_suit ) Nothing extra_class on_click
    Nothing ->
      span [ ] [ ]

render_playing_card : CardSuit -> Maybe Int -> String -> Msg -> Html Msg
render_playing_card card_suit maybe_rank extra_class on_click =
  let
    card_suit_class = ( "suit-" ++ get_card_suit_class card_suit )
    rank_class =
      case maybe_rank of
        Just rank -> "rank-" ++ ( toString rank )
        Nothing -> "no-rank"
    classes = String.join " " [ "card", card_suit_class, rank_class, extra_class ]
  in
    div [ class classes, onClick on_click ] [
      case maybe_rank of
        Just rank ->
          span [ class "rank" ] [ text ( toString rank ) ]
        Nothing ->
          span [ class "rank" ] [  ]
    ]

render_tarot_playing_card : CardSuit -> Maybe Int -> String -> Msg -> Html Msg
render_tarot_playing_card card_suit maybe_rank extra_class on_click =
  render_playing_card card_suit maybe_rank extra_class on_click

render_uno_playing_card : CardSuit -> Maybe Int -> String -> Msg -> Html Msg
render_uno_playing_card card_suit maybe_rank extra_class on_click =
  render_playing_card card_suit maybe_rank extra_class on_click

get_render_card_fn : CardSuit -> ( Maybe Int -> String -> Msg -> Html Msg )
get_render_card_fn card_suit =
  let
    render_card_fn =
      case card_suit of
        Heart -> render_playing_card
        Diamond -> render_playing_card
        Club -> render_playing_card
        Spade -> render_playing_card
        TarotHeart -> render_tarot_playing_card
        TarotDiamond -> render_tarot_playing_card
        TarotClub -> render_tarot_playing_card
        TarotSpade -> render_tarot_playing_card
        TarotTrump -> render_tarot_playing_card
        UnoRed -> render_uno_playing_card
        UnoBlue -> render_uno_playing_card
        UnoGreen -> render_uno_playing_card
        UnoYellow -> render_uno_playing_card
  in
    render_card_fn card_suit

-- question

render_questions : Model -> List ( Html Msg )
render_questions model =
  List.append [ render_questions_progress model ] ( render_question model )

render_questions_progress : Model -> Html Msg
render_questions_progress model =
  let
    question_min = id_to_nb 0
    question_now = id_to_nb model.state.question_id
    question_max = id_to_nb ( ( Array.length model.questions ) - 1 )
    progress_percent = ( ( ( question_now - question_min + 1 ) * 100 ) // ( question_max - question_min + 1 ) )
    progress_style = ( "width: " ++ toString( progress_percent ) ++ "%;" )
  in
    div [ class "row" ] [
      text ( "Question " ++ ( toString question_now ) ++ " / " ++ ( toString question_max ) ),
      div [ class "progress" ] [
        div [ class "progress-bar", attribute "role" "progressbar", attribute "style" progress_style, attribute "aria-valuenow" ( toString question_now ), attribute "aria-valuemin" ( toString question_min ), attribute "aria-valuemax" ( toString question_max ) ] [ ]
      ]
    ]

render_question : Model -> List ( Html Msg )
render_question model =
  case get_question model of
    Just question ->
      [
        div [ class "row theme" ] [
          text question.theme
        ],
        div [ class "row audio" ] [
          text question.audio
        ],
        div [ class "row question" ]
          ( Array.toList <| Array.indexedMap ( render_choice model ) question.choices ),
        fieldset [ ] [
          render_selected_cards model.state
        ],
        fieldset [ ] [
          render_player_scores model
        ]
      ]
    Nothing ->
      [
        div [ class "row question" ] [
          render_error "unknown question!"
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
    on_click = if not selected then SelectCard choice_id player_id else NothingToDo
    classes = "card " ++ ( render_choice_class model choice_id ) ++ ( if selected then " selected" else "" )
  in
    if player.active then
      div [ class classes, onClick on_click ] [
        text "P",
        render_id_to_nb( player_id )
      ]
    else
      span [ ] [ ]

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
      div [ ] [
        text "P",
        render_id_to_nb( selected_card.player_id )
      ],
      div [ ] [
        if selected_card.correct then
          text ( "+" ++ ( toString selected_card.engaged_point ) )
        else
          text ( toString selected_card.engaged_point )
      ]
    ]

-- player score

render_player_scores : Model -> Html Msg
render_player_scores model =
  div [ class "scores" ]
    ( List.indexedMap render_player_score <| List.reverse <| List.sortBy .score <| Array.toList model.players )

render_player_score : Int -> Player -> Html Msg
render_player_score rank_id player =
  div [ class ( "player rank-" ++ toString( id_to_nb( rank_id ) ) ) ] [
    div [ ] [
      text player.name
    ],
    div [ ] [
      text ( toString player.score )
    ]
  ]
