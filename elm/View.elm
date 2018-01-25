
module View exposing (..)

-- import

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)

import Type exposing (..)
import Init exposing (..)

-- helper

render_id_to_nb : Int -> Html Msg
render_id_to_nb id =
  text ( toString( id_to_nb id ) )

-- modal

-- page

render_page : Model -> Html Msg
render_page model =
  case model.state.page of
    PageStart -> render_page_skeleton model "start" [ render_test_cards ]
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

render_material_icon : String -> Html Msg
render_material_icon icon_class =
  if String.isEmpty icon_class then
    span [ ] [ ]
  else
    i [ class "icon material-icons" ] [ text icon_class ]

render_button : String -> String -> String -> String -> Msg -> Html Msg
render_button icon_class button_class button_text button_description on_click =
  div [ class ( "button " ++ button_class ), title button_description, onClick on_click ] [
    if String.isEmpty icon_class then
      span [ ] [ ]
    else
      span [ class "button-icon" ] [ render_material_icon icon_class ],
    span [ class "button-text" ] [ text button_text ]
  ]

render_activated_button : String -> String -> String -> String  -> Html Msg
render_activated_button icon_class button_class button_text button_description =
  render_button icon_class ( "activated " ++ button_class ) button_text button_description NothingToDo

render_enabled_button : String -> String -> String -> String  -> Msg -> Html Msg
render_enabled_button icon_class button_class button_text button_description on_click =
  render_button icon_class ( "enabled " ++ button_class ) button_text button_description on_click

render_disabled_button : String -> String -> String -> String  -> Html Msg
render_disabled_button icon_class button_class button_text button_description =
  render_button icon_class ( "disabled " ++ button_class ) button_text button_description NothingToDo

render_nav_item : Model -> Page -> String -> String -> Html Msg
render_nav_item model target_page icon_class page_name =
  let
    button_class = "go-to-" ++ page_name
    button_text = page_name
    button_description = "Go to " ++ page_name ++ "page"
    on_click = GoToPage target_page
  in
    li [ class "navbar-item" ] [
      if model.state.page == target_page then
        render_activated_button icon_class button_class button_text button_description
      else if can_go_to_page model target_page then
        render_enabled_button icon_class button_class button_text button_description on_click
      else
        render_disabled_button icon_class button_class button_text button_description
    ]

render_header : Model -> List ( Html Msg )
render_header model = [
    nav [ class "navbar" ] [
      div [ class "navbar-brand" ] [ text "@mnez!c" ],
      ul [ class "navbar-items" ] [
        render_nav_item model PageStart "home" "Start",
        render_nav_item model PageThemes "menu" "Themes",
        render_nav_item model PagePlayers "people" "Players",
        render_nav_item model PageQuestions "queue_music" "Question",
        render_nav_item model PageScore "grade" "Score",
        render_nav_item model PageEnd "" "End"
      ],
      div [ class "navbar-nav" ] [
        render_enabled_button "keyboard_arrow_right" "move-forward" "Next" "Next step" MoveForward
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

render_add_player_button : Model -> Html Msg
render_add_player_button model =
  if ( can_add_player model ) then
    render_enabled_button "person_add" "player-add" "Add" "Add player" ( AddPlayer )
  else
    span [] []

render_deactivate_player_button : Model -> Int -> Html Msg
render_deactivate_player_button model player_id =
  if ( can_deactivate_player model player_id ) then
    render_enabled_button "" "player-deactivate" "Deactivate" "Deactivate player" ( DeactivatePlayer player_id )
  else
    span [] []

render_activate_player_button : Model -> Int -> Html Msg
render_activate_player_button model player_id =
  if ( can_activate_player model player_id ) then
    render_enabled_button "" "player-activate" "Activate" "Activate player" ( ActivatePlayer player_id )
  else
    span [] []

render_delete_player_button : Model -> Int -> Html Msg
render_delete_player_button model player_id =
  if ( can_delete_player model player_id ) then
    render_enabled_button "" "player-delete" "Delete" "Delete player" ( DeletePlayer player_id )
  else
    render_disabled_button "" "player-delete" "Delete" "Delete player"

-- player

render_players : Model -> List ( Html Msg )
render_players model =
  let
    warning_notification =
      if not ( all_player_has_card model ) then
        span [ class "alert alert-warning" ] [ text "please select one card type and color for each player!" ]
      else
        span [ ] [ ]
    html_add_player =
        if can_add_player model then
          div [ class "player" ] [ render_add_player_button model ]
        else
          span [ ] [ ]
    html_players = List.append ( Array.toList <| Array.indexedMap ( render_player model ) model.players ) [ html_add_player ]
  in
    [
      div [ class "row" ] [
        warning_notification
      ],
      div [ class "players" ]
        html_players
    ]

render_player : Model -> Int -> Player -> Html Msg
render_player model player_id player =
  let
    classes = String.join " " <|
      ( "player" ) ::
      ( if player.active then "active" else "inactive" ) ::
      ( if has_card player then "valid" else "invalid" ) ::
      ( if has_card player then "valid" else "invalid" ) ::
      []
  in
    div [ class classes ] [
      render_id_to_nb( player_id ),
      text " - ",
      input [ placeholder "player name", onInput ( UpdatePlayerName player_id ), value player.name ] [ ],
      render_deactivate_player_button model player_id,
      render_activate_player_button model player_id,
      render_delete_player_button model player_id,
      div [ class "status" ] [
        text ( "score: " ++ ( toString player.score ) ++ " pt(s)" ),
        text ( ", status: " ++ ( classes ) )
      ],
      div [] [
        render_card_suits model player_id
      ],
      div [] [
        render_card_colors model player_id
      ]
    ]

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

-- playing card

render_selected_card_ : Maybe Int -> Msg -> Card -> Html Msg
render_selected_card_ maybe_card_rank on_click card =
  render_card_ card maybe_card_rank [ "selected" ] on_click

render_selectable_card_ : Maybe Int -> Msg -> Card -> Html Msg
render_selectable_card_ maybe_card_rank on_click card =
  render_card_ card maybe_card_rank [ "selectable" ] on_click

render_not_selectable_card_ : Maybe Int -> Card -> Html Msg
render_not_selectable_card_ maybe_card_rank card =
  render_card_ card maybe_card_rank [ "not-selectable" ] NothingToDo

render_card_ : Card -> Maybe Int -> List String -> Msg -> Html Msg
render_card_ card maybe_card_rank card_classes on_click =
  let
    card_suit_class =
      case card.card_suit of
        NoSuit -> "no-suit"
        Club -> "suit-club"
        Spade -> "suit-spade"
        Heart -> "suit-heart"
        Diamond -> "suit-diamond"
        Star -> "suit-star"
        Dot -> "suit-dot"
    card_color_class =
      case card.card_color of
        NoColor -> "no-color"
        Black -> "color-black"
        Red -> "color-red"
        Yellow -> "color-yellow"
        Blue -> "color-blue"
        Green -> "color-green"
    card_inverted_class =
      if card.inverted_color then
        "inverted-color"
      else
        ""
    card_rank_class =
      case maybe_card_rank of
        Just card_rank -> "rank-" ++ ( toString card_rank )
        Nothing -> "no-rank"
    classes = String.join " " ( "card" :: card_suit_class :: card_color_class :: card_inverted_class :: card_rank_class :: card_classes )
  in
    div [ class classes, onClick on_click ] [
      div [ class "card-content" ] [
        div [ class "card-rank" ] [ ]
      ]
    ]

-- card colors

render_card_colors : Model -> Int -> Html Msg
render_card_colors model player_id =
  span [ ] [
    span [ ]
      ( Array.toList <| Array.indexedMap ( render_card_color model player_id ) model.available_card_colors ),
    render_invert_card_color model player_id      
  ]

render_card_color : Model -> Int -> Int -> CardColor -> Html Msg
render_card_color model player_id card_color_id card_color =
  case get_player model player_id of
    Just player ->
      let
        new_card = init_card player.card.card_suit card_color player.card.inverted_color
        is_owned_by_player = ( match_card_color card_color player )
        is_owned_by_other = ( not is_owned_by_player ) && ( is_card_already_selected new_card model )
      in
        if is_owned_by_player then
          render_selected_card_ Nothing ( UnselectCardColor player_id ) new_card
        else if is_owned_by_other  then
          render_not_selectable_card_ Nothing new_card
        else
          render_selectable_card_ Nothing ( SelectCardColor player_id card_color ) new_card
    Nothing ->
      span [ ] [ ]

render_invert_card_color : Model -> Int -> Html Msg
render_invert_card_color model player_id =
  case get_player model player_id of
    Just player ->
      let
        new_card = init_card player.card.card_suit player.card.card_color ( not player.card.inverted_color )
        is_owned_by_other = ( is_card_already_selected new_card model )
      in
        if is_owned_by_other  then
          render_not_selectable_card_ Nothing new_card
        else
          render_selectable_card_ Nothing ( InvertCardColor player_id ) new_card
    Nothing ->
      span [ ] [ ]

-- card suits

render_card_suits : Model -> Int -> Html Msg
render_card_suits model player_id =
  span [ ]
    ( Array.toList <| Array.indexedMap ( render_card_suit model player_id ) model.available_card_suits )

render_card_suit : Model -> Int -> Int -> CardSuit -> Html Msg
render_card_suit model player_id card_suit_id card_suit =
  case get_player model player_id of
    Just player ->
      let
        new_card = init_card card_suit player.card.card_color player.card.inverted_color
        is_owned_by_player = ( match_card_suit card_suit player )
        is_owned_by_other = ( not is_owned_by_player ) && ( is_card_already_selected new_card model )
      in
        if is_owned_by_player then
          render_selected_card_ Nothing ( UnselectCardSuit player_id ) new_card
        else if is_owned_by_other  then
          render_not_selectable_card_ Nothing new_card
        else
          render_selectable_card_ Nothing ( SelectCardSuit player_id card_suit ) new_card
    Nothing ->
      span [ ] [ ]

-- card

render_cards : Model -> Int -> Html Msg
render_cards model choice_id =
  div [ class "cards" ]
    ( Array.toList <| Array.indexedMap ( render_card model choice_id ) model.players )

render_card : Model -> Int -> Int -> Player -> Html Msg
render_card model choice_id player_id player =
  let
    card_rank = id_to_nb choice_id
  in
    if player.active then
      if has_selected_card choice_id player_id model then
        render_selected_card_ ( Just card_rank ) ( UnselectCard choice_id player_id ) player.card
      else
        render_selectable_card_ ( Just card_rank ) ( SelectCard choice_id player_id ) player.card
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

-- test

render_test_cards : Html Msg
render_test_cards =
  let
    cards =
      ( init_card NoSuit NoColor False ) ::
      ( init_card Dot NoColor False ) ::
      ( init_card Dot Blue False ) ::
      ( init_card Spade Green False ) ::
      ( init_card Star Yellow False ) ::
      ( init_card Club Black False ) ::
      ( init_card Diamond Red False ) ::
      ( init_card NoSuit NoColor True ) ::
      ( init_card Dot NoColor True ) ::
      ( init_card Dot Blue True ) ::
      ( init_card Spade Green True ) ::
      ( init_card Star Yellow True ) ::
      ( init_card Club Black True ) ::
      ( init_card Diamond Red True ) ::
      []
  in
    div [ class "tmp" ] [
      div [ class "players" ]
        ( List.map ( render_selectable_card_ Nothing NothingToDo ) cards )
      ,
      div [ class "players" ]
        ( List.map ( render_selectable_card_ ( Just 2 ) NothingToDo ) cards )
      ,
      div [ class "players correct" ]
        ( List.map ( render_selectable_card_ ( Just 2 ) NothingToDo ) cards )
      ,
      div [ class "players incorrect" ]
        ( List.map ( render_selectable_card_ ( Just 2 ) NothingToDo ) cards )
      ,
      div [ class "players" ]
        ( List.map ( render_selected_card_ ( Just 2 ) NothingToDo ) cards )
      ,
      div [ class "players" ]
        ( List.map ( render_not_selectable_card_ ( Just 2 ) ) cards )
    ]
