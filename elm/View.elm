
module View exposing (..)

-- import

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Modal exposing (..)

import Type exposing (..)
import Init exposing (..)
import Update exposing (..)

-- helper

render_id_to_nb : Int -> Html Msg
render_id_to_nb id =
  text ( toString( id_to_nb id ) )

-- modal

-- page

render_page : Model -> Html Msg
render_page model =
    case model.state.page of
      PageStart -> render_page_skeleton model "start" [ render_all_cards model ]
      PageThemes -> render_default_page model "themes"
      PagePlayers -> render_players_page model "players"
      PageQuestions -> render_question_page model "question"
      PageScore -> render_default_page model "score"
      PageEnd -> render_default_page model "end"

render_default_page : Model -> String -> Html Msg
render_default_page model page_id =
  render_page_skeleton model page_id [ ]

render_players_page : Model -> String -> Html Msg
render_players_page model page_id =
  render_page_skeleton model page_id ( render_players model )

render_question_page : Model -> String -> Html Msg
render_question_page model page_id =
  render_page_skeleton model page_id ( render_question model )

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
      span [ class "button-text" ] [ text button_text ]
    else
      span [ class "button-icon" ] [ render_material_icon icon_class ]
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
    if model.state.page == target_page then
      render_activated_button icon_class button_class button_text button_description
    else if can_go_to_page model target_page then
      render_enabled_button icon_class button_class button_text button_description on_click
    else
      render_disabled_button icon_class button_class button_text button_description

render_header : Model -> List ( Html Msg )
render_header model = [
    case model.state.page of
      PageQuestions ->
        case get_question model of
          Just question -> render_question_theme question
          Nothing -> span [ ] [ ]
      _ -> div [ class "header-brand" ] [ text "@mnez!c" ]
    ,
    render_nav_item model PageStart "home" "Start",
    render_nav_item model PageThemes "menu" "Themes",
    render_nav_item model PagePlayers "people" "Players",
    render_nav_item model PageQuestions "queue_music" "Question",
    render_nav_item model PageScore "grade" "Score",
    render_nav_item model PageEnd "" "End",
    case model.state.page of
      PageQuestions -> render_question_nb model
      _ -> span [ ] [ ]
    ,
    render_enabled_button "keyboard_arrow_right" "move-forward" "Next" "Next step" MoveForward
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
    div [ class ( "page page-" ++ page_id ) ] [
      div [ class "page-header" ] ( render_header model ),
      div [ class "page-content" ] ( html_content ),
      div [ class "page-footer" ] ( render_footer model )
    ]

-- button

render_deactivate_player_button : Model -> Int -> Html Msg
render_deactivate_player_button model player_id =
  if ( can_deactivate_player model player_id ) then
    render_enabled_button "pause_circle_outline" "player-deactivate" "Deactivate" "Deactivate player" ( DeactivatePlayer player_id )
  else
    render_disabled_button "pause_circle_outline" "player-deactivate" "Deactivate" "Deactivate player"

render_activate_player_button : Model -> Int -> Html Msg
render_activate_player_button model player_id =
  if ( can_activate_player model player_id ) then
    render_enabled_button "play_circle_outline" "player-activate" "Activate" "Activate player" ( ActivatePlayer player_id )
  else
    render_disabled_button "play_circle_outline" "player-activate" "Activate" "Activate player"

render_delete_player_button : Model -> Int -> Html Msg
render_delete_player_button model player_id =
  if ( can_delete_player model player_id ) then
    render_enabled_button "delete" "player-delete" "Delete" "Delete player" ( DeletePlayer player_id )
  else
    render_disabled_button "delete" "player-delete" "Delete" "Delete player"

-- player

render_players : Model -> List ( Html Msg )
render_players model =
  let
    warning_notification =
      if not ( all_player_has_card model ) then
        div [ class "player-alert alert alert-warning" ] [ text "at least one player has no card!" ]
      else
        div [ class "player-alert alert alert-info" ] [ text "all players have cards!" ]
    -- next_player_id = "player-" ++ ( toString ( id_to_nb ( Array.length model.players ) ) )
    html_add_player =
      div [ class "player-add", onClick AddPlayer ] [
        render_material_icon "person_add"
      ]
  in
    warning_notification ::
        List.append
          ( Array.toList <| Array.indexedMap ( render_player model ) model.players )
          ( if can_add_player model then [ html_add_player ] else [] )

render_player : Model -> Int -> Player -> Html Msg
render_player model player_id player =
  let
    card_status = if has_card player then "player-with-card" else "player-without-card"
    player_status = if player.active then "player-active" else "player-inactive"
  in
    div [ class ( "player player-" ++ ( toString ( id_to_nb player_id ) )++ " " ++ player_status ++ " " ++ card_status ) ] [
      div [ class ( "player-card" ) ] [
        render_selectable_playing_card Nothing ( SetModalPlayerId player_id ) player.card
      ],
      div [ class "player-info" ] [
        div [ class "player-id" ] [
          text ( "Player #" ),
          render_id_to_nb( player_id ),
          text " / ",
          text player.name,
          text " / ",
          text player_status,
          text " / ",
          text card_status
        ],
        div [ class "player-name" ] [
          if player.active then
            input [ placeholder "player name", onInput ( UpdatePlayerName player_id ), value player.name ] [ ]
          else
            text player.name
        ],
        div [ class "player-score" ] [
          text ( "score: " ++ ( toString player.score ) ++ " pt(s)" )
        ],
        div [ class "player-buttons" ] [
            render_deactivate_player_button model player_id,
            render_activate_player_button model player_id,
            render_delete_player_button model player_id
        ]
      ],
      -- modal for card selection
      render_modal_player_card_selector model player_id player
    ]

render_modal_player_card_selector : Model -> Int -> Player -> Html Msg
render_modal_player_card_selector model player_id player =
  let
    show_modal =
      case model.state.maybe_modal_player_id of
        Just modal_player_id -> modal_player_id == player_id
        Nothing -> False
    modal_cfg = Modal.maintainableCssConfig "modal" Top UnsetModalPlayerId
    modal_view = Modal.view modal_cfg show_modal
  in
    modal_view [ ] [
      fieldset [ ] [
        legend [ ] [ text ( player.name ) ],
        render_not_selected_playing_card Nothing player.card
      ]
      ,
      fieldset [ ] [
        legend [ ] [ text ( "symbols" ) ],
        div [ ]
          ( List.map ( render_card_suit model player_id ) <| Array.toList model.available_card_suits )
      ]
      ,
      fieldset [ ] [
        legend [ ] [ text ( "colors" ) ],
        div [ ]
          ( List.map ( render_card_color model player_id False ) <| Array.toList model.available_card_colors ),
        div [ ]
          ( List.map ( render_card_color model player_id True ) <| Array.toList model.available_card_colors )
      ]
      ,
      button [ onClick UnsetModalPlayerId ] [ text "Close" ]
    ]

render_card_suit : Model -> Int -> CardSuit -> Html Msg
render_card_suit model player_id card_suit =
  case get_player model player_id of
    Just player ->
      let
        selected = match_card_suit card_suit player
        target_card = update_card_suit card_suit player.card
        not_selectable = ( not ( match_card target_card player ) ) && ( is_card_already_selected target_card model )
        display_card = init_card card_suit Black False
      in
        if selected then
          render_selected_playing_card Nothing ( UnselectCardSuit player_id ) display_card
        else if not_selectable then
          render_not_selectable_playing_card Nothing display_card
        else
          render_selectable_playing_card Nothing ( SelectCardSuit player_id card_suit ) display_card
    Nothing ->
      span [ ] [ ]

render_card_color : Model -> Int -> Bool -> CardColor -> Html Msg
render_card_color model player_id inverted_color card_color =
  case get_player model player_id of
    Just player ->
      let
        selected = match_card_color card_color inverted_color player
        target_card = update_inverted_color inverted_color <| update_card_color card_color player.card
        not_selectable = ( not ( match_card target_card player ) ) && ( is_card_already_selected target_card model )
        display_card = init_card NoSuit card_color inverted_color
      in
        if selected then
          render_selected_playing_card Nothing ( UnselectCardColor player_id ) display_card
        else if not_selectable  then
          render_not_selectable_playing_card Nothing display_card
        else
          render_selectable_playing_card Nothing ( SelectCardColor player_id card_color inverted_color ) display_card
    Nothing ->
      span [ ] [ ]

-- question

render_question_nb : Model -> Html Msg
render_question_nb model =
  let
    question_now = id_to_nb model.state.question_id
    question_max = id_to_nb ( ( Array.length model.questions ) - 1 )
  in
    div [ class "question-nb" ] [
      text ( "Question " ++ ( toString question_now ) ++ " / " ++ ( toString question_max ) )
    ]

render_question_theme : Question -> Html Msg
render_question_theme question =
  div [ class "question-theme" ] [
    text question.theme
  ]

render_question_audio : Question -> Html Msg
render_question_audio question =
  div [ class "question-audio" ] [
    text question.audio
  ]

render_question : Model -> List ( Html Msg )
render_question model =
  case get_question model of
    Just question ->
      ( List.concat <| Array.toList <| Array.indexedMap ( render_choice model ) question.choices )
       |> \l -> ( l ++ [ render_selected_cards model model.state ] )
       |> \l -> ( l ++ [ render_player_scores model ] )
    Nothing ->
      [ render_error "unknown question!" ]

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

render_choice_nb : Model -> Int -> Choice -> Html Msg
render_choice_nb model choice_id choice =
  let
    choice_nb = toString ( id_to_nb choice_id )
  in
    div [ class ( "nb nb-" ++ choice_nb ) ] [
      text choice_nb
    ]

render_choice_content : Model -> Int -> Choice -> List ( Html Msg )
render_choice_content model choice_id choice =
  case model.state.step of
    StepNotReady ->
      [ text "loading question..." ]
    StepShowChoices ->
      [
        div [ class "answer" ] [ text choice.answer ],
        div [ class "hint" ] [ text " --- " ]
      ]
    StepShowHints ->
      [
        div [ class "answer" ] [ text choice.answer ],
        div [ class "hint" ] [ text choice.hint ]
      ]
    StepShowCorrect ->
      [
        div [ class "answer" ] [ text choice.answer ],
        div [ class "hint" ] [ text choice.hint ]
      ]
    StepShowCards ->
      [
        render_choice_cards model choice_id,
        div [ class "answer" ] [ text choice.answer ],
        div [ class "hint" ] [ text choice.hint ]
      ]
    StepShowScore ->
      [
        div [ class "answer" ] [ text choice.answer ],
        div [ class "hint" ] [ text choice.hint ]
      ]

render_choice : Model -> Int -> Choice -> List ( Html Msg )
render_choice model choice_id choice =
  let
    choice_nb = toString ( id_to_nb choice_id )
    classes =
      if show_result model.state.step then
        "choice choice-" ++ choice_nb ++ " " ++ ( render_choice_class model choice_id )
      else
        "choice choice-" ++ choice_nb
  in
    [
      render_choice_nb model choice_id choice,
      div [ class classes ]
        ( render_choice_content model choice_id choice )
    ]

-- playing card

render_selected_playing_card : Maybe Int -> Msg -> Card -> Html Msg
render_selected_playing_card maybe_card_rank on_click card =
  render_playing_card card maybe_card_rank [ "selected" ] on_click

render_not_selected_playing_card : Maybe Int -> Card -> Html Msg
render_not_selected_playing_card maybe_card_rank card =
  render_playing_card card maybe_card_rank [ "not-selected" ] NothingToDo

render_selectable_playing_card : Maybe Int -> Msg -> Card -> Html Msg
render_selectable_playing_card maybe_card_rank on_click card =
  render_playing_card card maybe_card_rank [ "selectable" ] on_click

render_not_selectable_playing_card : Maybe Int -> Card -> Html Msg
render_not_selectable_playing_card maybe_card_rank card =
  render_playing_card card maybe_card_rank [ "not-selectable" ] NothingToDo

render_playing_card : Card -> Maybe Int -> List String -> Msg -> Html Msg
render_playing_card card maybe_card_rank card_classes on_click =
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
        Square -> "suit-square"
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

-- card

render_choice_cards : Model -> Int -> Html Msg
render_choice_cards model choice_id =
  div [ class "cards" ]
    ( Array.toList <| Array.indexedMap ( render_choice_card model choice_id ) model.players )

render_choice_card : Model -> Int -> Int -> Player -> Html Msg
render_choice_card model choice_id player_id player =
  let
    card_rank = id_to_nb choice_id
  in
    if player.active then
      if has_selected_card choice_id player_id model then
        render_not_selectable_playing_card ( Just card_rank ) player.card -- ( UnselectCard choice_id player_id )
      else
        render_selectable_playing_card ( Just card_rank ) ( SelectCard choice_id player_id ) player.card
    else
      span [ ] [ ]

-- selected cards

render_selected_cards : Model -> State -> Html Msg
render_selected_cards model state =
  div [ class "selected_cards" ]
    ( Array.toList <| Array.indexedMap ( render_selected_card model ) state.selected_cards )

render_selected_card : Model -> Int -> SelectedCard -> Html Msg
render_selected_card model selected_card_id selected_card =
  let
    classes = "selected_card " ++ ( render_correct_class ( Just selected_card.correct ) )
    card_rank = Just ( id_to_nb selected_card.choice_id )
    on_click = UnselectCard selected_card.choice_id selected_card.player_id
  in
    div [ class classes ] [
      div [ class "selected_card-card" ] [
        case get_player model selected_card.player_id of
          Just player ->
            render_selectable_playing_card card_rank on_click player.card
          Nothing ->
            span [ ] [ ]
      ],
      div [ class "selected_card-info" ] [
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

render_all_cards : Model -> Html Msg
render_all_cards model =
  let
    card_by_color =
      List.append
        ( List.map ( \c -> ( init_card Dot c False ) ) ( NoColor :: ( Array.toList model.available_card_colors ) ) )
        ( List.map ( \c -> ( init_card Dot c True  ) ) ( NoColor :: ( Array.toList model.available_card_colors ) ) )
    card_by_suit =
      List.append
        ( List.map ( \s -> ( init_card s NoColor False ) ) ( NoSuit :: ( Array.toList model.available_card_suits ) ) )
        ( List.map ( \s -> ( init_card s NoColor True  ) ) ( NoSuit :: ( Array.toList model.available_card_suits ) ) )
  in
    div [ class "tmp" ] [
      div [ class "players" ]
        ( List.map ( render_selectable_playing_card Nothing NothingToDo ) card_by_suit )
      ,
      div [ class "players correct" ]
        ( List.map ( render_selectable_playing_card ( Just 2 ) NothingToDo ) card_by_color )
      ,
      div [ class "players incorrect" ]
        ( List.map ( render_selectable_playing_card ( Just 2 ) NothingToDo ) card_by_color )
      ,
      div [ class "players" ]
        ( List.map ( render_selected_playing_card ( Just 2 ) NothingToDo ) card_by_color )
      ,
      div [ class "players" ]
        ( List.map ( render_not_selectable_playing_card ( Just 2 ) ) card_by_color )
    ]
