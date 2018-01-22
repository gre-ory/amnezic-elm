
module Update exposing (..)

-- import

import Array exposing (..)

import Type exposing (..)
import Init exposing (..)

-- keyboard

get_key : Int -> Key
get_key key_code =
  case key_code of
    32 -> Space
    37 -> ArrowLeft
    39 -> ArrowRight
    _ -> Unknown

-- navigation

reset_game: Model -> Model
reset_game model =
  { model | state = init_state }

go_to_page: Model -> Page -> Model
go_to_page model target_page =
  if can_go_to_page model target_page then
    if target_page == PageStart then
      update_page target_page <| reset_game model
    else
      update_page target_page model
  else
    model

go_to_next_page: Model -> Model
go_to_next_page model =
  go_to_page model ( next_page model.state.page )

go_to_previous_page: Model -> Model
go_to_previous_page model =
  go_to_page model ( previous_page model.state.page )

go_to_next_step: Model -> Model
go_to_next_step model =
  if can_go_to_next_step model then
    case model.state.step of
      StepNotReady -> update_step StepShowChoices model
      StepShowChoices -> update_step StepShowHints model
      StepShowHints -> update_step StepShowCorrect model
      StepShowCorrect -> update_step StepShowCards model
      StepShowCards -> update_step StepShowScore <| apply_engaged_points model
      StepShowScore -> go_to_next_question model
  else
    model

move_forward: Model -> Model
move_forward model =
  if can_move_forward model then
    if model.state.page == PageQuestions then
      go_to_next_step model
    else
      go_to_next_page model
  else
    model

move_backward: Model -> Model
move_backward model =
  if can_move_backward model then
    go_to_previous_page model
  else
    model

next_question_id : Model -> Int
next_question_id model =
  ( model.state.question_id + 1 )

can_go_to_next_question : Model -> Bool
can_go_to_next_question model =
  ( ( next_question_id model ) < Array.length( model.questions ) )

go_to_next_question : Model -> Model
go_to_next_question model =
  if ( can_go_to_next_question model ) then
    go_to_question model ( next_question_id model )
  else
    update_page PageScore model

go_to_question : Model -> Int -> Model
go_to_question model question_id =
  { model | state = ( update_state_for_new_question question_id model.state ) }

-- player

add_player : Model -> Model
add_player model =
  if can_add_player model then
    { model | players = Array.push ( init_default_player False ( Array.length model.players ) ) model.players }
  else
    model

deactivate_player : Model -> Int -> Model
deactivate_player model player_id =
  if can_deactivate_player model player_id then
    case get_player model player_id of
      Just player -> update_player model player_id ( update_player_active False )
      Nothing -> model
  else
    model

activate_player : Model -> Int -> Model
activate_player model player_id =
  if can_activate_player model player_id then
    case get_player model player_id of
      Just player -> update_player model player_id ( update_player_active True )
      Nothing -> model
  else
    model

delete_player : Model -> Int -> Model
delete_player model player_id =
  if can_delete_player model player_id then
    let
      before =
        if 0 < player_id then
          Array.slice ( 0 ) ( player_id ) model.players
        else
          Array.fromList [ ]
      after =
        if player_id < Array.length model.players then
          Array.slice ( player_id + 1 ) ( Array.length model.players ) model.players
        else
          Array.fromList [ ]
    in
      { model | players = Array.append before after }
  else
    model

-- update

update_page : Page -> Model -> Model
update_page page model =
  { model | state = ( update_state_page page model.state ) }

update_state_page: Page -> State -> State
update_state_page page state =
  { state | page = page }

update_step : Step -> Model -> Model
update_step step model =
  { model | state = ( update_state_step step model.state ) }

update_state_step: Step -> State -> State
update_state_step step state =
  { state | step = step }

update_state_for_new_question: Int -> State -> State
update_state_for_new_question question_id state =
  { state | question_id = question_id, step = StepNotReady, media_status = MediaNotReady, selected_cards = Array.fromList [ ] }

update_player : Model -> Int -> ( Player -> Player ) -> Model
update_player model player_id update_player_fn =
  case Array.get player_id model.players of
    Just player ->
      { model | players = Array.set player_id ( update_player_fn player ) model.players }
    Nothing ->
      model

update_player_card_suit_id : Maybe Int -> Player -> Player
update_player_card_suit_id maybe_card_suit_id player =
  { player | maybe_card_suit_id = maybe_card_suit_id }

update_player_name : String -> Player -> Player
update_player_name name player =
  { player | name = name }

update_player_active : Bool -> Player -> Player
update_player_active active player =
  { player | active = active }

reset_selected_cards : State -> State
reset_selected_cards state =
  { state | selected_cards = Array.fromList [ ] }

select_card : Model -> Int -> Int -> Model
select_card model choice_id player_id =
  case get_choice model choice_id of
    Just choice ->
      let
        nb_player = Array.length model.players
        selected_card = init_selected_card choice_id player_id choice.correct
        update_fn = Array.push selected_card
      in
        { model | state = ( update_selected_cards update_fn nb_player model.state ) }
    Nothing -> model

unselect_card : Model -> Int -> Int -> Model
unselect_card model choice_id player_id =
  let
    nb_player = Array.length model.players
    update_fn = Array.filter ( unmatch_selected_card choice_id player_id )
  in
    { model | state = ( update_selected_cards update_fn nb_player model.state ) }

update_selected_cards : ( Array SelectedCard -> Array SelectedCard ) -> Int -> State -> State
update_selected_cards update_fn nb_player state =
  let
    score_mode = state.score_mode
  in
    { state | selected_cards = ( compute_engaged_points score_mode nb_player <| update_fn <| state.selected_cards ) }

compute_engaged_points : ScoreMode -> Int -> Array SelectedCard -> Array SelectedCard
compute_engaged_points score_mode nb_player selected_cards =
  let
      nb_selected_card = Array.length selected_cards
  in
    Array.indexedMap ( compute_engaged_point score_mode nb_player nb_selected_card ) selected_cards

compute_engaged_point : ScoreMode -> Int -> Int -> Int -> SelectedCard -> SelectedCard
compute_engaged_point score_mode nb_player nb_selected_card velociy_id selected_card =
  let
    sign = if selected_card.correct then 1 else -1
    point_per_velocity = if ( velociy_id < nb_player ) then ( nb_player - velociy_id ) else 1
  in
    case score_mode of
      ScoreByVelocity ->
        { selected_card | engaged_point = sign * point_per_velocity }
      ScoreByVelocityCappedByRank ->
        { selected_card | engaged_point = sign } -- TODO

apply_selected_card : Int -> SelectedCard -> Int -> Int
apply_selected_card player_id selected_card score =
  if selected_card.player_id == player_id then
    score + selected_card.engaged_point
  else
    score

apply_selected_cards_on_player : Array SelectedCard -> Int -> Player -> Player
apply_selected_cards_on_player selected_cards player_id player =
  { player | score = ( Array.foldr ( apply_selected_card player_id ) player.score selected_cards ) }

apply_selected_cards : Model -> Model
apply_selected_cards model =
  { model | players = Array.indexedMap ( apply_selected_cards_on_player model.state.selected_cards ) model.players }

remove_selected_cards : Model -> Model
remove_selected_cards model =
  { model | state = reset_selected_cards model.state }

apply_engaged_points : Model -> Model
apply_engaged_points model =
  remove_selected_cards ( apply_selected_cards model )
