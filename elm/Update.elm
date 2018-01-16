
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

go_to_start_page: Model -> Model
go_to_start_page model =
  reset_game model

go_to_previous_page: Model -> Model
go_to_previous_page model =
  case model.state.page of
    PageStart -> model
    PageThemes -> model
    PagePlayers -> model
    PageQuestions -> update_page PagePlayers model
    PageScore -> update_page PageQuestions model
    PageEnd -> update_page PageScore model

go_to_next_page: Model -> Model
go_to_next_page model =
  case model.state.page of
    PageStart -> update_page PageThemes model
    PageThemes -> update_page PagePlayers model
    PagePlayers -> update_page PageQuestions model
    PageQuestions -> go_to_next_step model
    PageScore -> update_page PageEnd model
    PageEnd -> reset_game model

go_to_next_step: Model -> Model
go_to_next_step model =
  case model.state.step of
    StepNotReady -> update_step StepShowChoices model
    StepShowChoices -> update_step StepShowHints model
    StepShowHints -> update_step StepShowCorrect model
    StepShowCorrect -> update_step StepShowCards model
    StepShowCards -> update_step StepShowScore model
    StepShowScore -> go_to_next_question model

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

update_player_name : String -> Player -> Player
update_player_name player_name player =
  { player | name = player_name }

select_card : Model -> Int -> Int -> Model
select_card model choice_id player_id =
  case get_choice model choice_id of
    Just choice ->
      let
        selected_card = init_selected_card choice_id player_id choice.correct
      in
        { model | state=add_selected_card model.state selected_card }
    Nothing -> model

add_selected_card : State -> SelectedCard -> State
add_selected_card state selected_card =
  { state | selected_cards = ( Array.push selected_card state.selected_cards ) }

unselect_card : Model -> Int -> Int -> Model
unselect_card model choice_id player_id =
  { model | state = remove_selected_card choice_id player_id model.state }

remove_selected_card : Int -> Int -> State -> State
remove_selected_card choice_id player_id state =
  { state | selected_cards = ( Array.filter ( unmatch_selected_card choice_id player_id ) state.selected_cards ) }
