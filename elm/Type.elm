module Type exposing (..)

-- import

import Array exposing (..)
import Keyboard exposing (..)

-- types

type alias Model = {
  questions: Array Question,
  players: Array Player,
  state: State,
  available_card_types: Array String,
  available_card_colors: Array String
}

type alias State = {
  page: Page,
  question_id: Int,
  step: Step,
  media_status: MediaStatus,
  selected_cards: Array SelectedCard,
  score_mode: ScoreMode
}

type alias Question = {
  theme: String,
  audio: String,
  choices: Array Choice
}

type alias Choice = {
  answer: String,
  hint: String,
  correct: Bool
}

type alias Player = {
  name: String,
  score: Int,
  active: Bool,
  card_type: String,
  card_color: String
}

type alias SelectedCard = {
  choice_id: Int,
  player_id: Int,
  engaged_point: Int,
  correct: Bool
}

type Page =
  PageStart
  | PageThemes
  | PagePlayers
  | PageQuestions
  | PageScore
  | PageEnd

type Step =
  StepNotReady
  | StepShowChoices
  | StepShowHints
  | StepShowCorrect
  | StepShowCards
  | StepShowScore

type MediaStatus =
  MediaNotReady
  | MediaReady
  | MediaPlay
  | MediaPause
  | MediaEnd

type ScoreMode =
  ScoreByVelocity
  | ScoreByVelocityCappedByRank

-- key

type Key
  = Space
  | ArrowLeft
  | ArrowRight
  | Unknown

-- messages

type Msg =
  GoToPage Page
  | GoToNextPage
  | GoToPreviousPage
  | MoveForward
  | AddPlayer
  | DeactivatePlayer Int
  | ActivatePlayer Int
  | DeletePlayer Int
  | UpdatePlayerName Int String
  | UnselectCardColor Int
  | SelectCardColor Int String
  | UnselectCardType Int
  | SelectCardType Int String
  | SelectCard Int Int
  | UnselectCard Int Int
  | OnKey KeyCode
  | NothingToDo

-- id

id_to_nb : Int -> Int
id_to_nb id =
  id + 1

-- card suit

-- get_card_suit : Model -> Int -> Maybe CardSuit
-- get_card_suit model card_suit_id =
--   Array.get card_suit_id model.card_suits

match_card_type_and_color : String -> String -> Player -> Bool
match_card_type_and_color card_type card_color player =
  if not ( String.isEmpty card_type ) && not ( String.isEmpty card_color ) then
    ( player.card_type == card_type ) && ( player.card_color == card_color )
  else
    False

is_card_type_and_color_already_selected : String -> String -> Model -> Bool
is_card_type_and_color_already_selected card_type card_color model =
  List.any ( match_card_type_and_color card_type card_color ) ( Array.toList model.players )

has_card_type_and_color : Player -> Bool
has_card_type_and_color player =
  not ( String.isEmpty player.card_type ) && not ( String.isEmpty player.card_color )

all_player_has_card_type_and_color : Model -> Bool
all_player_has_card_type_and_color model =
  List.all ( has_card_type_and_color ) ( Array.toList model.players )

-- player

get_player : Model -> Int -> Maybe Player
get_player model player_id =
  Array.get player_id model.players

can_add_player : Model -> Bool
can_add_player model =
  Array.length model.players < 8

can_deactivate_player : Model -> Int -> Bool
can_deactivate_player model player_id =
  if Array.length model.players > 2 then
    case get_player model player_id of
      Just player -> player.active
      Nothing -> False
  else
    False

can_activate_player : Model -> Int -> Bool
can_activate_player model player_id =
  case get_player model player_id of
    Just player -> not player.active
    Nothing -> False

can_delete_player : Model -> Int -> Bool
can_delete_player model player_id =
  if Array.length model.players > 2 then
    case get_player model player_id of
      Just player -> not player.active
      Nothing -> False
  else
    False

-- question

get_question : Model -> Maybe Question
get_question model =
  Array.get model.state.question_id model.questions

-- choice

get_choice : Model -> Int -> Maybe Choice
get_choice model choice_id =
  case get_question model of
    Just question -> Array.get choice_id question.choices
    Nothing -> Nothing

-- selected card

match_selected_card : Int -> Int -> SelectedCard -> Bool
match_selected_card choice_id player_id selected_card =
  ( selected_card.choice_id == choice_id ) && ( selected_card.player_id == player_id )

unmatch_selected_card : Int -> Int -> SelectedCard -> Bool
unmatch_selected_card choice_id player_id selected_card =
  not ( match_selected_card choice_id player_id selected_card )

has_selected_card : Int -> Int -> Model -> Bool
has_selected_card choice_id player_id model =
  List.any ( match_selected_card choice_id player_id ) ( Array.toList model.state.selected_cards )

-- navigation

previous_page: Page -> Page
previous_page page =
  case page of
    PageStart -> PageStart
    PageThemes -> PageStart
    PagePlayers -> PageThemes
    PageQuestions -> PagePlayers
    PageScore -> PageQuestions
    PageEnd -> PageScore

next_page: Page -> Page
next_page page =
  case page of
    PageStart -> PageThemes
    PageThemes -> PagePlayers
    PagePlayers -> PageQuestions
    PageQuestions -> PageScore
    PageScore -> PageEnd
    PageEnd -> PageStart

can_change_page: Model -> Bool
can_change_page model =
  case model.state.page of
    PagePlayers -> all_player_has_card_type_and_color model
    _ -> True

can_go_to_page: Model -> Page -> Bool
can_go_to_page model target_page =
  if can_change_page model then
    case model.state.page of
      PageStart -> List.member target_page [ PageThemes ]
      PageThemes -> List.member target_page [ PagePlayers ]
      PagePlayers -> List.member target_page [ PagePlayers, PageQuestions, PageScore ]
      PageQuestions -> List.member target_page [ PagePlayers, PageQuestions, PageScore ]
      PageScore -> List.member target_page [ PagePlayers, PageQuestions, PageScore, PageEnd ]
      PageEnd -> List.member target_page [ PagePlayers, PageQuestions, PageScore, PageEnd, PageStart ]
  else
    False

can_go_to_previous_page: Model -> Bool
can_go_to_previous_page model =
  can_go_to_page model ( previous_page model.state.page )

can_go_to_next_page: Model -> Bool
can_go_to_next_page model =
  can_go_to_page model ( next_page model.state.page )

can_move_forward: Model -> Bool
can_move_forward model =
  if model.state.page == PageQuestions then
    can_go_to_next_step model
  else
    can_go_to_next_page model

can_move_backward: Model -> Bool
can_move_backward model =
  can_go_to_previous_page model

can_go_to_next_step: Model -> Bool
can_go_to_next_step model =
  case model.state.step of
    StepNotReady -> True
    StepShowChoices -> True
    StepShowHints -> True
    StepShowCorrect -> True
    StepShowCards -> True
    StepShowScore -> True

show_result : Step -> Bool
show_result step =
  case step of
    StepNotReady -> False
    StepShowChoices -> False
    StepShowHints -> False
    StepShowCorrect -> True
    StepShowCards -> True
    StepShowScore -> True
