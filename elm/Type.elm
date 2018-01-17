module Type exposing (..)

-- import

import Array exposing (..)
import Keyboard exposing (..)

-- types

type alias Model = {
  questions: Array Question,
  players: Array Player,
  state: State
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
  score: Int
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
  GoToStartPage Model
  | GoToNextPage Model
  | GoToPreviousPage Model
  | UpdatePlayerName Int String
  | SelectCard Int Int
  | UnselectCard Int Int
  | OnKey KeyCode

-- helper

id_to_nb : Int -> String
id_to_nb id =
  toString( id + 1 )

get_player : Model -> Int -> Maybe Player
get_player model player_id =
  Array.get player_id model.players

get_question : Model -> Maybe Question
get_question model =
  Array.get model.state.question_id model.questions

get_choice : Model -> Int -> Maybe Choice
get_choice model choice_id =
  case get_question model of
    Just question -> Array.get choice_id question.choices
    Nothing -> Nothing

match_selected_card : Int -> Int -> SelectedCard -> Bool
match_selected_card choice_id player_id selected_card =
  ( selected_card.choice_id == choice_id ) && ( selected_card.player_id == player_id )

unmatch_selected_card : Int -> Int -> SelectedCard -> Bool
unmatch_selected_card choice_id player_id selected_card =
  not ( match_selected_card choice_id player_id selected_card )

has_selected_card : Int -> Int -> Model -> Bool
has_selected_card choice_id player_id model =
  List.any ( match_selected_card choice_id player_id ) ( Array.toList model.state.selected_cards )

show_result : Step -> Bool
show_result step =
  case step of
    StepNotReady -> False
    StepShowChoices -> False
    StepShowHints -> False
    StepShowCorrect -> True
    StepShowCards -> True
    StepShowScore -> True
