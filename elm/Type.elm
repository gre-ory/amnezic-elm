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
  media_status: MediaStatus
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

type Page =
  PageStart
  | PageThemes
  | PagePlayers
  | PageQuestions
  | PageScore
  | PageEnd

type Step =
  StepNotReady
  | StepReady
  | StepShowChoices
  | StepShowHints
  | StepShowCorrect
  | StepShowCards
  | StepShowScore
  | StepEnd

type MediaStatus =
  MediaNotReady
  | MediaReady
  | MediaPlay
  | MediaPause
  | MediaEnd

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
  | OnKey KeyCode
