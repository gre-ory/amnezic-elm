module Type exposing (..)

-- import

import Array exposing (..)

-- types

type alias Model =
  { page: Page
  , questions: Array Question
  , players: Array Player
  }

type alias Question =
  { title: String
  }

type alias Player =
  { name: String
  , score: Int
  }

type Page =
  PageStart
  | PageSetUpPlayers
  | PageSetUpThemes
  | PageSetUpQuestions
  | PageQuestions
  | PageScore
  | PageEnd

type MediaStatus =
  Playing
  | Stopped

type Msg =
  GoToStartPage Model
  | GoToNextPage Model
  | GoToPreviousPage Model
  | UpdatePlayerName Int String
