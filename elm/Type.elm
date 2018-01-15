module Type exposing (..)

-- import

import Array exposing (..)
import Keyboard exposing (..)

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

-- key

type Key
  = Space
  | ArrowLeft
  | ArrowRight
  | Unknown

get_key : Int -> Key
get_key key_code =
  case key_code of
    32 -> Space
    37 -> ArrowLeft
    39 -> ArrowRight
    _ -> Unknown

-- messages

type Msg =
  GoToStartPage Model
  | GoToNextPage Model
  | GoToPreviousPage Model
  | UpdatePlayerName Int String
  | OnKey KeyCode
