
module Init exposing (..)

-- import

import Array exposing (..)

import Type exposing (..)

-- init

init_model : Model
init_model =
  { questions = init_default_questions
  , players = init_default_players
  , state = init_state
  , card_suits = Array.fromList [ Heart, Diamond, Club, Spade, TarotHeart, TarotTrump, UnoBlue, UnoYellow ]
  }

init_state : State
init_state =
  { page = PageStart
  , question_id = 0
  , step = StepNotReady
  , media_status = MediaNotReady
  , selected_cards = Array.fromList [ ]
  , score_mode = ScoreByVelocity
  }

init_default_players : Array Player
init_default_players =
  Array.initialize 2 ( init_default_player True )

init_default_player : Bool -> Int -> Player
init_default_player with_card_suit player_id =
  { name = ( "Player " ++ toString( id_to_nb player_id ) )
  , score = 0
  , active = True
  , maybe_card_suit_id = ( if with_card_suit then Just ( player_id ) else Nothing )
  }

init_default_questions: Array Question
init_default_questions =
  Array.initialize 4 init_default_question

init_default_question : Int -> Question
init_default_question question_id =
  { theme = ( "Theme " ++ toString( id_to_nb question_id ) )
  , audio = ( "audio_" ++ toString( id_to_nb question_id ) ++ ".mp3" )
  , choices = Array.fromList [ init_default_choice 0 False, init_default_choice 1 True, init_default_choice 2 False, init_default_choice 3 False ]
  }

init_default_choice : Int -> Bool -> Choice
init_default_choice choice_id correct =
  { answer = ( "Answer " ++ toString( id_to_nb choice_id ) )
  , hint = ( "Hint " ++ toString( id_to_nb choice_id ) )
  , correct = correct
  }

init_selected_card : Int -> Int -> Bool -> SelectedCard
init_selected_card choice_id player_id correct =
  { choice_id = choice_id
  , player_id = player_id
  , engaged_point = 0
  , correct = correct }
