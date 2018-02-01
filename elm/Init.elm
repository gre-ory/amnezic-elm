
module Init exposing (..)

-- import

import Html exposing (..)
import Array exposing (..)

import Type exposing (..)

-- init

init_model : Model
init_model =
  { questions = init_default_questions
  , players = init_default_players
  , state = init_state
  , available_card_suits = Array.fromList [ Club, Spade, Heart, Diamond, Star, Dot, Square ]
  , available_card_colors = Array.fromList [ Black, Red, Yellow, Blue, Green ]
  }

init_state : State
init_state =
  { page = PagePlayers -- PageStart
  , question_id = 0
  , step = StepNotReady
  , media_status = MediaNotReady
  , selected_cards = Array.fromList [ ]
  , score_mode = ScoreByVelocity
  , maybe_modal_player_id = Nothing
  }

init_default_players : Array Player
init_default_players =
  Array.fromList (
    []
      |> \l -> ( l ++ [ ( init_default_player Heart Red False 0 ) ] )
      |> \l -> ( l ++ [ ( init_default_player Spade Black False 1 ) ] )
      |> \l -> ( l ++ [ ( init_default_player Diamond Red False 2 ) ] )
      |> \l -> ( l ++ [ ( init_default_player Club Black False 3 ) ] )
      |> \l -> ( l ++ [ ( init_default_player Heart Green False 4 ) ] )
      |> \l -> ( l ++ [ ( init_default_player Square Yellow False 5 ) ] )
      |> \l -> ( l ++ [ ( init_default_player Star Black False 6 ) ] )
      |> \l -> ( l ++ [ ( init_default_player Dot Blue False 7 ) ] )
  )
  -- Array.fromList [ , ( init_default_player Spade Black 1 ) ]

init_default_player : CardSuit -> CardColor -> Bool -> Int -> Player
init_default_player card_suit card_color inverted_color player_id =
  { name = ( "Player " ++ toString( id_to_nb player_id ) )
  , score = 0
  , active = True
  , card = ( init_card card_suit card_color inverted_color )
  }

init_default_questions: Array Question
init_default_questions =
  Array.initialize 4 init_default_question

init_default_question : Int -> Question
init_default_question question_id =
  { theme = ( "Theme " ++ toString( id_to_nb question_id ) )
  , audio = ( "audio_" ++ toString( id_to_nb question_id ) ++ ".mp3" )
  , choices = Array.fromList (
      []
        |> \l -> ( l ++ [ ( init_default_choice 0 False ) ] )
        |> \l -> ( l ++ [ ( init_default_choice 1 True ) ] )
        |> \l -> ( l ++ [ ( init_default_choice 2 False ) ] )
        |> \l -> ( l ++ [ ( init_default_choice 3 True ) ] )
        |> \l -> ( l ++ [ ( init_default_choice 4 False ) ] )
        |> \l -> ( l ++ [ ( init_default_choice 5 False ) ] )
    )
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

init_card : CardSuit -> CardColor -> Bool -> Card
init_card card_suit card_color inverted_color =
  { card_suit = card_suit
  , card_color = card_color
  , inverted_color = inverted_color
  }
