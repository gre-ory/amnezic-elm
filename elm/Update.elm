
module Update exposing (..)

-- import

import Array exposing (..)

import Type exposing (..)

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
  { model | page=PageStart, question_id=0 }

go_to_start_page: Model -> Model
go_to_start_page model =
  reset_game model

go_to_next_page: Model -> Model
go_to_next_page model =
  case model.page of
    PageStart -> { model | page=PageThemes }
    PageThemes -> { model | page=PagePlayers }
    PagePlayers -> { model | page=PageQuestions }
    PageQuestions ->
      if model.question_id+1 < Array.length( model.questions ) then
        { model | question_id=model.question_id+1 }
      else
        { model | page=PageScore }
    PageScore -> { model | page=PageEnd }
    PageEnd -> reset_game model

go_to_previous_page: Model -> Model
go_to_previous_page model =
  case model.page of
    PageStart -> model
    PageThemes -> model
    PagePlayers -> model
    PageQuestions -> { model | page=PagePlayers }
    PageScore -> { model | page=PageQuestions }
    PageEnd -> { model | page=PageScore }

-- set-up player

init_default_players : Array Player
init_default_players =
  Array.fromList [ init_default_player 1, init_default_player 2 ]

init_default_player : Int -> Player
init_default_player player_id =
  { name = ( "Player " ++ ( toString player_id ) )
  , score = 0
  }

update_player : Model -> Int -> ( Player -> Player ) -> Model
update_player model player_id update_player_fn =
  case Array.get player_id model.players of
    Just player ->
      { model | players = Array.set player_id ( update_player_fn player ) model.players }
    Nothing
      -> model

update_player_name : String -> Player -> Player
update_player_name player_name player =
  { player | name=player_name }

  -- set-up question

init_default_questions: Array Question
init_default_questions =
  Array.fromList [ init_default_question 0, init_default_question 1, init_default_question 2 ]

init_default_question : Int -> Question
init_default_question question_id =
  { theme = ( "Theme " ++ ( toString question_id ) )
  , audio = ( "audio_" ++ ( toString question_id ) ++ ".mp3" )
  , choices = Array.fromList [ init_default_choice 0 False, init_default_choice 1 True, init_default_choice 2 False, init_default_choice 3 False ]
  }

-- set-up choice

init_default_choice : Int -> Bool -> Choice
init_default_choice choice_id correct =
  { answer = ( "Answer " ++ ( toString choice_id ) )
  , hint = ( "Hint " ++ ( toString choice_id ) )
  , correct = correct
  }
