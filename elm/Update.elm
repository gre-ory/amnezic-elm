
module Update exposing (..)

-- import

import Array exposing (..)

import Type exposing (..)

-- page

go_to_start_page: Model -> Model
go_to_start_page model =
  { model | page=PageStart }

go_to_next_page: Model -> Model
go_to_next_page model =
  case model.page of
    PageStart -> { model | page=PageSetUpPlayers }
    PageSetUpPlayers -> { model | page=PageSetUpThemes }
    PageSetUpThemes -> { model | page=PageSetUpQuestions }
    PageSetUpQuestions -> { model | page=PageQuestions }
    PageQuestions -> { model | page=PageScore }
    PageScore -> { model | page=PageEnd }
    PageEnd -> { model | page=PageStart }

go_to_previous_page: Model -> Model
go_to_previous_page model =
  case model.page of
    PageStart -> { model | page=PageEnd }
    PageSetUpPlayers -> { model | page=PageStart }
    PageSetUpThemes -> { model | page=PageSetUpPlayers }
    PageSetUpQuestions -> { model | page=PageSetUpThemes }
    PageQuestions -> { model | page=PageSetUpQuestions }
    PageScore -> { model | page=PageQuestions }
    PageEnd -> { model | page=PageScore }

-- set-up player

init_default_players : Array Player
init_default_players =
  Array.fromList [ init_default_player 1, init_default_player 2 ]

init_default_player : Int -> Player
init_default_player player_id =
  { name= ( "Player " ++ ( toString player_id ) )
  , score= 0
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
  { title= ( "Question " ++ ( toString question_id ) )
  }
