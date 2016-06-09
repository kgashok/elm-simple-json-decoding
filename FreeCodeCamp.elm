module Fcc exposing (..) -- where 

import Html.App as Html

import Model exposing (initialModel, fccAPI, Model) 
import View exposing (view) 
import Update exposing (update, Msg) 

import Ports

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    ( nextModel, nextCmd ) =
      Update.update msg model
  in
    ( nextModel
    , Cmd.batch
      [ Ports.logExternal msg
      -- , Ports.modelChange model
      , nextCmd
      ]
    )


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  ( Maybe.withDefault initialModel savedModel, Cmd.none )


main : Program (Maybe Model)
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions =
  \_ -> Sub.none


