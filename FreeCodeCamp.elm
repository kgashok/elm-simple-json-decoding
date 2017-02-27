module Fcc exposing (..) -- where 

import Html exposing (..) 

import Model exposing (initialModel, fccAPI, Model) 
import View exposing (view) 
import Update exposing (update, Msg) 
import Subscriptions exposing (subscriptions)

import Ports exposing (..)

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

main : Program (Maybe Model) Model Msg
main =
  Html.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




