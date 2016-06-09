module Subscriptions exposing (..) --where 

import Time exposing (Time, second, minute)

import Model exposing (Model)
import Update exposing (Msg(..))

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (30 * second) Tick



{-subscriptions : Model -> Sub Msg
subscriptions =
  \_ -> Sub.none
-}
