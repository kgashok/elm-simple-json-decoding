module Subscriptions exposing (..) --where 

import Time exposing (Time, second, minute, hour)

import Model exposing (Model)
import Update exposing (Msg(..))

-- import Version exposing (interval)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  --Time.every minute Tick
  case (model.min15) of
    True -> Time.every (15 * minute) Tick
    False -> Time.every (5 * minute) Tick



{-subscriptions : Model -> Sub Msg
subscriptions =
  \_ -> Sub.none
-}
