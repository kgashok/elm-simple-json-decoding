module Subscriptions exposing (..) --where 

import Time exposing (Time, second, minute, hour)

import Model exposing (Model)
import Update exposing (Msg(..))

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  --Time.every minute Tick
  Time.every (45 * second) Tick



{-subscriptions : Model -> Sub Msg
subscriptions =
  \_ -> Sub.none
-}
