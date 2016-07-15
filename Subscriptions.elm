module Subscriptions exposing (..) --where 

import Time exposing (Time, second, minute, hour)

import Model exposing (Model)
import Update exposing (Msg(..))

-- import Version exposing (interval)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  --Time.every minute Tick
  Time.every (15 * minute) Tick



{-subscriptions : Model -> Sub Msg
subscriptions =
  \_ -> Sub.none
-}
