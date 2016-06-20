module View exposing (..) -- where 

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Version exposing (version, gitRepo)
import Update exposing (Msg(..)) 
import Model exposing (..)
import Time 
import Date exposing (..)
import Date.Format exposing (formatISO8601)

import Numeral exposing (format)
import String

-- VIEW


buildResponse : Model -> String 
buildResponse model = 
  let 
    now = fromTime model.ts
    shour = format "00" (toFloat (hour now))
    smin  = format "00" (toFloat (minute now))
    dateString = formatISO8601 now 
    
  in 
    --if model.error == True
    --then "Error: userID not valid? " ++ model.message 
    if model.tPoints /= -1
    then "Challenges completed: " ++ (toString model.tPoints) ++
          " by " ++ (toString (List.length model.tList)) ++ " campers; " ++
          "last auto update @ " ++ dateString 
    else "" 


view : Model -> Html Msg
view model =
  let
    response = buildResponse model
    clist    = List.map .uname model.tList
  in
    div []
      [ h2 [] [ text "CamperBot for KGISL Meetups"]
      , footer
      , hr [] [ ]
      --, p [] [ text "Here I want to grab the ''browniePoints''"]
      --, p [] [ text ("FCC URL: " ++ fccAPI) ]
      , input [
          placeholder "Enter a FCC username",
          onInput StoreURL
        ] []
        , button [ onClick FetchData ] [ text "Fetch and Add!" ]
        , button [ onClick FetchGitter ] [text "Update from Gitter"]
        , h1 [rStyle]  [ text response ]
        --, div [] [ text (toString model.gRoom) ]
        --, div [] [ text (toString model.gList) ]
        , campList True model.tList
      ]


rStyle : Attribute msg 
rStyle = 
  style 
    [ ("backgroundColor", "#ff6600"),
      ("color", "white"),
      ("fontSize", "150%")
    ]

{-formatData : Time.Time -> Cdata -> String -- (Int, String)
formatData firstEntry cdata = 
  let
    timeLapsed = Time.inHours (cdata.ts - firstEntry.ts)
  in 
    case (cdata.ts, timeLapsed) of
      (0,_) -> toString cdata.points
      (_,0) -> toString cdata.points 
      (_,_)-> (toString cdata.delta) ++ 
              "(" ++ format "0.00" timeLapsed ++ ")"
-}

formatData : Maybe Cdata -> Cdata -> String -- (Int, String)
formatData prevEntry cdata = 
  case (prevEntry) of
    Nothing -> 
      toString cdata.points 
    Just(prevEntry) ->
      let
        timeLapsed = Time.inHours (cdata.ts - prevEntry.ts)
      in 
        case (cdata.ts, timeLapsed /=0 && timeLapsed >= -720) of
          (0,_) -> toString cdata.points
          (_,False) -> toString cdata.points 
          (_,True)-> (toString cdata.delta) ++ 
                  "(" ++ format "+0.00" timeLapsed ++ ")"



camperItem : Camper -> Html Msg
camperItem camper = 
  let 
    history = List.take 10 camper.chist
    prev = List.head history
    --points  = String.join ", " (List.map (formatData camper.last.ts) history )
    points  = String.join ", " (List.map (formatData prev) history )
  in 
    li []
      [ span [ class "uname" ] [ text camper.uname ],
        span [ class "points" ] [ text points ]
      ]


campList : Bool -> List Camper -> Html Msg
campList display campers = 
  let
    -- campers_ = List.sortWith flippedComparison2 campers
    campers' = List.sortWith flippedComparison campers
    items    = List.map camperItem campers'

  in
    div [] 
      [ 
        ul [] items 
      ]

flippedComparison2: Camper -> Camper -> Order
flippedComparison2 a b = 
  case compare a.last.points b.last.points of 
      GT -> LT
      EQ -> EQ
      LT -> GT



flippedComparison : Camper -> Camper -> Order
flippedComparison a b =
  let 
    ahist = List.map .points a.chist 
    bhist = List.map .points b.chist 

    deltaA = Maybe.withDefault 0 (List.maximum ahist) 
              - Maybe.withDefault 0 (List.minimum ahist)
    deltaB = Maybe.withDefault 0 (List.minimum bhist)
              - Maybe.withDefault 0 (List.minimum bhist)

  in
    case compare deltaA deltaB of
      LT -> GT
      EQ -> EQ
      GT -> LT


footer : Html Msg
footer = 
  div [id "footer"]
  [a [href (gitRepo ++ "/issues/new"), 
              target "_blank", 
              rel "noopener noreferrer"] 
              [text version]]

