module View exposing (..) -- where 

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Version exposing (version, gitRepo)
import Update exposing (Msg(..)) 
import Model exposing (..)
import Time 

import Numeral exposing (format)
import String

-- VIEW


buildResponse : Model -> String 
buildResponse model = 
  if model.error == True
  then "There was an error"
  --else if model.result /= ""
  --then "I just found: " ++ model.result
  else if model.points /= -1
  then "Challenges completed: " ++ (toString model.tPoints)
  else "" 


view : Model -> Html Msg
view model =
  let
    response = buildResponse model
    clist    = List.map .uname model.tList
  in
    div []
      [ h3 [] [ text "Simple Object"]
      , p [] [ text "Here I want to grab the ''browniePoints''"]
      , p [] [ text ("FCC URL: " ++ fccAPI) ]
      , input [
          placeholder "Enter a FCC username",
          onInput StoreURL
        ] []
        , button [ onClick FetchData ] [ text "Fetch!" ]
        , h1 [rStyle]  [ text response ]
        -- , div [] [ text (toString model) ]
        , campList True model.tList
        , footer
      ]
          

rStyle : Attribute msg 
rStyle = 
  style 
    [ ("backgroundColor", "#ff6600"),
      ("color", "white"),
      ("fontSize", "100%")
    ]

formatData : Time.Time -> Cdata -> String -- (Int, String)
formatData nowTime cdata = 
  let
    timeLapsed = Time.inMinutes (cdata.ts - nowTime)    
  in 
    case (cdata.ts, timeLapsed) of
      (0,_) -> toString cdata.points
      (_,0) -> toString cdata.points 
      (_,_)-> (toString cdata.points) ++ 
              "(" ++ format "00.0" timeLapsed ++ ")"


camperItem : Camper -> Html Msg
camperItem camper = 
  let 
    history = List.take 10 camper.chist
    points  = String.join ", " (List.map (formatData camper.last.ts) history )
  in 
    li []
      [ span [ class "uname" ] [ text camper.uname ],
        span [ class "points" ] [ text points ]
      ]


campList : Bool -> List Camper -> Html Msg
campList display campers = 
  let
    sortedCampers = List.sortWith flippedComparison campers
    items  = List.map camperItem sortedCampers

  in
    div [] 
      [ 
        ul [] items 
      ]

flippedComparison : Camper -> Camper -> Order
flippedComparison a b =
    case compare a.last.points b.last.points of
      LT -> GT
      EQ -> EQ
      GT -> LT


footer : Html Msg
footer = 
  div []
  [a [href (gitRepo ++ "/issues/new"), 
              target "_blank", 
              rel "noopener noreferrer"] 
              [text version]]



{-renderCamper : Camper -> Html Msg
renderCamper member =
  div [ class "member" ] [
    img [ classNames ["profile", "u-mr-sml"], src member.avatar_lrg ] []
    , span [ class "username" ] [ text ("@" ++ member.name) ]
  ]
-}