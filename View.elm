module View exposing (..) -- where 

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Version exposing (version, gitRepo)
import Update exposing (Msg(..)) 
import Model exposing (fccAPI, Model)

-- VIEW


buildResponse : Model -> String 
buildResponse model = 
    if model.error == True
    then "There was an error"
    --else if model.result /= ""
    --then "I just found: " ++ model.result
    else if model.points /= -1
    then "Challenges completed: " ++ (toString model.points)
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
        , div [] [ text (toString model) ]
        , footer [] 
          [a [href (gitRepo ++ "/issues/new"), 
              target "_blank", 
              rel "noopener noreferrer"] [text version] ]
      ]

rStyle : Attribute msg 
rStyle = 
  style 
    [ ("backgroundColor", "#ff6600"),
      ("color", "white"),
      ("fontSize", "300%")
    ]
