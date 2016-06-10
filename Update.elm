module Update exposing (..) -- where 

import Http
import Json.Decode as Json exposing ((:=))
import Task
import String
import Time exposing (Time, second, minute)

import Model exposing (..)
import Ports



-- UPDATE

type Msg
  = FetchData
  | FetchSucceed Member
  | StoreURL String
  | FetchFail Http.Error
  | Tick Time 
  --| FetchPoints String


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    FetchData ->
      let
        model' = {model|uname = model.name}
      in 
        (model', 
         --makeRequest (model'.url ++ model'.uname) )
         getData model'.url model'.uname)

    FetchSucceed member ->
      let 
        model' =  addToList member model
      in 
        (model', Ports.modelChange model')

    StoreURL name ->
      ({ model | name = String.toLower name }, Cmd.none)

    FetchFail _ ->
      ({ model | error = True, points = -1, uname="" }, Cmd.none)

    --FetchPoints val -> 
    --  ({ model | points = val, error = False }, Cmd.none )

    Tick newTime -> 
      let
        model' = {model | ts = newTime, uname = model.name}
        clist  = List.map .uname model'.tList

      in  
        (model', 
         makeRequest (model'.url ++ model'.uname) )


-- HTTP



addToList : Member -> Model -> Model 
addToList member model = 
  let 
    clist = List.map .uname model.tList
    isPresent = List.member member.uname clist
    camper = createCamper member model.ts  
    model' = {model| points = member.points, error = False}
  in 
    case isPresent of 
      True -> model' 
      False -> {model' | tList = camper :: model.tList} 


makeRequest : String -> Cmd Msg
makeRequest url =
  --Task.perform FetchFail FetchSucceed (Http.get decodePoints url)
  Task.perform FetchFail FetchSucceed (Http.get decodeData url)


getData : String -> String -> Cmd Msg 
getData api uname = 
  let 
    url = api ++ uname
  in 
    makeRequest url 


-- decodeTitle
-- return the string from 'title'

decodeTitle : Json.Decoder String
decodeTitle =
  Json.at ["title"] Json.string


decodePoints : Json.Decoder Int
decodePoints = 
  Json.at ["about", "browniePoints"] Json.int 


decodeData : Json.Decoder Member
decodeData =
  Json.at ["about"] 
    (
      Json.object2 Member
        ("username" := Json.string)
        ("browniePoints" := Json.int) 
    )


{-
  https://api.myjson.com/bins/4j9e0?pretty=1 - for kgashok

  {
    "about": {
      "username": "kgashok",
      "browniePoints": 174,
      "bio": ""
    }
  }

-}