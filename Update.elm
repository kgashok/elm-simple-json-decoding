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
  | FetchSucceed Int
  | StoreURL String
  | FetchFail Http.Error
  | Tick Time 
  --| FetchPoints String


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    FetchData ->
      let
        model_ = {model|uname = model.name}
        model' =  addToList model_.name model_
      in 
        (model', 
         makeRequest (model'.url ++ model'.uname) )

    FetchSucceed val ->
      let 
        model' = {model | points = val, error = False}
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

addToList : String -> Model -> Model 
addToList n model = 
  let 
    clist = List.map .uname model.tList
    isPresent = List.member n clist
    camper = createCamper n 
  in 
    case isPresent of 
      True -> model 
      False -> {model | tList = camper :: model.tList} 


makeRequest : String -> Cmd Msg
makeRequest url =

  Task.perform FetchFail FetchSucceed (Http.get decodePoints url)


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
