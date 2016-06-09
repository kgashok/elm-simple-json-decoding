module Update exposing (..) -- where 

import Http
import Json.Decode as Json exposing ((:=))
import Task
import String


import Model exposing (fccAPI, Model)



-- UPDATE

type Msg
  = FetchData
  | FetchSucceed Int
  | StoreURL String
  | FetchFail Http.Error
  --| FetchPoints String


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    FetchData ->
      (model, makeRequest (model.url ++ model.uname) )

    FetchSucceed val ->
      ({ model | points = val, error = False }, Cmd.none)

    StoreURL uname ->
      ({ model | uname = String.toLower uname }, Cmd.none)

    FetchFail _ ->
      ({ model | error = True, points = -1, uname="" }, Cmd.none)

    --FetchPoints val -> 
    --  ({ model | points = val, error = False }, Cmd.none )


-- HTTP

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
