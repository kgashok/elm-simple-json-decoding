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
  | UpdateSucceed Member
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
        model' = addToList member model
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
        ( model', 
          tickRequest (model'.url ++ model'.uname)
        )    

    UpdateSucceed member -> 
      let
        camper = 
          List.filterMap (getCamper member.uname) model.tList
            |> List.head
      in
        case camper of 
          Nothing -> (model, Cmd.none)
          Just(camper) ->
            ({ model |tList = updateCHistory member camper model,
                    tPoints = calculateTotal model.tList  
             }
             , Ports.modelChange model
            )


-- HTTP

getCamper : String -> Camper -> Maybe Camper
getCamper name camper = 
  if name == camper.uname then
      Just camper 
  else
      Nothing

calculateTotal : List Camper -> Int 
calculateTotal tlist = 
  tlist 
    |> List.filterMap (.chist >> List.head) 
    |> List.map .points 
    |> List.sum



tickRequest : String -> Cmd Msg
tickRequest url =
  --Task.perform FetchFail FetchSucceed (Http.get decodePoints url)
  Task.perform FetchFail UpdateSucceed (Http.get decodeData url)


updateCHistory : Member -> Camper -> Model -> List Camper   
updateCHistory member camper model = 
  let 
    data = pointsData member.points model.ts
    camper' = {camper| chist = data :: camper.chist, 
                       last  = data 
              }
    model' = {model |tList = 
      List.filter (\x -> x.uname /= member.uname) model.tList
    }

  in
    camper' :: model'.tList



addToList : Member -> Model -> Model 
addToList member model = 
  let 
    clist = List.map .uname model.tList
    isPresent = List.member member.uname clist
    camper  = createCamper member model.ts
    model'  = {model| points = member.points,
               tPoints = calculateTotal model.tList, error = False}
  in 
    case isPresent of 
      True ->  model
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