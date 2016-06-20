module Update exposing (..) -- where 

import Http
import Json.Decode as Json exposing ((:=))
import Task
import String
import Time exposing (Time)

import Model exposing (..)
import Ports exposing (..)

--import Set
--import List.Extra exposing (dropDuplicates)

-- UPDATE



type Msg
  = FetchData
  | FetchSucceed Member
  | UpdateSucceed Member
  | StoreURL String
  | FetchFail Http.Error
  | Tick Time 
  | FetchGitter
  | GitterSuccess (List GRoom) 
  | GitterFail Http.Error
  | GitterIDSuccess (List Gid)
  --| FetchPoints String


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    FetchData ->
      let
        model' = {model|uname = model.name}
      in 
        (model', 
         getData model'.url model'.uname)

    FetchSucceed member ->
      let 
        model' = addToList member model
        clist  = List.map .uname model'.tList
      in 
        -- (model', Ports.modelChange model')
        ( model',
          -- tickRequest (model'.url ++ model'.uname)
          Cmd.batch (List.map (tickRequest model'.url) clist)
        )


    StoreURL name ->
      ({ model | name = String.toLower name }, Cmd.none)

    
    FetchFail err ->
      ( { model | error = True 
                , points = -1
                , uname = ""
                , message = toString err 
        }
        , Cmd.none
      )

    --FetchPoints val -> 
    --  ({ model | points = val, error = False }, Cmd.none )

    Tick newTime -> 
      let
        model' = {model | ts = newTime, uname = model.name}
        --cList = updateList model.tList
        cList = List.map .uname model.tList 

      in
        ( model', 
          -- tickRequest (model'.url ++ model'.uname)
          Cmd.batch (List.map (tickRequest model'.url) cList)
        )

    UpdateSucceed member -> 
      let
        camper = 
          List.filterMap (getCamper member) model.tList
            |> List.head
      in
        case camper of 
          Nothing -> (model, Cmd.none)
          Just(camper) ->
            ({ model |tList = updateCHistory member camper model,
                    tPoints = calculateTotal model.tList, 
                    message = ""
             }
             , Ports.modelChange model
             -- , Cmd.none
            )

    FetchGitter -> 
      (model, refreshGitterIDs gUrl)

    GitterSuccess grooms ->
      let
        gRoom' = List.head (List.filter (\x -> x.name == model.gRoom.name) grooms)
      in
        case gRoom' of 
          Nothing -> (model, Cmd.none) 
          Just(gRoom') ->
            ( { model |gRoom = gRoom'}
              , Cmd.batch (List.map (gitterIDRequest gRoom') 
                  (skipList gRoom'.userCount))
            )

    GitterIDSuccess gids -> 
      let 
        camperList = List.filterMap (createCamperFromGid model.tList) gids 
        model' = { model|tList = model.tList ++ camperList}
        cList  = List.map .uname camperList 
      in
        ( model' 
          , Cmd.batch (List.map (tickRequest model.url) cList)
        )

    GitterFail _ ->
      (model, Cmd.none)

-- HTTP



gitterIDRequest : GRoom -> Int -> Cmd Msg  
gitterIDRequest groom skip = 
  Task.perform GitterFail GitterIDSuccess 
    (Http.get decodeIDData (gUserUrl groom.id gitterKey skip) )


decodeIDData : Json.Decoder (List Gid) 
decodeIDData = 
  Json.at [] (Json.list nestedListGID)


nestedListGID : Json.Decoder Gid  
nestedListGID = 
  Json.object3 Gid
    ("username" := Json.string)
    ("displayName" := Json.string)
    ("avatarUrlSmall" := Json.string)


tickRequest : String -> String -> Cmd Msg
tickRequest url name =
  --Task.perform FetchFail FetchSucceed (Http.get decodePoints url)
  Task.perform FetchFail UpdateSucceed (Http.get decodeData (url ++ name))


getCamper : Member -> Camper -> Maybe Camper
getCamper member camper = 
  if member.uname == camper.uname && 
     member.points /= camper.last.points &&
     not (List.member member.uname excluded ) then
      Just camper 
  else
      Nothing

calculateTotal : List Camper -> Int 
calculateTotal tlist = 
  tlist 
    |> List.filterMap (.chist >> List.head) 
    |> List.map .points 
    |> List.sum


updateCHistory : Member -> Camper -> Model -> List Camper   
updateCHistory member camper model = 
  let 
    data = pointsData member.points model.ts camper.last.points
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
    camper  = createCamper model.ts member 
    model'  = {model| points = member.points,
               tPoints = calculateTotal model.tList, 
               message ="", error = False}
  in 
    case isPresent of 
      True ->  model
      False -> {model' | tList = camper :: model.tList} 


makeRequest : String -> Cmd Msg
makeRequest url =
  Task.perform FetchFail FetchSucceed (Http.get decodeData url)


getData : String -> String -> Cmd Msg 
getData api uname = 
  let 
    url = api ++ uname
  in 
    makeRequest url 


refreshGitterIDs : String -> Cmd Msg 
refreshGitterIDs url = 
  Task.perform GitterFail GitterSuccess (Http.get decodeGData url)


decodeGData : Json.Decoder (List GRoom) 
decodeGData = 
  Json.at [] (Json.list nestedListG)


nestedListG : Json.Decoder GRoom 
nestedListG = 
  Json.object3 GRoom
    ("id" := Json.string)
    ("name" := Json.string)
    ("userCount" := Json.int)



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