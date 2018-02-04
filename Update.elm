module Update exposing (..) -- where 

import Http
import Json.Decode as Json exposing (field)
import Task
import String
import Time exposing (Time)
import Json.Encode as Encode
import Model exposing (..)
import Ports exposing (..)
import Debug exposing (..)

--import Set
--import List.Extra exposing (dropDuplicates)

-- UPDATE



type Msg
  = FetchData
--  | FetchSucceed Member
--  | FetchFail Http.Error
  | FetchOne (Result Http.Error Member)
  | FetchAll (Result Http.Error Member) 
  | UpdateSucceed String (Result Http.Error Member)
  | StoreID String
  | StoreRoom String 
  | Tick Time 
  | GitterStatus (Result Http.Error (List GRoom)) 
  | FetchGitter
--  | GitterFail Http.Error
  | GitterIDStatus (Result Http.Error (List Gid)) 
--  | GitterIDSuccess (List Gid)
  | Set5min Bool 
  | Set15min Bool 
  --| FetchPoints String


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    FetchData ->
      let
        model_ = {model|uname = model.name}
      in 
        (model_, 
         getData fccAPI model_.uname)

    FetchOne (Ok member) ->
      let 
        model_ = addToList member model
        clist  = List.map .uname model_.tList
      in 
        -- (model', Ports.modelChange model')
        ( model_,
          -- tickRequest (model'.url ++ model'.uname)
          Cmd.batch (List.map (tickRequest fccAPI) clist)
        )

    FetchOne (Err err) ->
      ( { model | error = True 
                , points = -1
                , uname = ""
                , message = "User does not exist? " -- toString err 
        }
        , Cmd.none
      )

    FetchAll (Ok member) -> 
      let 
        model_ = addToList member model
        clist  = List.map .uname model_.tList
      in 
        -- (model', Ports.modelChange model')
        ( model_,
          -- tickRequest (model'.url ++ model'.uname)
          Cmd.batch (List.map (tickRequest fccAPI) clist)
        )


    FetchAll (Err error) -> 
      ( { model | error = True 
                , points = -1
                , uname = ""
                --, message = toString error
                , message = "User does not exist?"
        }
        , Cmd.none
      )


    GitterStatus (Ok grooms) ->
      let
        gRoom_ = List.head (List.filter (\x -> x.name == model.gRoom.name) grooms)
      in
        case gRoom_ of 
          Nothing -> (model, Cmd.none) 
          Just(gRoom_) ->
            ( { model |gRoom = gRoom_}
              , Cmd.batch (List.map (gitterIDRequest gRoom_) 
                  (skipList gRoom_.userCount))
            )

    GitterStatus (Err error) -> 
      (model, Cmd.none)

    GitterIDStatus (Ok gids) -> 
      let 
        camperList = List.filterMap (createCamperFromGid model.tList) gids 
        model_ = { model| tList = model.tList ++ camperList 
                        , exclude = excluded
        }
        cList  = List.map .uname camperList 
      in
        ( model_ 
          , Cmd.batch (List.map (tickRequest fccAPI) cList)
        )

    GitterIDStatus (Err error) ->
      (model, Cmd.none)

    -- GitterFail _ ->
    --  (model, Cmd.none)


    -- GitterIDSuccess gids -> 
    {--   let 
        camperList = List.filterMap (createCamperFromGid model.tList) gids 
        model_ = { model|tList = model.tList ++ camperList}
        cList  = List.map .uname camperList 
      in
        ( model_ 
          , Cmd.batch (List.map (tickRequest fccAPI) cList)
        )
    --}

    StoreID name ->
      ({ model | name = String.toLower name }, Cmd.none)

    StoreRoom gname ->
      let
        change = (String.toLower gname) /= model.gRoom.name 
        room = {id="", name= String.toLower gname, userCount=0}
      in 
        ({ model | gRoom = room, roomChange = change }, Cmd.none)
    

    Tick newTime -> 
      let
        model_ = {model | ts = newTime, uname = model.name, tPoints_prev = model.tPoints}
        --cList = updateList model.tList
        cList = List.map .uname model_.tList
          |> List.filter (\x -> not <| List.member x model.exclude)
          
      in
        ( model_
        --, -- tickRequest (model'.url ++ model'.uname)
        , Cmd.batch (List.map (tickRequest fccAPI) cList)
        )

    Set5min bool -> 
      let 
        model_ = {model|min5 = bool, min15 = False}
      in 
        ( model_, Ports.modelChange model_)

    Set15min bool -> 
      let 
        model_ = {model|min15 = bool, min5 = False}
      in 
        ( model_, Ports.modelChange model_)

    UpdateSucceed name (Ok member) -> 
      let
        model_ = 
          { model|tList = updateCHistory model member,
                  tPoints = calculateTotal model.tList, 
                  message = ""
          } 
      in
        (model_, Ports.modelChange model_)
        -- (model', Cmd.none)

    UpdateSucceed name (Err error) -> 
    let 
      _ = Debug.log "Error retrieving for id: " name  
    in
      ( { model | error = True 
                , points = -1
                , uname = ""
                --, message = toString error 
                , message = "User " ++ name ++ " does not exist?" 
                , exclude = 
                    case (List.member name model.exclude) of 
                      True -> model.exclude
                      False -> name :: model.exclude
        }
        , Cmd.none
      )


    FetchGitter -> 
      case model.roomChange of 
        True -> 
          ({model|tList = [], roomChange = False}, refreshGitterIDs gUrl)
        False ->
          (model, refreshGitterIDs gUrl)
    
    
-- HTTP



gitterIDRequest : GRoom -> Int -> Cmd Msg  
gitterIDRequest groom skip = 
  Task.attempt GitterIDStatus
    (Http.toTask  (Http.get (gUserUrl groom.id gitterKey skip) decodeIDData ) )


decodeIDData : Json.Decoder (List Gid) 
decodeIDData = 
  Json.at [] (Json.list nestedListGID)


nestedListGID : Json.Decoder Gid  
nestedListGID = 
  Json.map3 Gid
    (field "username" Json.string)
    (field "displayName" Json.string)
    (field "avatarUrlSmall" Json.string)


tickRequest : String -> String -> Cmd Msg
tickRequest url name =
  --Task.perform FetchFail FetchSucceed (Http.get decodePoints url)
  Task.attempt (UpdateSucceed name) (Http.toTask (Http.get (url ++ name) decodeData ) )
  --Task.attempt UpdateSucceed (getUserData url name)
  

getUserData url name = 
    let
        downloadURL =
            url ++ name
        settings =
            { postSettings | url = downloadURL }  
    in 
        Http.toTask (Http.request settings)


getCamper : Member -> Camper -> Maybe Camper
getCamper member camper = 
  if member.uname == camper.uname && 
     member.points /= camper.last.points then 
      Just camper 
  else
      Nothing

calculateTotal : List Camper -> Int 
calculateTotal tlist = 
  tlist 
    |> List.filterMap (.chist >> List.head) 
    |> List.map .points 
    |> List.sum


updateCHistory : Model -> Member -> List Camper   
updateCHistory model member = 
  let
    tList_ = model.tList 
      |> List.filter (\x -> not <| List.member x.uname model.exclude)
    camper = tList_
      |> List.filterMap (getCamper member)
      |> List.head
  in
    case (camper) of
      Nothing -> tList_ 
      Just (camper) -> 
        let 
          data = pointsData member.points model.ts camper.last.points
          camper_ = {camper| chist = data :: camper.chist, 
                         last  = data 
                }
          model_ = {model |tList = 
            List.filter (\x -> x.uname /= member.uname) tList_
          }
        in
          camper_ :: model_.tList



addToList : Member -> Model -> Model 
addToList member model = 
  let 
    clist = List.map .uname model.tList
    isPresent = List.member member.uname clist
    camper  = createCamper model.ts member 
    model_  = {model| points = member.points,
               tPoints = calculateTotal model.tList, 
               message ="", error = False}
  in 
    case isPresent of 
      True ->  model
      False -> {model_ | tList = camper :: model.tList} 


makeRequest : String -> Cmd Msg
makeRequest url =
  -- Task.perform FetchFail FetchSucceed (Http.get decodeData url))
  Task.attempt FetchOne (Http.toTask (Http.get url decodeData) )


getData : String -> String -> Cmd Msg 
getData api uname = 
  let 
    url = api ++ uname
  in 
    makeRequest url 


refreshGitterIDs : String -> Cmd Msg 
refreshGitterIDs url = 
  Task.attempt GitterStatus (Http.toTask (Http.get url decodeGData) )


decodeGData : Json.Decoder (List GRoom) 
decodeGData = 
  Json.at [] (Json.list nestedListG)


nestedListG : Json.Decoder GRoom 
nestedListG = 
  Json.map3 GRoom
    (field "id" Json.string)
    (field "name" Json.string)
    (field "userCount" Json.int)



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
      Json.map2 Member
        (field "username" Json.string)
        (field "browniePoints" Json.int) 
    )


downloadArgs : List ( String, Encode.Value )
downloadArgs =
    [ ( "path", Encode.string "junk" ) ]


uploadArgs : List ( String, Encode.Value )
uploadArgs =
    downloadArgs ++ [ ( "mode", Encode.string "overwrite" ) ]


stringify : List ( String, Encode.Value ) -> String
stringify =
    Encode.object >> Encode.encode 0


authorizationHeader : Http.Header
authorizationHeader =
    Http.header "Authorization" "Bearer 4bhveELh1l8AAAAAAAAg1hjS4PUDWf0EeED2cIsmOsdJE04uqkichInc0sN0QZao"


-- Missing required request header. Must specify one of: origin,x-requested-with
downloadHeaders : List Http.Header
downloadHeaders =
    [ 
    --Http.header "Access-Control-Allow-Origin" "*"
    --, Http.header "Access-Control-Allow-Credentials" "True"
    Http.header "Access-Control-Allow-Methods" "GET,HEAD,OPTIONS,POST,PUT"
    --, Http.header "Access-Control-Allow-Headers" "Access-Control-Allow-Origin, Access-Control-Allow-Headers, Origin, Content-Type, Accept, Authorization, Access-Control-Request-Method, Access-Control-Request-Headers"
    , Http.header "Access-Control-Allow-Headers" "Origin,  X-Requested-With, Content-Type"
    --, Http.header "Content-Type" "application/json"
    --, authorizationHeader
    --, Http.header "Dropbox-API-Arg" (stringify downloadArgs)
    ]


postSettings =
    { method = "POST"
    , headers = downloadHeaders
    , url = ""
    , body = Http.emptyBody
    -- , expect = expectString
    -- , expect = expectJson decodeFileInfo
    -- , expect = expectStringResponse expectRev
    -- , expect = expectStringResponse fileInfo
    , expect = Http.expectJson decodeData
    , timeout = Nothing
    , withCredentials = False
    }

--{"about":{"username":"kgashok","browniePoints":318,"bio":"Emperor, coffee enthusiast. "}}



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