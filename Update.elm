module Update exposing (..)

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


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FetchData ->
            let
                model_ =
                    { model | uname = model.name }
            in
                ( model_
                , getData fccAPI model_.uname
                )

        FetchOne (Ok member) ->
            let
                model_ =
                    addToList member model

                clist =
                    List.map .uname model_.tList
            in
                -- (model', Ports.modelChange model')
                ( model_
                , -- tickRequest (model'.url ++ model'.uname)
                  Cmd.batch (List.map (tickRequest fccAPI) clist)
                )

        FetchOne (Err err) ->
            ( { model
                | error = True
                , points = -1
                , uname = ""
                , message = "User does not exist? " -- toString err
              }
            , Cmd.none
            )

        FetchAll (Ok member) ->
            let
                model_ =
                    addToList member model

                clist =
                    List.map .uname model_.tList
            in
                -- (model', Ports.modelChange model')
                ( model_
                , -- tickRequest (model'.url ++ model'.uname)
                  Cmd.batch (List.map (tickRequest fccAPI) clist)
                )

        FetchAll (Err error) ->
            ( { model
                | error = True
                , points = -1
                , uname = ""

                --, message = toString error
                , message = "User does not exist?"
              }
            , Cmd.none
            )

        GitterStatus (Ok grooms) ->
            let
                gRoom_ =
                    List.head (List.filter (\x -> x.name == model.gRoom.name) grooms)
            in
                case gRoom_ of
                    Nothing ->
                        ( model, Cmd.none )

                    Just gRoom_ ->
                        ( { model | gRoom = gRoom_ }
                        , Cmd.batch (gitterIDBatchRequest gRoom_)
                        )

        GitterStatus (Err error) ->
            ( model, Cmd.none )

        GitterIDStatus (Ok gids) ->
            let
                camperList =
                    List.filterMap (createCamperFromGid model.tList) gids

                model_ =
                    { model
                        | tList = model.tList ++ camperList
                        , exclude = excluded
                    }

                cList =
                    List.map .uname camperList
            in
                ( model_
                , Cmd.batch (List.map (tickRequest fccAPI) cList)
                )

        GitterIDStatus (Err error) ->
            ( model, Cmd.none )

        -- GitterFail _ ->
        --  (model, Cmd.none)
        -- GitterIDSuccess gids ->
        {--let
        camperList = List.filterMap (createCamperFromGid model.tList) gids
        model_ = { model|tList = model.tList ++ camperList}
        cList  = List.map .uname camperList
      in
        ( model_
          , Cmd.batch (List.map (tickRequest fccAPI) cList)
        )
    --}
        StoreID name ->
            ( { model | name = String.toLower name }, Cmd.none )

        StoreRoom gname ->
            let
                change =
                    (String.toLower gname) /= model.gRoom.name

                room =
                    { id = "", name = String.toLower gname, userCount = 0 }
            in
                ( { model | gRoom = room, roomChange = change }, Cmd.none )

        Tick newTime ->
            let
                model_ =
                    { model | ts = newTime, uname = model.name, tPoints_prev = model.tPoints }

                --cList = updateList model.tList
                cList =
                    List.map .uname model_.tList
                        |> List.filter (\x -> not <| List.member x model.exclude)
            in
                ( model_
                  --, -- tickRequest (model'.url ++ model'.uname)
                , Cmd.batch (List.map (tickRequest fccAPI) cList)
                )

        Set5min bool ->
            let
                model_ =
                    { model | min5 = bool, min15 = False }
            in
                ( model_, Ports.modelChange model_ )

        Set15min bool ->
            let
                model_ =
                    { model | min15 = bool, min5 = False }
            in
                ( model_, Ports.modelChange model_ )

        UpdateSucceed name (Ok member) ->
            let
                model_ =
                    { model
                        | tList = updateCHistory model member
                        , tPoints = calculateTotal model.tList
                        , message = ""
                    }
            in
                ( model_, Ports.modelChange model_ )

        -- (model', Cmd.none)
        UpdateSucceed name (Err error) ->
            let
                _ =
                    Debug.log "Error retrieving for id: " name
            in
                ( { model
                    | error = True
                    , points = -1
                    , uname = ""

                    --, message = toString error
                    , message = "User " ++ name ++ " does not exist?"
                    , exclude =
                        case (List.member name model.exclude) of
                            True ->
                                model.exclude

                            False ->
                                name :: model.exclude
                  }
                , Cmd.none
                )

        FetchGitter ->
            case model.roomChange of
                True ->
                    ( { model | tList = [], roomChange = False }, refreshGitterIDs gUrl )

                False ->
                    ( model, refreshGitterIDs gUrl )



-- HTTP


{-| gUserUrl returns a valid URL to access the Gitter API.
Used primarily in gitterIDRequest to fire off simultaneous
requests to get all userids in the gitter room
The "index" value is generated using the skipList function

    -- handcoded one from earlier on
    -- gUserUrl roomId key index = "https://api.gitter.im/v1/rooms/570a5925187bb6f0eadebf05/users?access_token=ae28f23f134c4364ad45e7b7355cfa91c92038bb&skip=0"

    gUserUrl "570a5" "ae28" 30
    --> "https://api.gitter.im/v1/rooms/570a5/users?access_token=ae28&skip=30"

-}
gUserUrl : String -> String -> Int -> String
gUserUrl roomID key index =
    "https://api.gitter.im/v1/rooms/"
        ++ roomID
        ++ "/users?access_token="
        ++ key
        ++ "&skip="
        ++ toString index


{-| skipList returns a list of numbers in intervals of 30.

    -- this is required for parallel dispatch of ~30 requests

    skipList 120
    --> [0, 30, 60, 90, 120]

    skipList 170
    --> [0, 30, 60, 90, 120, 150, 180]

-}
skipList : Int -> List Int
skipList userCount =
    List.map (\x -> x * 30) (List.range 0 (round ((toFloat userCount) / 30)))


{-| gitterIDBatchRequest helps create a list of Http.gets
to get all the Ids in a specific gitter room

    -- uses skipList and gUserUrl to generate a list
    -- of Http requests

-}
gitterIDBatchRequest : GRoom -> List (Cmd Msg)
gitterIDBatchRequest groom =
    let
        gitterIDRequest url =
            Task.attempt GitterIDStatus
                (Http.toTask (Http.get url decodeIDData))
    in
        skipList groom.userCount
            |> List.map (gUserUrl groom.id gitterKey)
            |> List.map gitterIDRequest


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
    Task.attempt (UpdateSucceed name) (Http.toTask (Http.get (url ++ name) decodeData))



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
    if
        member.uname
            == camper.uname
            && member.points
            /= camper.last.points
    then
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
        tList_ =
            model.tList
                |> List.filter (\x -> not <| List.member x.uname model.exclude)

        camper =
            tList_
                |> List.filterMap (getCamper member)
                |> List.head
    in
        case (camper) of
            Nothing ->
                tList_

            Just camper ->
                let
                    data =
                        pointsData member.points model.ts camper.last.points

                    camper_ =
                        { camper
                            | chist = data :: camper.chist
                            , last = data
                        }

                    model_ =
                        { model
                            | tList =
                                List.filter (\x -> x.uname /= member.uname) tList_
                        }
                in
                    camper_ :: model_.tList


addToList : Member -> Model -> Model
addToList member model =
    let
        clist =
            List.map .uname model.tList

        isPresent =
            List.member member.uname clist

        camper =
            createCamper model.ts member

        model_ =
            { model
                | points = member.points
                , tPoints = calculateTotal model.tList
                , message = ""
                , error = False
            }
    in
        case isPresent of
            True ->
                model

            False ->
                { model_ | tList = camper :: model.tList }


makeRequest : String -> Cmd Msg
makeRequest url =
    -- Task.perform FetchFail FetchSucceed (Http.get decodeData url))
    Task.attempt FetchOne (Http.toTask (Http.get url decodeData))


getData : String -> String -> Cmd Msg
getData api uname =
    let
        url =
            api ++ uname
    in
        makeRequest url


refreshGitterIDs : String -> Cmd Msg
refreshGitterIDs url =
    Task.attempt GitterStatus (Http.toTask (Http.get url decodeGData))


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
    Json.at [ "title" ] Json.string


decodePoints : Json.Decoder Int
decodePoints =
    Json.at [ "about", "browniePoints" ] Json.int


decodeData : Json.Decoder Member
decodeData =
    Json.at [ "about" ]
        (Json.map2 Member
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
    [ --Http.header "Access-Control-Allow-Origin" "*"
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


sortBasedOnHistory : Time -> Time -> List Camper -> List Camper
sortBasedOnHistory now cutOff campers =
    campers
        --|> List.map (truncateHistory now cutOff)
        --|> Debug.log "after trunc"
        -- summation of deltas in history
        |> List.sortWith flippedComparison
        -- latest points
        --|> List.sortWith flippedComparison3
        -- latest timestamp
        --|> List.sortWith flippedComparison2


sortBasedOnHistory2 : Time -> Time -> List Camper -> List Camper
sortBasedOnHistory2 now cutOff campers =
    -- campers_ = List.sortWith flippedComparison2 campers
    campers
        |> List.map (truncateHistory now cutOff)
        --|> Debug.log "post cut"
        --|> List.sortWith flippedComparison3
        --|> Debug.log "post compare3"
        |> List.sortWith flippedComparison2
        --|> Debug.log "post compare2"
        |> List.sortWith flippedComparison



--|> Debug.log "post compare"


flippedComparison3 : Camper -> Camper -> Order
flippedComparison3 a b =
    case compare a.last.points b.last.points of
        GT ->
            LT

        EQ ->
            EQ

        LT ->
            GT


flippedComparison2 : Camper -> Camper -> Order
flippedComparison2 a b =
    case compare a.last.ts b.last.ts of
        GT ->
            LT

        EQ ->
            EQ

        LT ->
            GT


truncateHistory : Time -> Time -> Camper -> Camper
truncateHistory now cutOff camper =
    { camper
        | chist =
            List.filterMap (isWithinCutOff now cutOff) camper.chist
    }


isWithinCutOff : Time -> Time -> Cdata -> Maybe Cdata
isWithinCutOff now cutOff data =
    case (data.ts >= (now - cutOff)) of
        True ->
            Just data

        False ->
            Nothing


{-| sum all the deltas from the history (or truncated history)
and use it for ordering in the sorting operations

    flippedComparison { uname = "kgashok"
                      , chist = [{ points = 229, ts = 17000, delta = 3 }]
                      , last = { points = 229, ts = 17000, delta = 3 }
                      }
                      { uname = "sudhar"
                      , chist = [ { points = 124, ts = 16000, delta = 4 }
                                , { points = 120, ts = 15000, delta = 10 }
                                ]
                      , last = { points = 124, ts = 16000, delta = 4 } }
    --> GT

-}
flippedComparison : Camper -> Camper -> Order
flippedComparison a b =
    let
        adelta =
            List.sum <| List.map .delta a.chist

        bdelta =
            List.sum <| List.map .delta b.chist
    in
        case
            compare ( bdelta, b.last.ts, b.last.points )
                ( adelta, a.last.ts, a.last.points )
        of
            GT ->
                GT

            EQ ->
                EQ

            _ ->
                LT



{-
   ahist =
       List.map .points a.chist

   bhist =
       List.map .points b.chist

   deltaA =
       Maybe.withDefault 0 (List.maximum ahist)
           - Maybe.withDefault 0 (List.minimum ahist)

   deltaB =
       Maybe.withDefault 0 (List.maximum bhist)
           - Maybe.withDefault 0 (List.minimum bhist)

   _ = Debug.log "a " a
   _ = Debug.log "b " b
   _ = Debug.log "deltaA " deltaA
   _ = Debug.log "deltaB " deltaB
-}
-- {"about":{"username":"kgashok","browniePoints":318,"bio":"Emperor, coffee enthusiast. "}}
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
