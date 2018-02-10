module Model exposing (..)

import Time exposing (Time, inHours)
import String


{-| Other variants that have not worked properly

    --fccAPI = "https://comfortable-fibre.glitch.me/"
    --fccAPI = "http://www.freecodecamp.com/about?username="
    --fccAPI = "https://cors-anywhere.herokuapp.com/https://www.freecodecamp.org/api/users/about?username="
    --fccAPI = "https://cors.now.sh/http://www.freecodecamp.org/api/users/about?username="

-}
fccAPI : String
fccAPI =
    "https://www.freecodecamp.org/api/users/about?username="


{-| mock test URL to get brownie points information for user
-- points value is a string equivalent of the integer value
-}
testUrl1 : String
testUrl1 =
    "https://api.myjson.com/bins/3fueo"


{-| mock test URL to get brownie points information for user
-- points value is an integer value
-}
testUrl2 : String
testUrl2 =
    "https://api.myjson.com/bins/2kjv4"


{-| mock test gitter URL to simulate rooms information
-}
testGitterUrl =
    "https://api.myjson.com/bins/nel8"


{-| mock test gitter URL to simulate userids in specific gitter room
-}
testGitterUserUrl roomdID key index =
    "https://api.myjson.com/bins/42vt0"


gitterKey : String
gitterKey =
    "ae28f23f134c4364ad45e7b7355cfa91c92038bb"


gUrl : String
gUrl =
    "https://api.gitter.im/v1/rooms?access_token=" ++ gitterKey



-- MODEL


type alias GRoom =
    { id : String
    , name : String
    , userCount : Int
    }


type alias Model =
    { url : String -- needs to moved out
    , name : String
    , uname : String
    , message : String
    , error : Bool
    , roomChange : Bool
    , points : Int
    , ts : Time
    , tList : List Camper
    , tPoints : Int
    , tPoints_prev : Int
    , gList : List Camper -- from Gitter room /kgisl/campsite
    , gRoom : GRoom
    , min5 : Bool
    , min15 : Bool
    , exclude : List String
    }


type Interval
    = SetMin5
    | SetMin15


type alias Camper =
    { uname : String
    , chist : List Cdata
    , last : Cdata
    }


type alias Member =
    { uname : String
    , points : Int
    }


type alias Cdata =
    { points : Int
    , ts : Time
    , delta : Int
    }


type alias Gid =
    { username : String
    , displayName : String
    , avatarUrlSmall : String
    }


createCamper : Time -> Member -> Camper
createCamper ts member =
    let
        data =
            pointsData member.points ts member.points
    in
        { uname = String.toLower member.uname
        , chist = [ data ]
        , last = data
        }


createCamperFromGid : List Camper -> Gid -> Maybe Camper
createCamperFromGid tList gid =
    let
        cList =
            List.map .uname tList

        isPresent =
            List.member (String.toLower gid.username) cList
    in
        case isPresent of
            False ->
                Just
                    { uname = String.toLower gid.username
                    , chist = []
                    , last = { points = 0, ts = 0, delta = 0 }
                    }

            True ->
                Nothing


pointsData : Int -> Time -> Int -> Cdata
pointsData p time prev =
    { points = p, ts = time, delta = p - prev }



{- Both the below needed to be included in the model -}
{- In Elm repl inHours 2592000000 = 720 hours  or 30 days -}


cutOff : Float
cutOff =
    inHours 2592000000



-- 720 hours
-- cutOff = inHours 5000000


excluded : List String
excluded =
    [ "quincylarson", "ddd" ]


initialModel : Model
initialModel =
    { url = fccAPI -- needs to be moved out
    , name = ""
    , uname = ""
    , message = ""
    , error = False
    , points = -1
    , ts = 0
    , tList = []
    , tPoints = 0
    , tPoints_prev = 0
    , gList = []
    , gRoom = { id = "", name = "kgisl/campsite", userCount = 0 }
    , roomChange = False
    , min5 = False
    , min15 = True
    , exclude = excluded
    }
