module Model exposing (..) -- where 

import Time exposing (Time)
import String


url1 : String
url1 = "https://api.myjson.com/bins/3fueo"

url2 : String 
url2 = "https://api.myjson.com/bins/2kjv4"

fccAPI : String 
fccAPI = "https://www.freecodecamp.com/api/users/about?username="

gitterKey : String
gitterKey = "ae28f23f134c4364ad45e7b7355cfa91c92038bb"

gUrl : String
gUrl = "https:/api.gitter.im/v1/rooms?access_token=" ++ gitterKey
--gUrl = "https://api.myjson.com/bins/nel8"


gUserUrl : String -> String -> Int -> String
gUserUrl roomID key index = 
  "https://api.gitter.im/v1/rooms/" ++ 
    roomID ++
    "/users?access_token=" ++ 
    key ++ 
    "&skip=" ++ toString index 

--gUserUrl roomId key index =
  -- "https://api.gitter.im/v1/rooms/570a5925187bb6f0eadebf05/users?access_token=ae28f23f134c4364ad45e7b7355cfa91c92038bb&skip=0"
-- gUserUrl roomdID key index = "https://api.myjson.com/bins/42vt0"


-- MODEL

type alias GRoom = 
  { id : String
  , name: String 
  , userCount : Int
  }

type alias Model =
  { url : String
  , name : String 
  , uname : String
  , message : String 
  , error : Bool
  , points : Int 
  , ts  : Time
  , tList : List Camper
  , tPoints : Int 
  , gList : List Camper  -- from Gitter room /kgisl/campsite
  , gRoom : GRoom
  }

type alias Camper = 
  { uname: String
  , chist: List Cdata
  , last: Cdata
  }

type alias Member = 
  { uname: String
  , points: Int  
  }

type alias Cdata = 
  { points: Int 
  , ts : Time
  , delta : Int
  }

type alias Gid = 
  { username : String
  , displayName : String  
  , avatarUrlSmall : String
  }

createCamper : Member -> Time -> Camper 
createCamper member ts = 
  let 
    data = pointsData member.points ts member.points
  in
    { uname = String.toLower member.uname
    , chist = [data]
    , last  = data
    }

createCamperFromGid : Gid -> Camper 
createCamperFromGid gid = 
  { uname = String.toLower gid.username
  , chist = []
  , last  = {points = 0, ts = 0, delta = 0}
  }

pointsData : Int -> Time -> Int -> Cdata 
pointsData p time prev = 
  {points = p, ts = time, delta = p - prev }



initialModel : Model
initialModel = 
  { url = fccAPI
  , name = ""
  , uname = ""
  , message = ""
  , error = False
  , points = -1
  , ts = 0
  , tList = []
  , tPoints = 0
  , gList = []
  , gRoom = {id="", name="kgisl/campsite", userCount=0}
  }
