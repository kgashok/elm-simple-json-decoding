module Model exposing (..) -- where 

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

url1 : String
url1 = "https://api.myjson.com/bins/3fueo"

url2 : String 
url2 = "https://api.myjson.com/bins/2kjv4"

fccAPI : String 
fccAPI = "https://www.freecodecamp.com/api/users/about?username="


-- MODEL


type alias Model =
  { url : String
  , name : String 
  , uname : String 
  , error : Bool
  , points : Int 
  }


initialModel : Model
initialModel = 
  { url = fccAPI
  , name = ""
  , uname = ""
  , error = False
  , points = -1
  }
