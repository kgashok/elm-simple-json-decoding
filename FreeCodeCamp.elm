import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task

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

main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { url : String
  , uname : String 
  , result : String 
  , error : Bool
  , points : Int 
  }


initialModel : Model
initialModel = {
  url = fccAPI
  , uname = ""
  , result = ""
  , error = False
  , points = -1
  }


init : (Model, Cmd Msg)
init =
  (initialModel, Cmd.none)


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
      (model, makeRequest model.url)

    FetchSucceed val ->
      ({ model | points = val, error = False }, Cmd.none)

    StoreURL uname ->
      ({ model | uname = uname, url = fccAPI ++ uname }, Cmd.none)

    FetchFail _ ->
      ({ model | error = True }, Cmd.none)

    --FetchPoints val -> 
    --  ({ model | points = val, error = False }, Cmd.none )

-- VIEW

url1 : String
url1 = "https://api.myjson.com/bins/3fueo"

url2 : String 
url2 = "https://api.myjson.com/bins/2kjv4"

fccAPI : String 
fccAPI = "https://www.freecodecamp.com/api/users/about?username="


view : Model -> Html Msg
view model =
  let
    response =
      if model.error == True
      then "There was an error"
      else if model.result /= ""
      then "I just found: " ++ model.result
      else if model.points /= -1
      then "I just found: " ++ (toString model.points)
      else "" 
  in
    div []
      [ h3 [] [ text "Simple Object"]
      , p [] [ text "Here I want to grab the ''browniePoints''"]
      , p [] [ text ("FCC URL: " ++ fccAPI) ]
      , input [
          placeholder "Enter a FCC username",
          onInput StoreURL
        ] []
        , button [ onClick FetchData ] [ text "Fetch!" ]
        , h1 [rStyle]  [ text response ]
        , div [] [ text (toString model) ]
      ]

rStyle : Attribute msg 
rStyle = 
  style 
    [ ("backgroundColor", "#ff6600"),
      ("color", "white"),
      ("fontSize", "300%")
    ]


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


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
