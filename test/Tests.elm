module Main exposing (..) -- where

--import List

import ElmTest exposing (..)

import Model exposing (..)

testm : Model 
testm =  {initialModel | gRoom = 
    {id = "570a5925187bb6f0eadebf05", name="", userCount=0} }


createMember : String -> Int -> Member 
createMember name points = 
  { uname  = name
  , points = points
  }

memberList : List Member
memberList = 
    [ createMember "kgashok" 200
    , createMember "sudhar" 100
    , createMember "ramya" 150
    ]

{-pointsData : Int -> Time -> Int -> Cdata -}

historyS : List Cdata 
historyS = 
    [ pointsData 110 9000 0
    , pointsData 120 15000 9000
    , pointsData 122 16000 10000
    ]


historyR : List Cdata 
historyR = 
    [ pointsData 150 9000 0
    , pointsData 170 15500 9000
    , pointsData 222 17000 10000
    ]

historyA : List Cdata 
historyA = 
    [ pointsData 220 9000 0
    , pointsData 222 10000 9000
    , pointsData 226 11000 10000
    , pointsData 229 11000 10000
    ]

assignHistory : List Cdata -> Camper -> Camper 
assignHistory data camper = 
    {camper| chist = data} 


createCampersFromMembers : List Member -> List Camper 
createCampersFromMembers mList = 
  let 
    cList = List.map (createCamper 0) memberList
  in 
    List.map2 assignHistory [historyA, historyS, historyR] cList


dinfo : { c | last : { b | points : a }, uname : String } -> String
dinfo {uname, last} = 
    uname ++ " " ++ (toString last.points)


tests : List Test
tests =
  let 
    clist = createCampersFromMembers memberList
    dummy = createCamper 0 {uname = "NA", points = 0}
    first = List.head clist |> Maybe.withDefault dummy 
    sortOut = List.map dinfo (sortHistory clist)
  in 
    [ 0 `equals` 0
    , test "pass" <| assert True
    , test "fail" <| assertNotEqual True False
    , test "pass" <| assertEqual [0, 30, 60, 90, 120, 150, 180] (skipList 170)
    , test "pass" <| assertEqual historyA first.chist
    ]
    ++
    (List.map defaultTest <| assertionList ["ramya", "sudhar", "kgashok"] sortOut)
    -- ++
    -- (List.map defaultTest <| assertionList [1..10] [1..10])


consoleTests : Test
consoleTests =
    suite "All Tests" tests

main : Program Never
main =
    runSuite consoleTests