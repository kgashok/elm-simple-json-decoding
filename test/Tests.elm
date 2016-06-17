module Main exposing (..) -- where

--import List

import ElmTest exposing (..)

import Model exposing (..)

testm : Model 
testm =  {initialModel | gRoom = 
    {id = "570a5925187bb6f0eadebf05", name="", userCount=0} }


tests : List Test
tests =
    [ 0 `equals` 0
    , test "pass" <| assert True
    , test "fail" <| assertNotEqual True False
    , test "pass" <| assertEqual [0, 30, 60, 90, 120, 150, 180] (skipList 170)
    -- , test "pass" <| assertEqual 275 (gitterIDRequest)
    ]
    -- ++
    -- (List.map defaultTest <| assertionList [1..10] [1..10])


consoleTests : Test
consoleTests =
    suite "All Tests" tests

main : Program Never
main =
    runSuiteHtml consoleTests