module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


--suite : List Test
--suite =
all : Test 
all =
    --todo "Implement our first test. See http://package.elm-lang.org/packages/elm-community/elm-test/latest for how to do this!"
    describe "Fcc Test Suite" 
        [describe "Unit test examples"
            [ test "zero" <| \() -> Expect.equal 0 0
            , test "pass" <| \() -> Expect.true "the truth" True
            --, test "fail" <| assertNotEqual True False
            --, test "pass" <| assertEqual [0, 30, 60, 90, 120, 150, 180] (skipList 170)
            --, test "pass" <| assertEqual historyA first.chist
            ]
        ]