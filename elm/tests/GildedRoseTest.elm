module GildedRoseTest exposing (suite, testEmptyList)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GildedRose exposing (..)
import Test exposing (..)

suite : Test
suite =
    test "example test"
        (\_ ->
            let
                foo =
                    Item "foo" 10 30
            in
            Expect.equal foo.name "foo"
        )

testEmptyList : Test
testEmptyList =
    test "test empty list" <|
        \_ -> GildedRose.update_quality [] |> Expect.equal []