module GildedRoseTest exposing (suite, testEmptyList, testLegendaryItem)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GildedRose exposing (..)
import Test exposing (..)

applyNTimes: Int -> (a -> a) -> a -> a
applyNTimes n f value = 
    case n of
        0 -> value
        _ -> applyNTimes (n - 1) f (f value)

legendaryItem: Int -> Item
legendaryItem sellBy = Item "Sulfuras, Hand of Ragnaros" sellBy 80 Legendary

suite : Test
suite =
    test "example test"
        (\_ ->
            let
                foo =
                    Item "foo" 10 30 Regular
            in
            Expect.equal foo.name "foo"
        )

testEmptyList : Test
testEmptyList =
    test "test empty list" <|
        \_ -> GildedRose.update_quality [] |> Expect.equal []

testLegendaryItem: Test
testLegendaryItem =
    
    fuzz2 int (Fuzz.intRange 0 5) "legendary item should always keep the same quality and sellBy" <|
        \sellBy days -> 
            Expect.equal [legendaryItem sellBy] (applyNTimes days GildedRose.update_quality [legendaryItem sellBy]) 
    