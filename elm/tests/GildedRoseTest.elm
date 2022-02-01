module GildedRoseTest exposing (suite, testEmptyList, testLegendaryItem, testRandomList)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import GildedRose exposing (..)
import Test exposing (..)
import Array exposing (Array)
    
tupleFuzzer : Fuzzer Int -> Fuzzer Int -> Fuzzer (Int, Int)
tupleFuzzer sell_by_fuzzer quality_fuzzer =
        Fuzz.tuple (sell_by_fuzzer, quality_fuzzer)    


generateRandomItems : List (Int, Int) ->  List GildedRose.Item
generateRandomItems valuesList =
    let
        names =
            [ "Sulfuras, Hand of Ragnaros"
            , "Apple Pie"
            , "Elixer of the Mongoose"
            , "Aged Brie"
            , "French fries with extra mayo"
            , "Backstage passes to a TAFKAL80ETC concert"
            ] |> Array.fromList
        nameIndex  = \i -> modBy (Array.length names) i |> \index -> Array.get index names |> Maybe.withDefault "An item representing an error"
    in
    List.map 
        (\(sellBy, quality) ->
            let
                 name = nameIndex (sellBy + quality)
            in
            case typeFromName name of
                Legendary ->    legendaryItem sellBy
                _ ->
                    {sell_by = sellBy, quality = quality, name = name }) 
        valuesList


applyNTimes: Int -> (a -> a) -> a -> a
applyNTimes n f value = 
    case n of
        0 -> value
        _ -> applyNTimes (n - 1) f (f value)

legendaryItem: Int -> Item
legendaryItem sellBy = Item "Sulfuras, Hand of Ragnaros" sellBy 80 

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

testLegendaryItem: Test
testLegendaryItem =
    fuzz2 int (Fuzz.intRange 0 5) "legendary item should always keep the same quality and sellBy" <|
        \sellBy days -> 
            Expect.equal [legendaryItem sellBy] (applyNTimes days GildedRose.update_quality [legendaryItem sellBy]) 
    
testRandomList: Test
testRandomList =
    fuzz (list <| tupleFuzzer int (Fuzz.intRange 0 50)) "A random list should return the same result on the old function and the new function" <|
        \t -> 
            let
                randomItems = generateRandomItems t 
            in
            Expect.equal (GildedRose.update_quality randomItems) (GildedRose.update_quality_old randomItems)
 