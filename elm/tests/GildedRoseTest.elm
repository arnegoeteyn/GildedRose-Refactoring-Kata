module GildedRoseTest exposing (suite, testConjuredItem, testEmptyList, testLegendaryItem, testRandomConjuredLists, testRandomList)

import Fuzz exposing (Fuzzer, int, list)
import GildedRose exposing (..)
import Test exposing (..)
import Array
import Expect 


tupleFuzzer : Fuzzer Int -> Fuzzer Int -> Fuzzer ( Int, Int )
tupleFuzzer sell_by_fuzzer quality_fuzzer =
    Fuzz.tuple ( sell_by_fuzzer, quality_fuzzer )


oldSupportedNames : List String
oldSupportedNames =
    [ "Sulfuras, Hand of Ragnaros"
    , "Apple Pie"
    , "Elixer of the Mongoose"
    , "Aged Brie"
    , "French fries with extra mayo"
    , "Backstage passes to a TAFKAL80ETC concert"
    ]


newSupportedNames : List String
newSupportedNames =
    oldSupportedNames
        ++ [ "Conjured headphones"
           , "Conjured trashcan"
           ]


generateRandomItems : List ( Int, Int ) -> List String -> List GildedRose.Item
generateRandomItems valuesList names =
    let
        arrayNames =
            names |> Array.fromList

        nameIndex =
            \i -> modBy (Array.length arrayNames) i |> (\index -> Array.get index arrayNames |> Maybe.withDefault "An item representing an error")
    in
    List.map
        (\( sellBy, quality ) ->
            let
                name =
                    nameIndex (sellBy + quality)
            in
            case typeFromName name of
                Legendary ->
                    legendaryItem sellBy

                _ ->
                    { sell_by = sellBy, quality = quality, name = name }
        )
        valuesList


applyNTimes : Int -> (a -> a) -> a -> a
applyNTimes n f value =
    case n of
        0 ->
            value

        _ ->
            applyNTimes (n - 1) f (f value)


legendaryItem : Int -> Item
legendaryItem sellBy =
    Item "Sulfuras, Hand of Ragnaros" sellBy legendaryQuality


isValidQuality : Item -> Bool
isValidQuality item =
    let
        maxQuality =
            if typeFromName item.name == Legendary then
                legendaryQuality

            else
                regularMaxQuality
    in
    item.quality
        >= 0
        && item.quality
        <= maxQuality + 2 -- The old function has a bug in it such that a ticket can be worth up to 52 quality
        && (if typeFromName item.name == Legendary then
                item.quality == maxQuality  

            else
                True
           )


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


testLegendaryItem : Test
testLegendaryItem =
    fuzz2 int (Fuzz.intRange 0 5) "legendary item should always keep the same quality and sellBy" <|
        \sellBy days ->
            Expect.equal [ legendaryItem sellBy ] (applyNTimes days GildedRose.update_quality [ legendaryItem sellBy ])


testConjuredItem : Test
testConjuredItem =
    fuzz2 (Fuzz.intRange 0 20) (Fuzz.intRange 0 regularMaxQuality) "A conjured item should degrade twice as fast as an old one" <|
        \sellBy quality ->
            let
                originalConjuredItem =
                    Item "Conjured Pancakes" sellBy quality

                conjuredItem =
                    GildedRose.updateItem originalConjuredItem

                originalRegularItem =
                    Item "Pancakes" sellBy quality

                regularItem =
                    GildedRose.updateItem originalRegularItem
            in
            Expect.all
                [ \item -> item.sell_by |> Expect.equal regularItem.sell_by
                , \item ->
                    item.quality
                        |> Expect.equal
                            (if regularItem.quality == 0 then
                                0

                             else
                                originalConjuredItem.quality - 2 * (originalRegularItem.quality - regularItem.quality)
                            )
                ]
                conjuredItem


testRandomList : Test
testRandomList =
    fuzz (list <| tupleFuzzer (Fuzz.intRange 0 100) (Fuzz.intRange 0 regularMaxQuality)) "A random list should return the same result on the old function and the new function" <|
        \t ->
            let
                randomItems =
                    generateRandomItems t oldSupportedNames
            in
            Expect.equal (GildedRose.update_quality randomItems) (GildedRose.update_quality_old randomItems)


testRandomConjuredLists : Test
testRandomConjuredLists =
    fuzz (list <| tupleFuzzer (Fuzz.intRange 0 100) (Fuzz.intRange 0 regularMaxQuality)) "No item should be wrongly updated" <|
        \t ->
            let
                randomItems =
                    generateRandomItems t newSupportedNames
            in
            Expect.all
                [ List.all isValidQuality >> Expect.true "expect every item to have a valid quantity"
                , \items ->
                    items
                        |> List.filter (\item -> typeFromName item.name == Ticket && item.sell_by < -1)
                        |> List.all (\item -> item.quality == 0)
                        |> Expect.true "Every ticket past its due date should have quality 0"
                ]
            <|
                GildedRose.update_quality randomItems
