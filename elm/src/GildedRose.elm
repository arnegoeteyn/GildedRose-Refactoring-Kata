module GildedRose exposing (Item, ItemType(..), typeFromName, updateItem, update_quality, update_quality_old)


type alias Item =
    { name : String
    , sell_by : Int
    , quality : Int
    }


type ItemType
    = Legendary
    | Brie
    | Ticket
    | Regular
    | Conjured


typeFromName : String -> ItemType
typeFromName name =
    case name of
        "Sulfuras, Hand of Ragnaros" ->
            Legendary

        "Backstage passes to a TAFKAL80ETC concert" ->
            Ticket

        "Aged Brie" ->
            Brie

        _ ->
            if String.startsWith "Conjured" name then
                Conjured

            else
                Regular


updateSellBy : ( Item, Bool ) -> Item
updateSellBy ( item, shouldLower ) =
    { item
        | sell_by =
            if shouldLower then
                item.sell_by - 1

            else
                item.sell_by
    }


regularUpdate : Item -> Item
regularUpdate item =
    if item.sell_by < 0 && item.quality >= 2 then
        { item | quality = item.quality - 2 }

    else if item.quality >= 1 then
        { item | quality = item.quality - 1 }

    else
        { item | quality = 0 }


brieUpdate : Item -> Item
brieUpdate item =
    { item | quality = item.quality + 1 }


ticketUpdate : Item -> Item
ticketUpdate item =
    if item.sell_by < 0 then
        { item | quality = 0 }

    else if item.sell_by < 6 then
        { item | quality = item.quality + 3 }

    else if item.sell_by < 11 then
        { item | quality = item.quality + 2 }

    else
        { item | quality = item.quality + 1 }


qualityUpdate : Item -> ( Item, Bool )
qualityUpdate item =
    let
        type_ =
            typeFromName item.name
    in
    case type_ of
        Legendary ->
            ( item, False )

        Brie ->
            if item.quality < 50 then
                ( brieUpdate item, True )

            else
                ( item, False )

        Ticket ->
            if item.quality < 50 then
                ( ticketUpdate item, True )

            else
                ( item, False )

        Regular ->
            ( regularUpdate item, True )

        Conjured ->
            ( (regularUpdate << regularUpdate) item, True )


updateItem : Item -> Item
updateItem =
    qualityUpdate >> updateSellBy


update_quality : List Item -> List Item
update_quality =
    List.map updateItem


update_quality_old : List Item -> List Item
update_quality_old items =
    List.map
        (\item ->
            if item.name == "Aged Brie" || item.name == "Backstage passes to a TAFKAL80ETC concert" then
                if item.quality < 50 then
                    if item.name == "Backstage passes to a TAFKAL80ETC concert" then
                        if item.sell_by < 0 then
                            { item | sell_by = item.sell_by - 1, quality = 0 }

                        else if item.sell_by < 6 then
                            { item | sell_by = item.sell_by - 1, quality = item.quality + 3 }

                        else if item.sell_by < 11 then
                            { item | sell_by = item.sell_by - 1, quality = item.quality + 2 }

                        else
                            { item | sell_by = item.sell_by - 1, quality = item.quality + 1 }

                    else
                        { item | sell_by = item.sell_by - 1, quality = item.quality + 1 }

                else
                    { item | sell_by = item.sell_by }

            else if item.name /= "Aged Brie" && item.name /= "Sulfuras, Hand of Ragnaros" then
                if item.sell_by < 0 && item.quality > 0 then
                    if item.quality >= 2 then
                        { item | sell_by = item.sell_by - 1, quality = item.quality - 2 }

                    else
                        { item | sell_by = item.sell_by - 1, quality = 0 }

                else if item.quality >= 1 then
                    { item | sell_by = item.sell_by - 1, quality = item.quality - 1 }

                else
                    { item | sell_by = item.sell_by - 1, quality = 0 }

            else
                item
        )
        items
