module GildedRose exposing (Item, ItemType(..), update_quality)


type alias Item =
    { name : String
    , sell_by : Int
    , quality : Int
    , type_ : ItemType
    }


type ItemType
    = Legendary
    | Brie
    | Ticket
    | Regular


updateItem : Item -> Item
updateItem item =
    case item.type_ of
        Legendary ->
            item

        Brie ->
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

        Ticket ->
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

        Regular ->
            if item.sell_by < 0 && item.quality > 0 then
                if item.quality >= 2 then
                    { item | sell_by = item.sell_by - 1, quality = item.quality - 2 }

                else
                    { item | sell_by = item.sell_by - 1, quality = 0 }

            else if item.quality >= 1 then
                { item | sell_by = item.sell_by - 1, quality = item.quality - 1 }

            else
                { item | sell_by = item.sell_by - 1, quality = 0 }


update_quality : List Item -> List Item
update_quality =
    List.map updateItem