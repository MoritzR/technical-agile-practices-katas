module GildedRose where

type GildedRose = [Item]

data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

updateItemQuality :: Item -> Int

updateItemQuality (Item "Aged Brie" sellIn quality)
  | sellIn <= 0 = min (quality + 2) 50
  | otherwise   = min (quality + 1) 50

updateItemQuality (Item "Sulfuras, Hand of Ragnaros" sellIn quality) = quality

updateItemQuality (Item "Backstage passes to a TAFKAL80ETC concert" sellIn quality)
  = min (updateBackStagePassQuality sellIn quality) 50
 where
  updateBackStagePassQuality sellIn quality
    | sellIn <= 0                   = 0
    | sellIn <= 5 && quality <= 47  = quality + 3
    | sellIn <= 10 && quality <= 48 = quality + 2
    | otherwise                     = quality + 1

updateItemQuality (Item name sellIn quality)
  | sellIn <= 0 = max (quality - 2) 0
  | otherwise   = max (quality - 1) 0


updateQuality :: GildedRose -> GildedRose
updateQuality = map updateItem

sellInAfterOneDay sellIn = sellIn - 1

updateItem item@(Item name@"Sulfuras, Hand of Ragnaros" sellIn quality) =
  let quality' = updateItemQuality item
      sellIn'  = sellIn
  in  Item name sellIn' quality'

updateItem item@(Item name sellIn quality) =
  let quality' = updateItemQuality item
      sellIn'  = sellInAfterOneDay sellIn
  in  Item name sellIn' quality'
