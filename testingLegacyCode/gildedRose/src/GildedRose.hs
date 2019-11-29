module GildedRose where

type GildedRose = [Item]

data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality
  
updateQuality :: GildedRose -> GildedRose
updateQuality = map updateItem

updateItem :: Item -> Item
updateItem item@(Item name sellIn quality) =
  let quality' = updateItemQuality item
      sellIn'  = updateItemSellIn item
  in  Item name sellIn' quality'


updateItemSellIn :: Item -> Int
updateItemSellIn (Item name sellIn _)
  | name == "Sulfuras, Hand of Ragnaros" = sellIn
  | otherwise                            = sellIn - 1


updateItemQuality :: Item -> Int

updateItemQuality (Item "Aged Brie" sellIn quality)
  | sellIn <= 0 = min (quality + 2) 50
  | otherwise   = min (quality + 1) 50

updateItemQuality (Item "Sulfuras, Hand of Ragnaros" sellIn quality) = quality

updateItemQuality (Item "Backstage passes to a TAFKAL80ETC concert" sellIn quality)
  = min (updateBackStagePassQuality sellIn quality) 50
 where
  updateBackStagePassQuality sellIn quality
    | afterConcert          = 0
    | fiveDaysBeforeConcert = quality + 3
    | tenDaysBeforeConcert  = quality + 2
    | otherwise             = quality + 1

  fiveDaysBeforeConcert = sellIn <= 5
  tenDaysBeforeConcert  = sellIn <= 10
  afterConcert          = sellIn <= 0

updateItemQuality (Item name sellIn quality)
  | sellIn <= 0 = max (quality - 2) 0
  | otherwise   = max (quality - 1) 0
