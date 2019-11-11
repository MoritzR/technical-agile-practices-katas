module GildedRose where

type GildedRose = [Item]

data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

updateItemQuality :: Item -> Int

updateItemQuality (Item "Aged Brie" sellIn quality) = min (quality + 1) 50

updateItemQuality (Item "Sulfuras, Hand of Ragnaros" sellIn quality) = quality

updateItemQuality (Item "Backstage passes to a TAFKAL80ETC concert" sellIn quality)
  = min (increaseBackStagePassQuality sellIn quality) 50

updateItemQuality (Item name sellIn quality) = max (quality - 1) 0


increaseBackStagePassQuality sellIn quality = quality + qualityIncrease
 where
  qualityIncrease | sellIn < 6 && quality <= 47  = 3
                  | sellIn < 11 && quality <= 48 = 2
                  | otherwise                    = 1

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityItem


updateQualityItem (Item "Aged Brie" sellIn quality) =
  let name     = "Aged Brie"
      quality' = updateItemQuality (Item name sellIn quality)
      sellIn'  = sellIn - 1
  in  if quality' < 50 && sellIn' < 0
        then (Item name sellIn' (quality' + 1))
        else (Item name sellIn' quality')

updateQualityItem (Item "Backstage passes to a TAFKAL80ETC concert" sellIn quality)
  = let name     = "Backstage passes to a TAFKAL80ETC concert"
        quality' = updateItemQuality (Item name sellIn quality)
        sellIn'  = sellIn - 1
    in  if sellIn' < 0
          then (Item name sellIn' 0)
          else (Item name sellIn' quality')

updateQualityItem (Item "Sulfuras, Hand of Ragnaros" sellIn quality) =
  let name     = "Sulfuras, Hand of Ragnaros"
      quality' = updateItemQuality (Item name sellIn quality)
      sellIn'  = sellIn
  in  (Item name sellIn' quality')


updateQualityItem (Item name sellIn quality) =
  let quality' = updateItemQuality (Item name sellIn quality)
      sellIn'  = sellIn - 1
  in  if sellIn' < 0 && quality' > 0
        then (Item name sellIn' (quality' - 1))
        else (Item name sellIn' quality')
