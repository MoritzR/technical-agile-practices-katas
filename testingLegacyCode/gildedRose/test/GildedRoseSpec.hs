module GildedRoseSpec (spec) where

import Test.Hspec
import GildedRose

spec :: Spec
spec =
  describe "updateQuality" $ do
    it "doesn't decrease quality below 0" $
       let inventory = [Item "foo" 0 0]
           actual = updateQuality inventory
           expected = [Item "foo" (-1) 0]
       in actual `shouldBe` expected

    it "decreases sellIn and quality when day passes" $
      let inventory = [Item "foo" 123 10]
          actual = updateQuality inventory
          expected = [Item "foo" 122 9]
      in actual `shouldBe` expected

    it "decreases quality double as fast when sell by date passed" $
      let inventory = [Item "foo" 0 10, Item "foo" (-20) 10]
          actual = updateQuality inventory
          expected = [Item "foo" (-1) 8, Item "foo" (-21) 8]
      in actual `shouldBe` expected

    it "Aged Brie increases in quality before sellIn day" $
      let inventory = [Item "Aged Brie" 10 10]
          actual = updateQuality inventory
          expected = [Item "Aged Brie" 9 11]
      in actual `shouldBe` expected

    it "Aged Brie increases in quality twice as fast after sellIn day" $
      let inventory = [Item "Aged Brie" (-1) 10]
          actual = updateQuality inventory
          expected = [Item "Aged Brie" (-2) 12]
      in actual `shouldBe` expected
  
    it "Aged Brie doesn't increase in quality past 50" $
      let inventory = [Item "Aged Brie" (-1) 50]
          actual = updateQuality inventory
          expected = [Item "Aged Brie" (-2) 50]
      in actual `shouldBe` expected

    it "Back stage passes to T. concert increases in quality by two when 10 days before sellIn day" $
      let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 10 10]
          actual = updateQuality inventory
          expected = [Item "Backstage passes to a TAFKAL80ETC concert" 9 12]
      in actual `shouldBe` expected

    it "Back stage passes to T. concert never increases quality over 50" $
      let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 15 50]
          actual = updateQuality inventory
          expected = [Item "Backstage passes to a TAFKAL80ETC concert" 14 50]
      in actual `shouldBe` expected

    it "Back stage passes to T. concert never increases quality over 50 even when 10 days before" $
      let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 10 50]
          actual = updateQuality inventory
          expected = [Item "Backstage passes to a TAFKAL80ETC concert" 9 50]
      in actual `shouldBe` expected

    it "Back stage passes to T. concert never increases quality over 50 even when 5 days before" $
      let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 5 50]
          actual = updateQuality inventory
          expected = [Item "Backstage passes to a TAFKAL80ETC concert" 4 50]
      in actual `shouldBe` expected

    it "Back stage passes to T. concert increases in quality by three when 5 days before sellIn day" $
      let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 5 10]
          actual = updateQuality inventory
          expected = [Item "Backstage passes to a TAFKAL80ETC concert" 4 13]
      in actual `shouldBe` expected

    it "Back stage passes to T. concert drops quality to 0 when after sellIn day" $
      let inventory = [Item "Backstage passes to a TAFKAL80ETC concert" 0 10]
          actual = updateQuality inventory
          expected = [Item "Backstage passes to a TAFKAL80ETC concert" (-1) 0]
      in actual `shouldBe` expected


    it "doesn't decrease sellIn or quality for legendary Item Sulfuras" $
      let inventory = [Item "Sulfuras, Hand of Ragnaros" 1 80]
          actual = updateQuality inventory
          expected = [Item "Sulfuras, Hand of Ragnaros" 1 80]
      in actual `shouldBe` expected
