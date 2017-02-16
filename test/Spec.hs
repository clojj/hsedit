{-# LANGUAGE OverloadedStrings #-}

import qualified Brick.AttrMap      as A
import           EditRope
import qualified Graphics.Vty       as V
import           Test.Hspec
import           Brick.Util             (on)

attrMap :: A.AttrMap
attrMap = A.attrMap V.defAttr
    [ (editAttr, V.white `on` V.black)
     ,(A.attrName "ITinteger", V.withStyle (V.green `on` V.black) V.bold)
    ]

main :: IO ()
main = do

  let line = ""
      tokens = []
      
  hspec $

    describe "renders a tokenized line of Text" $ do

      it "renders empty line" $
        -- renderTokensForLine :: [GHC.Located GHC.Token] -> AttrMap -> T.Text -> V.Image
        renderTokensForLine tokens attrMap line `shouldBe` V.text' V.defAttr " "

      -- it "renders original line, if there are no tokens" $
      --   renderTokensForLine [] attrMap line `shouldBe` expectedImage

