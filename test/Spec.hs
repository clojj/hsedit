{-# LANGUAGE OverloadedStrings #-}

import           EditRope
import qualified Graphics.Vty       as V
import           Test.Hspec

import TestHelper

main :: IO ()
main = hspec $

    -- renderTokensForLine :: [GHC.Located GHC.Token] -> AttrMap -> T.Text -> V.Image
    describe "renders a tokenized line of Text" $ do
    
      it "renders original line, if there are no tokens" $ 
        renderTokensForLine attrMap [] lineABC `shouldBe`
          V.text' V.defAttr lineABC

      it "renders line with 1 token; leading and trailing whitespace" $ 
        renderTokensForLine attrMap tokensLineABC lineABC `shouldBe`
          V.horizCat [
              V.text' V.defAttr " "
            , V.text' (getAttr attrMap "ITvarid") "abc"
            , V.text' V.defAttr " "
            ]

      it "renders line with 2 tokens; leading and trailing whitespace" $ 
        renderTokensForLine attrMap tokensLineABC_123 lineABC_123 `shouldBe`
          V.horizCat [
              V.text' V.defAttr " "
            , V.text' (getAttr attrMap "ITvarid") "abc"
            , V.text' V.defAttr " "
            , V.text' (getAttr attrMap "ITinteger") "123"
            , V.text' V.defAttr " "
            ]

-- tokenize reverted

      it "REVERTED: renders line with 2 tokens; leading and trailing whitespace" $ 
        renderTokensForLineReverse attrMap tokensLineABC_123_rev lineABC_123 `shouldBe`
          V.horizCat [
              V.text' V.defAttr " "
            , V.text' (getAttr attrMap "ITvarid") "abc"
            , V.text' V.defAttr " "
            , V.text' (getAttr attrMap "ITinteger") "123"
            , V.text' V.defAttr " "
            ]

-- TODOs
      it "TODO: multiline comments" $ 
        False

      it "TODO: evaluate API Annotations" $ 
        False
