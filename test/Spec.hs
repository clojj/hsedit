{-# LANGUAGE OverloadedStrings #-}

import qualified Brick.AttrMap      as A
import           EditRope
import qualified Graphics.Vty       as V
import           Test.Hspec
import           Brick.Util             (on)

import qualified GHC
import qualified Lexer                  as GHC
import qualified FastString             as GHC


attrMap :: A.AttrMap
attrMap = A.attrMap V.defAttr
    [ (editAttr, V.white `on` V.black)
     ,(A.attrName "ITinteger", V.withStyle (V.green `on` V.black) V.bold)
    ]

main :: IO ()
main = do

  let lineABC = " abc"
      tokensEmpty = []
      tokens1 = [GHC.L (GHC.mkSrcSpan (GHC.mkSrcLoc (GHC.fsLit "") 1 2) (GHC.mkSrcLoc (GHC.fsLit "") 1 5)) $ GHC.ITvarid (GHC.fsLit "abc")]
      
  hspec $

    -- renderTokensForLine :: [GHC.Located GHC.Token] -> AttrMap -> T.Text -> V.Image
    describe "renders a tokenized line of Text" $ do
    
      it "renders original line, if there are no tokens" $ 
        renderTokensForLine tokensEmpty attrMap lineABC `shouldBe` V.text' V.defAttr lineABC

      it "renders beginning of line and 1 token" $ 
        renderTokensForLine tokens1 attrMap lineABC `shouldBe` V.text' V.defAttr lineABC

      it "TODO: multiline comments" $ 
        False
