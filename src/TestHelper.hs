{-# LANGUAGE OverloadedStrings #-}
module TestHelper 
  (lineABC, tokensLineABC, attrMap, mkLocatedToken)
where 
  
import qualified Brick.AttrMap      as A
import           EditRope
import qualified Graphics.Vty       as V
import           Brick.Util             (on)

import qualified Data.Text              as T

import qualified GHC
import qualified Lexer                  as GHC
import qualified FastString             as GHC

attrMap :: A.AttrMap
attrMap = A.attrMap V.defAttr
    [ (editAttr, V.white `on` V.black)
     ,(A.attrName "ITinteger", V.withStyle (V.green `on` V.black) V.bold)
     ,(A.attrName "ITvarid", V.withStyle (V.blue `on` V.black) V.bold)
    ]

mkLocatedToken :: Int -> Int -> Int -> Int -> GHC.Token -> GHC.Located GHC.Token
mkLocatedToken lin1 col1 lin2 col2 = 
  GHC.L (GHC.mkSrcSpan 
    (GHC.mkSrcLoc (GHC.fsLit "") lin1 col1)
    (GHC.mkSrcLoc (GHC.fsLit "") lin2 col2))

lineABC = " abc " :: T.Text

tokensLineABC = [mkLocatedToken 1 2 1 5 $ GHC.ITvarid (GHC.fsLit "abc")]
