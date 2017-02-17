{-# LANGUAGE OverloadedStrings #-}

import qualified Brick.AttrMap      as A
import           EditRope
import qualified Graphics.Vty       as V
import           Brick.Util             (on)

import Criterion.Main

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

      toTestEmpty = renderTokensForLine tokensEmpty attrMap
      toTest1 = renderTokensForLine tokens1 attrMap

  defaultMain [
    bgroup "renderTokensForLine" [
                   bench "original line" $ whnf toTestEmpty lineABC
                 , bench "lineABC" $ whnf toTest1 lineABC
                 ]
    ]