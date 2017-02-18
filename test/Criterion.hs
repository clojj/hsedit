{-# LANGUAGE OverloadedStrings #-}

import           EditRope

import Criterion.Main

import TestHelper

main :: IO ()
main = do

  let toTestEmpty = renderTokensForLine [] attrMap
      toTestLineABC = renderTokensForLine tokensLineABC attrMap

  defaultMain [
    bgroup "renderTokensForLine" [
                   bench "original line" $ whnf toTestEmpty lineABC
                 , bench "lineABC" $ whnf toTestLineABC lineABC
                 ]
    ]