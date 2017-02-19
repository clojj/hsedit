{-# LANGUAGE OverloadedStrings #-}

import           EditRope

import Criterion.Main

import TestHelper

main :: IO ()
main = do

  let toTest = renderTokensForLine attrMap

  defaultMain [
    bgroup "renderTokensForLine" [
                   bench "original line" $ whnf (toTest []) lineABC
                 , bench "lineABC" $ whnf (toTest tokensLineABC) lineABC
                 , bench "lineABC_123" $ whnf (toTest tokensLineABC_123) lineABC_123
                 , bench "lineABC_123_rev" $ whnf (toTest tokensLineABC_123_rev) lineABC_123
                 ]
    ]