-- |
-- Module      :  Phonetic.Languages.Simplified.Lists.Ukrainian.FuncRep2RelatedG2
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Functions to choose from the 'FuncRep' variants.

{-# LANGUAGE BangPatterns #-}

module Phonetic.Languages.Simplified.Lists.Ukrainian.FuncRep2RelatedG2 where

import CaseBi (getBFst')
import qualified Data.Vector as VB
import Phonetic.Languages.Simplified.DataG
import Phonetic.Languages.Lists.Ukrainian.PropertiesFuncRepG2
import Phonetic.Languages.Lists.Ukrainian.PropertiesSyllablesG2

-- | Allows to choose the variant of the computations in case of usual processment.
chooseMax :: (Ord c) => (Double -> c) -> Coeffs2 -> String -> FuncRep2 String Double c
chooseMax g coeffs choice
 | isPair coeffs = getBFst' (procBoth3InvF g coeffs, VB.fromList [("02y",procRhythmicity23F g "02y" coeffs),
    ("03y",procRhythmicity23F g "03y" coeffs), ("0y",procRhythmicity23F g "0y" coeffs),
     ("y",procBothF g coeffs),("y0",procDiverse2F g),("y2",procBoth2F g coeffs),("y3",procBoth3F g coeffs),
       ("yy",procBothInvF g coeffs),("yy2",procBoth2InvF g coeffs)]) choice
 | otherwise = getBFst' (procBoth3InvF g coeffs, VB.fromList [("02y",procRhythmicity23F g "02y" coeffs),
    ("03y",procRhythmicity23F g "03y" coeffs),("0y",procRhythmicity23F g "0y" coeffs),
     ("y",procBothF g coeffs),("y0",procDiverse2F g),("y2",procBoth2F g coeffs),("y3",procBoth3F g coeffs),
       ("yy",procBothInvF g coeffs),("yy2",procBoth2InvF g coeffs)]) choice

-- | Allows to choose precision in the Numeric.showFDouble function being given a choice parameter.
precChoice :: String -> Maybe Int
precChoice = getBFst' (Just 4, VB.fromList [("02y",Just 0),("03y",Just 0),("0y",Just 0),("y",Just 0),("y0",Just 0),("y2",Just 0),("y3",Just 0)])
