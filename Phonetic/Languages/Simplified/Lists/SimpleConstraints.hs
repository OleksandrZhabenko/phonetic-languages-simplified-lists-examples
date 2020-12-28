-- |
-- Module      :  Phonetic.Languages.Simplified.Lists.SimpleConstraints
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Some code shared for different Main modules.

module Phonetic.Languages.Simplified.Lists.SimpleConstraints where

import Data.Monoid

showB :: Int -> Bool -> String
showB n bool
 | n >= 2 && bool == True = 'B':show (n - 1) `mappend` concatMap show [0..n - 2]
 | otherwise = ""
