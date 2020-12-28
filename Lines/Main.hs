-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Inspired by: https://functional-art.org/2020/papers/Poetry-OleksandrZhabenko.pdf from the https://functional-art.org/2020/performances ;
-- Allows to rewrite the given text (usually a poetical one).

{-# OPTIONS_GHC -threaded -rtsopts #-}

{-# LANGUAGE BangPatterns #-}

module Main where

import Phonetic.Languages.Simplified.Lists.DeEnCoding (newLineEnding)
import System.IO
import Data.SubG
import Data.MinMax.Preconditions
import qualified Data.Vector as VB
import Data.List (sort)
import Phonetic.Languages.Lists.Ukrainian.PropertiesSyllablesG2
import Phonetic.Languages.Simplified.StrictVG
import Phonetic.Languages.Permutations
import Languages.UniquenessPeriods.Vector.Filters (unsafeSwapVecIWithMaxI)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import System.Environment
import Languages.Phonetic.Ukrainian.PrepareText
import Phonetic.Languages.Simplified.DataG
import Data.Char (isDigit)
import Phonetic.Languages.Simplified.Lists.Ukrainian.FuncRep2RelatedG2
import Data.Monoid (mappend)

-- | The function allows to rewrite the Ukrainian text in the file given as the first command line argument to a new file. In between, it is rewritten
-- so that every last word on the lines is preserved at its position, and the rest of the line is rearranged using the specified other command line
-- arguments. They are general for the whole program. The first command line argument is a FilePath to the file with a Ukrainian text to be rewritten.
-- The second one is a variant of the \"properties\" used to evaluate the variants.
-- The further command line arguments are: the number of the intervals and the numbers of the intervals
-- that are swapped with the maximum one so that they are available for further usage by the program. See documentation for @uniqueness-periods-vector-filters@
-- package
-- 'https://hackage.haskell.org/package/uniqueness-periods-vector-filters'
--
main :: IO ()
main = do
 args <- getArgs
 let coeffs = readCF . concat . take 1 $ args -- The first command line argument. If not sure, pass just \"1_\".
 if isPair coeffs then do
  let !numericArgs = filter (all isDigit) . drop 3 $ args
      !choice = concat . drop 2 . take 3 $ args
      !numberI = fromMaybe 1 (readMaybe (concat . take 1 $ numericArgs)::Maybe Int)
      !file = concat . drop 1 . take 2 $ args
  generalProcessment coeffs numericArgs choice numberI file
 else do
  let !numericArgs = filter (all isDigit) . drop 2 $ args
      !choice = concat . drop 1 . take 2 $ args
      !numberI = fromMaybe 1 (readMaybe (concat . take 1 $ numericArgs)::Maybe Int)
      !file = concat . take 1 $ args
  generalProcessment coeffs numericArgs choice numberI file

generalProcessment :: Coeffs2 -> [String] -> String -> Int -> FilePath -> IO ()
generalProcessment coeffs numericArgs choice numberI file = do
  contents <- readFile file
  let !permsV = VB.force genPermutationsVL
      !flines = fLines contents
      !lasts = map (\ts -> if null . words $ ts then [] else last . words $ ts) flines
  if compare numberI 2 == LT then toFileStr (file ++ ".new.txt") (circle2 coeffs permsV choice [] $ flines)
  else do
    let !intervalNmbrs = (\vs -> if null vs then VB.singleton numberI else VB.uniq . VB.fromList $ vs) . sort . filter (<= numberI) .
           map (\t -> fromMaybe numberI (readMaybe t::Maybe Int)) . drop 2 $ numericArgs
        !us = words . concat . take 1 $ flines
        !l2 = (subtract 3) . length $ us
    if compare l2 0 /= LT then do
      let !perms2 = VB.unsafeIndex permsV $ l2
          (!minE,!maxE) = let !frep20 = chooseMax id coeffs choice in minMax11C . map (toPropertiesF' frep20) .
                    uniquenessVariants2GNPBL [] (concat . take 1 $ lasts) ' ' id id id perms2 . init $ us
      toFileStr (file ++ ".new.txt") (circle2I coeffs permsV choice [] numberI intervalNmbrs minE maxE $ flines)
    else toFileStr (file ++ ".new.txt") ((concat . take 1 $ flines):(circle2I coeffs permsV choice [] numberI intervalNmbrs 0.0 0.0 . drop 1 $ flines))

fLines :: String -> [String]
fLines ys =
  let preText = prepareText ys
      wss = map (length . subG " 01-") preText
      g (t:ts) (r:rs) = if r > 7 then filter (`notElem` "01-") t:g ts rs else t:g ts rs
      g _ _ = []
       in g preText wss

-- | Processment without rearrangements.
circle2 :: Coeffs2 -> VB.Vector [VB.Vector Int] -> String -> [String] -> [String] -> [String]
circle2 coeffs permsG1 choice yss xss
 | null xss = yss
 | otherwise = circle2 coeffs permsG1 choice (yss `mappend` [ws]) tss
      where (!zss,!tss) = splitAt 1 xss
            !rs = words . concat $ zss
            !l = length rs
            !frep2 = chooseMax id coeffs choice
            !ws = if compare l 3 == LT then unwords rs else line . maximumElR . map (toResultR frep2) .
               uniquenessVariants2GNPBL [] (last rs) ' ' id id id (VB.unsafeIndex permsG1 (l - 3)) . init $ rs


-- | Processment with rearrangements.
circle2I :: Coeffs2 -> VB.Vector [VB.Vector Int] -> String -> [String] -> Int -> VB.Vector Int -> Double -> Double -> [String] -> [String]
circle2I coeffs permsG1 choice yss numberI vI minE maxE xss
 | null xss = yss
 | otherwise = circle2I coeffs permsG1 choice (yss `mappend` [ws]) numberI vI minE1 maxE1 tss
      where (!zss,!tss) = splitAt 1 xss
            !w2s = words . concat . take 1 $ tss
            !l3 = (subtract 3) . length $ w2s
            !rs = words . concat $ zss
            !l = length rs
            !frep2 = chooseMax (unsafeSwapVecIWithMaxI minE maxE numberI vI) coeffs choice
            !ws = if compare (length rs) 3 == LT then unwords rs else line . maximumElR . map (toResultR frep2) .
               uniquenessVariants2GNPBL [] (last rs) ' ' id id id (VB.unsafeIndex permsG1 (l - 3)) . init $ rs
            (!minE1,!maxE1)
             | compare l3 0 /= LT =
               let !perms3 = VB.unsafeIndex permsG1 l3
                   !v4 = init w2s
                   !frep20 = chooseMax id coeffs choice in minMax11C . map (toPropertiesF' frep20) .
                      uniquenessVariants2GNPBL [] (last w2s) ' ' id id id perms3 $ v4
             | otherwise = (0.0,0.0)

-- | Prints every element from the structure on the new line to the file. Uses 'appendFile' function inside. Is taken from
-- the Languages.UniquenessPeriods.Vector.General.DebugG module from the @phonetic-languages-general@ package.
toFileStr ::
  FilePath -- ^ The 'FilePath' to the file to be written in the 'AppendMode' (actually appended with) the information output.
  -> [String] -- ^ Each element is appended on the new line to the file.
  -> IO ()
toFileStr file xss = mapM_ (\xs -> appendFile file (xs `mappend` newLineEnding)) xss
