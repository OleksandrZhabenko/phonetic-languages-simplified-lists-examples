-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Analyzes a poetic text in Ukrainian, for every line prints statistic data and
-- then for the whole poem prints the hypothesis evaluation information.
--
-- To enable parallel computations (potentially, they can speed up the work), please, run the @propertiesText@ executable with
-- @+RTS -threaded -RTS@ command line options with possibly @-N@ option inside.
--

{-# OPTIONS_GHC -threaded -rtsopts #-}

{-# LANGUAGE BangPatterns, FlexibleContexts #-}

module Main where


import Data.SubG hiding (takeWhile,dropWhile)
import System.IO
import Control.Concurrent
import Control.Exception
import Control.Parallel.Strategies
import Data.Maybe (fromMaybe)
import Data.List (sort)
import Text.Read (readMaybe)
import qualified Data.Vector as VB
import Melodics.ByteString.Ukrainian
import System.Environment
import Languages.Phonetic.Ukrainian.PrepareText
import Numeric (showFFloat)
import Languages.UniquenessPeriods.Vector.Filters
import Data.Char (isAlpha)
import Data.Statistics.RulesIntervalsPlus
import Data.MinMax.Preconditions
import Phonetic.Languages.Lists.Ukrainian.PropertiesSyllablesG2
import Phonetic.Languages.Simplified.StrictVG
import Phonetic.Languages.Permutations
import Phonetic.Languages.Simplified.DataG
import Phonetic.Languages.Simplified.Lists.Ukrainian.FuncRep2RelatedG2
import Languages.UniquenessPeriods.Vector.Constraints.Encoded
import Phonetic.Languages.Simplified.Lists.SimpleConstraints

main :: IO ()
main = do
 args000 <- getArgs
 let !args00 = filter (/= "++B") args000
     !lstW = any (== "++B") args000
     !args0 = takeWhile (/= "+M") args00 `mappend` drop 1 (dropWhile (/= "-M") args00)
     !multiples = drop 1 . dropWhile (/= "+M") . takeWhile (/= "-M") $ args00 -- Arguments for multiple metrices mode
     !args = filter (\xs -> all (/= ':') xs && all (/= '@') xs) args0
     !coeffs = readCF . concat . take 1 $ args -- The first command line argument. If not sure, just enter \"1_\".
     !lInes = filter (any (== ':')) args0
     !numbersJustPrint =  filter (== "@n") args0
 if isPair coeffs then do
  let !file = concat . drop 1 . take 2 $ args  -- The second command line argument except those ones that are RTS arguments
  if null numbersJustPrint then do
   let !gzS = concat . take 1 . drop 2 $ args -- The third command line argument that controls the choice of the number of intervals
       !printLine = fromMaybe 0 (readMaybe (concat . take 1 . drop 3 $ args)::(Maybe Int)) -- The fourth command line argument except those ones that are  RTS arguments. Set to 1 if you would like to print the current line within the information
       !toOneLine = fromMaybe 0 (readMaybe (concat . take 1 . drop 4 $ args)::(Maybe Int)) -- The fifth command line argument except those ones that are RTS arguments. Set to 1 if you would like to convert the text into one single line before applying to it the processment (it can be more conceptually consistent in such a case)
       !choice = concat . drop 5 . take 6 $ args -- The sixth command line argument that controls what properties are used.
   generalProc lstW multiples lInes coeffs file gzS printLine toOneLine choice
  else do
   contents <- readFile file
   fLinesIO contents
 else do
  let !file = concat . take 1 $ args
  if null numbersJustPrint then do
   let !gzS = concat . take 1 . drop 1 $ args
       !printLine = fromMaybe 0 (readMaybe (concat . take 1 . drop 2 $ args)::(Maybe Int))
       !toOneLine = fromMaybe 0 (readMaybe (concat . take 1 . drop 3 $ args)::(Maybe Int))
       !choice = concat . drop 4 . take 5 $ args
   generalProc lstW multiples lInes coeffs file gzS printLine toOneLine choice
  else do
   contents <- readFile file
   fLinesIO contents

generalProc :: Bool -> [String] -> [String] -> Coeffs2 -> FilePath -> String -> Int -> Int -> String -> IO ()
generalProc lstW multiples2 lInes coeffs file gzS printLine toOneLine choice
 | null lInes = do
    contents <- readFile file
    let !flines = fLines toOneLine contents
    getData3 lstW coeffs (getIntervalsNS lstW gzS flines) printLine choice multiples2 flines
 | otherwise = do
    contents <- readFile file
    let !flines = fLines toOneLine . unlines . linesFromArgsG lInes . fLines 0 $ contents
    getData3 lstW coeffs (getIntervalsNS lstW gzS flines) printLine choice multiples2 flines

linesFromArgs1 :: Int -> String -> [String] -> [String]
linesFromArgs1 n xs yss =
  let (!ys,!zs) = (\(x,z) -> (x, drop 1 z)) . break (== ':') $ xs
      !ts = sort . map (min n . abs) $ [fromMaybe 1 (readMaybe ys::Maybe Int), fromMaybe n (readMaybe zs::Maybe Int)] in
        drop (head ts - 1) . take (last ts) $ yss

linesFromArgsG :: [String] -> [String] -> [String]
linesFromArgsG xss yss = let n = length yss in concatMap (\ts -> linesFromArgs1 n ts yss) xss

getData3 :: Bool -> Coeffs2 -> Int -> Int -> String -> [String] -> [String] -> IO ()
getData3 lstW coeffs gz printLine choice multiples3 zss = let !permsV4 = genPermutationsVL in putStrLn (replicate (length multiples3 + 1) '\t' `mappend` show gz) >> mapM_ (process1Line lstW coeffs gz printLine choice multiples3 permsV4) zss

process1Line :: Bool -> Coeffs2 -> Int -> Int -> String -> [String] -> VB.Vector [VB.Vector Int] -> String -> IO ()
process1Line lstW coeffs gz printLine choice multiples4 !permsV50 v
 | null multiples4 = bracket (do {
    myThread <- forkIO (do
     let !v2 = words v
         !l2 = length v2 - 2
     if l2 >= (if lstW then 1 else 0) then do
      let !permsV5 = decodeConstraint1 (fromMaybe (E 1) . readMaybeECG (l2 + 1) . showB (l2 + 2) $ lstW) .
            VB.unsafeIndex permsV50 $ l2
          ((!minE,!maxE),!data2) = runEval (parTuple2 rpar rpar (minMax11C . map (toTransPropertiesF' (chooseMax id coeffs choice )) .
                uniquenessVariants2GNBL ' ' id id id permsV5 $ v2, toTransPropertiesF' (chooseMax  id coeffs choice) . unwords . subG " 01-" $ v))
          (!wordsN,!intervalN) = (l2 + 2, intervalNRealFrac minE maxE gz data2)
          !ratio = if maxE == 0.0 then 0.0 else 2.0 * data2 / (minE + maxE)
      hPutStr stdout . showFFloat (precChoice choice) minE $ "\t"
      hPutStr stdout . showFFloat (precChoice choice) data2 $ "\t"
      hPutStr stdout . showFFloat (precChoice choice) maxE $ "\t"
      hPutStr stdout . showFFloat (Just 4) (data2 / minE) $ "\t"
      hPutStr stdout . showFFloat (Just 4) (maxE / minE) $ "\t"
      hPutStr stdout . showFFloat (Just 4) (maxE / data2) $ "\t"
      hPutStr stdout . showFFloat (Just 8) ratio $ "\t"
      hPutStr stdout ('\t':show (wordsN::Int))
      hPutStr stdout ('\t':show (intervalN::Int))
      hPutStrLn stdout (if printLine == 1 then '\t':v else "")
     else putStrLn (replicate (length multiples4) '\t' ++ if printLine == 1 then '\t':v else ""))
   ; return myThread }) (killThread) (\_ -> putStr "")
 | otherwise = bracket (do {
   myThread <- forkIO (do
    let !v2 = words v
        !l2 = length v2 - 2
    if l2 >= (if lstW then 1 else 0) then do
     let !permsV5 = decodeConstraint1 (fromMaybe (E 1) . readMaybeECG (l2 + 1) . showB (l2 + 2) $ lstW) .
            VB.unsafeIndex permsV50 $ l2
         rs = parMap rpar (\choiceMMs -> (minMax11C .
           map (toTransPropertiesF' (chooseMax id coeffs choiceMMs)) .
             uniquenessVariants2GNBL ' ' id id id permsV5 $ v2,
               toTransPropertiesF' (chooseMax  id coeffs choiceMMs) . unwords . subG " 01-" $ v,gz)) multiples4
         (!wordsN,!intervalNs) = (l2 + 2, map (\((!x,!y),!z,!t) -> intervalNRealFrac x y t z) rs)
           in do
            hPutStr stdout (show (wordsN::Int))
            mapM_ (\i -> hPutStr stdout ('\t':show (i::Int))) intervalNs
            hPutStrLn stdout (if printLine == 1 then '\t':v else "")
    else putStrLn (replicate (length multiples4) '\t' ++ if printLine == 1 then '\t':v else ""))
  ; return myThread }) (killThread) (\_ -> putStr "")

fLines :: Int -> String -> [String]
fLines !toOneLine ys =
  let preText = filter (any (\x -> isUkrainianL x && isAlpha x)) . prepareText . (\z -> if toOneLine == 1 then unwords . words $ z else z) $ ys
      wss = map (length . subG " 01-") preText
      g (t:ts) (r:rs) = if r > 7 then filter (`notElem` "01-") t:g ts rs else t:g ts rs
      g _ _ = []
        in g preText wss

fLinesIO :: String -> IO ()
fLinesIO ys =
  let preText = filter (any (\x -> isUkrainianL x && isAlpha x)) . prepareText $ ys
      wss = map (length . subG " 01-") preText
      g (t:ts) (r:rs) = if r > 7 then filter (`notElem` "01-") t:g ts rs else t:g ts rs
      g _ _ = []
        in VB.mapM_ putStrLn . VB.map (\(i,x) -> show (i + 1) ++ "\t" ++ x) . VB.indexed . VB.fromList . g preText $ wss
