-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Prints the rearrangements with the \"property\" information for the Ukrainian language text.

{-# OPTIONS_GHC -threaded -rtsopts #-}

{-# LANGUAGE BangPatterns #-}

module Main where

import Numeric
import Languages.UniquenessPeriods.Vector.Constraints.Encoded (decodeLConstraints,readMaybeECG)
import qualified Data.Vector as VB
import Phonetic.Languages.Simplified.DataG
import Phonetic.Languages.Lists.Ukrainian.PropertiesSyllablesG2
import Languages.UniquenessPeriods.Vector.Filters (unsafeSwapVecIWithMaxI)
import Phonetic.Languages.Simplified.StrictVG
import Languages.Phonetic.Ukrainian.PrepareText
import Data.Char (isDigit,isAlpha)
import Melodics.ByteString.Ukrainian (isUkrainianL)
import qualified Data.List  as L (span,sort,zip4,isPrefixOf)
import Phonetic.Languages.Simplified.Lists.Ukrainian.FuncRep2RelatedG2
import Phonetic.Languages.Permutations
import Data.SubG hiding (takeWhile,dropWhile)
import System.Environment
import Data.Maybe
import Data.MinMax.Preconditions
import Text.Read (readMaybe)
import Phonetic.Languages.Simplified.Lists.DeEnCoding
import Phonetic.Languages.Simplified.Lists.SimpleConstraints


-- | Prints the rearrangements with the \"property\" information for the Ukrainian language text. The first command line argument must be a
-- positive 'Int' number and is a number of printed variants for the line (if they are present, otherwise just all possible variants are printed).
-- The second one is the number of the intervals into which the all range of possible metrics values are divided. The next numeric arguments that must be
-- sequenced without interruptions further are treated as the numbers of the intervals (counting is started from 1) which values are moved to the maximum
-- values of the metrics interval using the 'unsafeSwapVecIWithMaxI' function. The first textual command line argument should be in the form either \"y0\",
-- or \"0y\", or \"yy\", or \"y\", or \"02y\", or \"y2\", or \"03y\", or \"yy2\", or \"y3\", or some other variant and specifies, which property or properties is or are evaluated.
-- The rest of the command line arguments is the Ukrainian text. Besides, you can use multiple metrices (no more than 5 different ones) together by
-- using \"+M\" ... \"-M\" command line arguments.
--
-- You can specify constraints according to the 'decodeLConstraints' function between +A and -A command line arguments. If so, the program will
-- ask you additional question before proceeding. The \"+M\" ... \"-M\" and \"+A\" ... \"-A\" groups must not mutually intersect one another.
main :: IO ()
main = do
 args00 <- getArgs
 let args0 = filter (\xs -> xs /= "++B" && xs /= "++L" && xs /= "++BL" && xs /= "++I") args00
     lstW = if any (\x -> x == "++B" || x == "++BL") args00 then True else False -- If one of the command line options is \"++B\" or \"++BL\" then the last word of the line will remain the last one.
     jstL0 = if any (\x -> x == "++L" || x == "++BL") args00 then True else False -- If one of the command line options is \"++L\" or \"++BL\" then the program outputs just lines without metrices values.
     toFileMode1 = concat . take 1 . drop 1 . dropWhile (/= "+IF") . takeWhile (/= "-IF") $ args0  -- Prints the last resulting line of the interactive mode processment (the last selected variant) to the file and also to the stdout.
     interactiveP = if any (\xs -> xs == "++I" || xs == "+IF") args00 then True else False -- If one of the command line options is \"++I\", or \"+FIB\", or \"+IF\" then the program prints the variants and then prompts for the preferred variant. Afterwards, it prints just that variant alone.
     args01 = takeWhile (/= "+A") args0 `mappend` (drop 1 . dropWhile (/= "-A") $ args0)
     args02
      | null toFileMode1 = filter (\xs -> xs /= "+IF" && xs /= "-IF") args01
      | otherwise = takeWhile (/= "+IF") args01 `mappend` (drop 1 . dropWhile (/= "-IF") $ args01)
     args = takeWhile (/= "+M") args02 `mappend` (drop 1 . dropWhile (/= "-M") $ args02)
     coeffs = readCF . concat . take 1 $ args -- The first command line argument. If not sure, just pass \"1_\".
 if isPair coeffs then generalProc2 toFileMode1 interactiveP jstL0 args0 coeffs (drop 1 args) lstW
 else generalProc2 toFileMode1 interactiveP jstL0 args0 coeffs args lstW

forMultiplePropertiesF :: [String] -> [(String,[String])]
forMultiplePropertiesF (xs:xss)
 | any isAlpha xs = (xs,yss):forMultiplePropertiesF zss
 | otherwise = []
     where l = length . takeWhile (all isDigit) $ xss
           (yss,zss) = splitAt l xss
forMultiplePropertiesF _ = []

generalProc2 :: FilePath -> Bool -> Bool -> [String] -> Coeffs2 -> [String] -> Bool -> IO ()
generalProc2 toFile1 interactive jstL0 args0 coeffs args lstW2 = do
  let !argMss = take 5 . filter (not . null) . forMultiplePropertiesF . drop 1 . dropWhile (/= "+M") . takeWhile (/= "-M") $ args0
  if null argMss then do
   let (!numericArgs,!textualArgs) = L.span (all isDigit) $ args
       !xs = concat . take 1 . fLines . unwords . drop 1 $ textualArgs
       !l = length . words $ xs
       !argCs = catMaybes (fmap (readMaybeECG (l - 1)) . (showB l lstW2:) . drop 1 . dropWhile (/= "+A") . takeWhile (/= "-A") $ args0)
       !arg0 = fromMaybe 1 $ (readMaybe (concat . take 1 $ numericArgs)::Maybe Int)
       !numberI = fromMaybe 1 $ (readMaybe (concat . drop 1 . take 2 $ numericArgs)::Maybe Int)
       !choice = concat . take 1 $ textualArgs
       !intervalNmbrs = (\zs -> if null zs then VB.singleton numberI else VB.uniq . VB.fromList $ zs) . L.sort . filter (<= numberI) .
           map (\t -> fromMaybe numberI $ (readMaybe t::Maybe Int)) . drop 2 $ numericArgs
   if compare l 2 == LT then let !frep20 = chooseMax id coeffs choice in let !wwss = (:[]) . toResultR frep20 $ xs in
    if interactive then interactivePrintResult line toFile1 wwss else print1el jstL0 choice wwss
   else do
    let !subs = subG " 01-" xs
    if null argCs then let !perms = genPermutationsL l in do
          temp <- generalProcMs coeffs perms subs (intervalNmbrs, arg0, numberI, choice)
          if interactive then interactivePrintResult line toFile1 temp else print1el jstL0 choice temp
    else do
     correct <- printWarning xs
     if correct == "n" then putStrLn "You stopped the program, please, if needed, run it again with better arguments. "
     else let !perms = decodeLConstraints argCs . genPermutationsL $ l in do
          temp <- generalProcMs coeffs perms subs (intervalNmbrs, arg0, numberI, choice)
          if interactive then interactivePrintResult line toFile1 temp else print1el jstL0 choice temp
  else do
   let !choices = map fst argMss
       !numericArgss = map snd argMss
       !arg0s = map (\ts -> fromMaybe 1 $ (readMaybe (concat . take 1 $ ts)::Maybe Int)) numericArgss
       !numberIs = map (\ts -> fromMaybe 1 $ (readMaybe (concat . drop 1 . take 2 $ ts)::Maybe Int)) numericArgss
       !intervalNmbrss = map (\us -> let !numberI = fromMaybe 1 $ (readMaybe (concat . drop 1 . take 2 $ us)::Maybe Int) in
         (\zs -> if null zs then VB.singleton numberI else VB.uniq . VB.fromList $ zs) . L.sort . filter (<= numberI) .
           map (\t -> fromMaybe numberI $ (readMaybe t::Maybe Int)) . drop 2 $ us) $ numericArgss
       !argsZipped = L.zip4 intervalNmbrss arg0s numberIs choices
       !xs = concat . take 1 . fLines . unwords $ args
       !l = length . words $ xs
       !argCs = catMaybes (fmap (readMaybeECG (l - 1)) . (showB l lstW2:) . drop 1 . dropWhile (/= "+A") . takeWhile (/= "-A") $ args0)
   if compare l 2 == LT then let !frep20 = chooseMax id coeffs (concat . take 1 $ choices) in let !wwss = (:[]) . toResultR frep20 $ xs in if interactive then interactivePrintResult line toFile1 wwss else print1el jstL0 (concat . take 1 $ choices) wwss
   else do
    let !subs = subG " 01-" xs
    if null argCs then let !perms = genPermutationsL l in generalProcMMs interactive toFile1 coeffs argsZipped perms subs
    else do
     correct <- printWarning xs
     if correct == "n" then putStrLn "You stopped the program, please, if needed, run it again with better arguments. "
     else let !perms = decodeLConstraints argCs . genPermutationsL $ l in generalProcMMs interactive toFile1 coeffs argsZipped perms subs

interactivePrintResult :: (a -> String) -> String -> [a] -> IO ()
interactivePrintResult f ys xss
  | null xss = putStrLn ""
  | otherwise = do
     let !datas = map (\(idx,str) -> show idx `mappend` ('\t' : str)) . trans232 . map f $ xss
     mapM_ putStrLn datas
     putStrLn ""
     putStrLn "Please, specify the variant which you would like to become the resulting string by its number. "
     number <- getLine
     let !lineRes = concat . filter ((number `mappend` "\t")`L.isPrefixOf`) $ datas
     (\xs -> if null ys then putStrLn xs else putStrLn xs >> appendFile ys (xs `mappend` newLineEnding)) . drop 1 . dropWhile (/= '\t') $ lineRes

printWarning :: String -> IO String
printWarning xs = do
  putStr "Please, check whether the line below corresponds and is consistent with the constraints you have specified between the +A and -A options. "
  putStr "Check also whether you have specified the \"++B\" or \"++BL\" option(s). "
  putStrLn "If it is inconsistent then enter further \"n\", press Enter and then run the program again with better arguments. "
  putStrLn "If the line is consistent with your input between +A and -A then just press Enter to proceed further. "
  putStrLn xs
  getLine

generalProcMs :: Coeffs2 -> [VB.Vector Int] -> [String] -> (VB.Vector Int,Int,Int,String) -> IO [Result [] Char Double Double]
generalProcMs coeffs perms subs (intervalNmbrs, arg0, numberI, choice) = do
  if compare numberI 2 == LT then let !frep2 = chooseMax id coeffs choice in return . fst . maximumGroupsClassificationR arg0 .
    map (toResultR frep2) . uniquenessVariants2GNBL ' ' id id id perms $ subs
  else do
    let !variants1 = uniquenessVariants2GNBL ' ' id id id perms subs
        !frep20 = chooseMax id coeffs choice
        (!minE,!maxE) = minMax11C . map (toPropertiesF' frep20) $ variants1
        !frep2 = chooseMax (unsafeSwapVecIWithMaxI minE maxE numberI intervalNmbrs) coeffs choice
    return . fst . maximumGroupsClassificationR arg0 . map (toResultR frep2) $ variants1

generalProcMMs :: Bool -> FilePath -> Coeffs2 -> [(VB.Vector Int,Int,Int,String)] -> [VB.Vector Int] -> [String] -> IO ()
generalProcMMs interactiveMM file coeffs rs perms subs =
 case length rs of
  0 -> putStrLn "No data has been specified to control the computation process. "
  1 -> putStrLn "You have specified just one variant of the metrices. " >> do
        temp <- generalProcMs coeffs perms subs (head rs)
        finalProc interactiveMM file line temp
  _ -> do
         genVariants <- mapM (generalProcMs coeffs perms subs) rs
         finalProc interactiveMM file id . foldlI . map (map line) $ genVariants

foldlI :: [[String]] -> [String]
foldlI (xs:ys:xss) = foldlI (intersectInterResults xs ys : xss)
foldlI (xs:_) = xs
foldlI _ = []

finalProc :: Bool -> FilePath -> (a -> String) -> [a] -> IO ()
finalProc bool ys f xss = if bool then interactivePrintResult f ys xss else mapM_ (putStrLn . f) xss

fLines :: String -> [String]
fLines ys =
  let preText = filter (any (\x -> isUkrainianL x && isAlpha x)) . prepareText $ ys
      wss = map (length . subG " 01-") preText
      g (t:ts) (r:rs) = if r > 7 then filter (`notElem` "01-") t:g ts rs else t:g ts rs
      g _ _ = []
        in g preText wss

print1el :: Bool -> String -> [Result [] Char Double Double] -> IO ()
print1el jstlines choice (x:xs)
 | jstlines == True = putStrLn (line x) >> print1el True choice xs
 | otherwise = putStrLn (line x) >> putStrLn (showFFloat ch (propertiesF x) "") >> putStrLn (showFFloat ch (transPropertiesF x) "") >> print1el False choice xs
       where !ch = precChoice choice
print1el _ _ _ = return ()
