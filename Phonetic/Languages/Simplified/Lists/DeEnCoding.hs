-- |
-- Module      :  Phonetic.Languages.Simplified.Lists.DeEnCoding
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Functions to encode and decode 'String' -> \['Int8'\] used in the Simple/Main.hs code.

{-# LANGUAGE BangPatterns #-}

module Phonetic.Languages.Simplified.Lists.DeEnCoding where

import Data.Heap (Heap)
import qualified Data.Heap as Heap
import GHC.Int
import Data.Foldable (foldl')
import Data.List (sortBy,sort)
import System.IO

--default (Int, Double)

encodeToInt :: [String] -> Int
encodeToInt yss = foldl' (\x y -> x * 10 + y) 0 . map (\(j,_) -> fromEnum j) . sortBy (\x y -> compare (snd x) (snd y)) . trans2 $ yss
{-# INLINABLE encodeToInt #-}


-- | Is taken mostly from the Phonetic.Languages.Simplified.Lists.UniquenessPeriodsG module from the @phonetic-languages-simplified-common@ package.
indexedL :: Foldable t => b -> t b -> [(Int8, b)]
indexedL y zs = foldr f v zs
  where !v = [(toEnum (length zs + 1),y)]
        f x ((j,z):ys) = (j-1,x):(j,z):ys
{-# INLINE indexedL #-}

trans2 :: [[a]] -> [(Int8, [a])]
trans2 = init . indexedL []
{-# INLINE trans2 #-}

trans232 :: [[a]] -> [(Int, [a])]
trans232 zs = init . foldr f v $ zs
  where !v = [(length zs + 1,[])]
        f x ((j,z):ys) = (j-1,x):(j,z):ys
{-# INLINE trans232 #-}

int2l :: Int -> [Int8]
int2l n
 | n < 10 = [toEnum n]
 | otherwise = int2l n1 `mappend` [l]
     where (!n1,!l0) = quotRem n 10
           !l = toEnum l0
{-# INLINABLE int2l #-}

-- | So:
-- > decodeToStr (int2l . encodeToInt . words $ xs) xs == unwords . words $ xs
--
decodeToStr :: [Int8] -> String -> String
decodeToStr ys = unwords . map snd . sortBy (\x y -> compare (fst x) (fst y)) . zip ys . sort . words
{-# INLINE decodeToStr #-}

-- | Every 'String' consists of words with whitespace symbols in between.
toHeap :: [String] -> Heap Int
toHeap yss@(xs:xss)
  | null xss = Heap.singleton . encodeToInt . words $ xs
  | otherwise = Heap.fromList . map (encodeToInt . words) $ yss
toHeap _ = Heap.empty
{-# INLINE toHeap #-}

fromHeap :: String -> Heap Int -> [String]
fromHeap ys heap
 | Heap.null heap = []
 | otherwise = map (flip decodeToStr ys . int2l) . Heap.toUnsortedList $ heap
{-# INLINE fromHeap #-}

intersectInterResults :: [String] -> [String] -> [String]
intersectInterResults zss
 | null zss = const []
 | otherwise = fromHeap (head zss) . Heap.intersect (toHeap zss) . toHeap
{-# INLINE intersectInterResults #-}

-- | Auxiliary printing function to define the line ending in some cases. Is taken from the
-- Languages.UniquenessPeriods.Vector.General.DebugG module from the @phonetic-languages-general@ package
newLineEnding :: String
newLineEnding
  | nativeNewline == LF = "\n"
  | otherwise = "\r\n"
