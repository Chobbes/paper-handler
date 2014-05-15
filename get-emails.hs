#!/usr/bin/env runhaskell

{- Copyright (C) 2014 Calvin Beck

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

import System.Environment
import Data.Char
import Data.Function
import Data.List.Split
import Data.List hiding (lookup)
import Data.Maybe
import Data.Map hiding (map, null, mapMaybe)
import Prelude hiding (lookup)
import Control.Applicative

main = do
  [emailFile, inviteFile, outputFile] <- getArgs
  emails <- readFile emailFile
  invites <- readFile inviteFile
  mapM (interactMail outputFile (pairMap emails)) $ lines invites

interactMail filePath addresses nameLine
    | null mailPairs = putStrLn $ "Not found: " ++ nameLine
    | length mailPairs == 1 = putStrLn ("Found: " ++ nameLine) >>
                              appendFile filePath (head mailPairs ++ "\n")
    | otherwise = do putStrLn ("\nChoices: " ++ nameLine ++ "\n" ++ choices)
                     choice <- getLine
                     appendFile filePath ((mailPairs !! read choice) ++ "\n")
    where
      choices = unlines $ zipWith (\x y -> show x ++ ": " ++ y) [0..] mailPairs
      name = (head . words) nameLine
      mailPairs = mailLookup addresses name

mailLookup :: Map String [String] -> String -> [String]
mailLookup addresses name = map ((upperName ++ ":") ++) <$> fromMaybe [] $ lookup lowerName addresses
    where
      lowerName = map toLower name
      upperName = toUpper (head name) : tail name

pairMap :: String -> Map String [String]
pairMap = fromListWith (++) . map (mapSnd (:[])) . mapMaybe getPair . lines
    where
      mapSnd f (x, y) = (x, f y)

getPair :: String -> Maybe (String, String)
getPair str
    | numEntries == 2 = Just (map toLower name, email)
    | otherwise = Nothing
    where
      [name, email] = entries
      entries = splitOn ":" str
      numEntries = length entries
