{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Lib.Monitor.Utils where

import qualified Data.Char as Char
import Data.Time
import Data.Time.Format (formatTime, parseTime)

capitalized :: String -> String
capitalized (x:xs) = Char.toUpper x : xs
capitalized [] = []

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%QZ"

parseISO8601 :: String -> Maybe UTCTime
parseISO8601 t = parseTimeM True defaultTimeLocale "%FT%T%QZ" t
