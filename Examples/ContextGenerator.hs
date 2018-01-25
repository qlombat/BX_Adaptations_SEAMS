{-# LANGUAGE FlexibleContexts,  TemplateHaskell,  TypeFamilies, OverloadedStrings #-}

module Examples.ContextGenerator where

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.Map


import Lib.Synchronizer.Context

getHourOfDay :: IO Int
getHourOfDay = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
    return hour

getTimestamp :: IO Int
getTimestamp = (read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime) :: IO Int

makeContext :: IO Context
makeContext = do
    hour <- getHourOfDay
    timestamp <- getTimestamp
    emergency <- return True
    return (fromList [("Emergency", B emergency),("HourOfDay", I hour),("Timestamp", I timestamp)])
