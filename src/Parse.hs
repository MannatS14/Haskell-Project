{-|
Module      : Parse
Description : JSON parsing module using Aeson
Copyright   : (c) Group X, 2025
License     : BSD3
Maintainer  : example@example.com
Stability   : experimental
Portability : POSIX

This module handles parsing of JSON responses from the TfL API into Haskell data types.
It utilizes GHC Generics and Aeson for efficient decoding.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Parse where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Types

-- | Parse JSON data into a list of Lines
-- Decodes the API response for line comparisons.
parseLines :: LBS.ByteString -> Either String [Line]
parseLines = eitherDecode

-- | Parse JSON data into a list of Stations
-- Decodes the API response containing StopPoints.
parseStations :: LBS.ByteString -> Either String [Station]
parseStations = eitherDecode

-- | Generate JSON from data and write to file
-- Encodes a list of Lines into JSON and saves it to the specified FilePath.
writeJson :: FilePath -> [Line] -> IO ()
writeJson path lines = LBS.writeFile path (encode lines)

-- | FromJSON and ToJSON instances
-- We use default generic instances which match the field names if they align with JSON
-- Note: TfL API uses "id", "name", "lineStatuses" etc.
-- Our Types.hs uses "id", "name", "lineStatuses".
-- However, "lineStatuses" in JSON might be "lineStatuses" or "LineStatuses".
-- TfL API usually uses camelCase.
-- "statusId" in LineStatus might be "id".
-- We might need custom instances if field names don't match exactly.
-- For now, let's assume they match or we'll fix it after testing.

instance FromJSON Line
instance ToJSON Line
instance FromJSON LineStatus where
    parseJSON = withObject "LineStatus" $ \v -> LineStatus
        <$> v .: "id"
        <*> v .: "statusSeverity"
        <*> v .: "statusSeverityDescription"
        <*> v .:? "reason"

instance ToJSON LineStatus
instance FromJSON Station where
    parseJSON = withObject "Station" $ \v -> Station
        <$> v .: "id"
        <*> v .: "commonName"
        <*> v .: "lat"
        <*> v .: "lon"
instance ToJSON Station

-- | Parse JSON data into a JourneyResponse
-- Decodes the complex Journey planner API response.
parseJourney :: LBS.ByteString -> Either String JourneyResponse
parseJourney = eitherDecode

instance FromJSON JourneyResponse
instance FromJSON Journey
instance FromJSON Leg where
    parseJSON = withObject "Leg" $ \v -> Leg
        <$> v .: "duration"
        <*> v .: "instruction"
        <*> v .: "mode"
        <*> v .: "departurePoint"
        <*> v .: "arrivalPoint"

instance FromJSON Instruction
instance FromJSON Mode where
    parseJSON = withObject "Mode" $ \v -> Mode
        <$> v .: "id"
        <*> v .: "name"

instance FromJSON Point where
    parseJSON = withObject "Point" $ \v -> Point
        <$> v .: "commonName"
