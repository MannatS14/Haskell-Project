{-|
Module      : Parse
Description : JSON parsing module
In this file we are parsing the JSON data from the TfL API into Haskell data types.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Parse where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Types

-- | Parse the JSON data into a list of Lines
parseLines :: LBS.ByteString -> Either String [Line]
parseLines = eitherDecode

-- | Parse the JSON data into a list of Stations
parseStations :: LBS.ByteString -> Either String [Station]
parseStations = eitherDecode

-- | Generate the JSON from data and write to file
writeJson :: FilePath -> [Line] -> IO ()
writeJson path lines = LBS.writeFile path (encode lines)

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

-- | Parse the JSON data into a JourneyResponse
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
