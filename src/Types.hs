{-|
Module      : Types
Description : Data types for the application
This module defines the core data structures used throughout the application,
mirroring the TfL API response structure.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Data type representing a TfL Line
-- Based on https://api.tfl.gov.uk/Line/Mode/tube/Status
data Line = Line
    { id :: Text -- ^ The unique identifier for the line (e.g., "bakerloo")
    , name :: Text -- ^ The display name of the line (e.g., "Bakerloo")
    , modeName :: Text -- ^ The mode of transport (e.g., "tube")
    , created :: Maybe Text -- ^ Timestamp of creation (optional)
    , modified :: Maybe Text -- ^ Timestamp of last modification (optional)
    , lineStatuses :: [LineStatus] -- ^ List of current statuses for the line
    } deriving (Show, Generic)

-- | Data type representing the status of a line
data LineStatus = LineStatus
    { statusId :: Int -- ^ Unique identifier for the status
    , statusSeverity :: Int -- ^ Severity level (0-20, where 10 is Good Service)
    , statusSeverityDescription :: Text -- ^ Textual description of severity (e.g., "Good Service")
    , reason :: Maybe Text -- ^ Detailed reason for delay/disruption (optional)
    } deriving (Show, Generic)

-- | Data type representing a Station (StopPoint)
data Station = Station
    { stationId :: Text -- ^ The unique identifier for the station (Naptan ID)
    , commonName :: Text -- ^ The common name of the station
    , lat :: Double -- ^ Latitude
    , lon :: Double -- ^ Longitude
    } deriving (Show, Generic)

-- | Data type representing a Journey Response
data JourneyResponse = JourneyResponse
    { journeys :: [Journey] -- ^ List of possible journey options
    } deriving (Show, Generic)

-- | Data type representing a single Journey option
data Journey = Journey
    { startDateTime :: Text -- ^ Start time of the journey in ISO 8601
    , duration :: Int -- ^ Total duration in minutes
    , arrivalDateTime :: Text -- ^ Arrival time of the journey in ISO 8601
    , legs :: [Leg] -- ^ List of legs (segments) that make up the journey
    } deriving (Show, Generic)

-- | Data type representing a Leg of a journey
data Leg = Leg
    { legDuration :: Int -- ^ Duration of this leg in minutes
    , instruction :: Instruction -- ^ Instruction for this leg
    , mode :: Mode -- ^ Transport mode used for this leg
    , departurePoint :: Point -- ^ Starting point of this leg
    , arrivalPoint :: Point -- ^ Ending point of this leg
    } deriving (Show, Generic)

-- | Data type representing an instruction for a leg
data Instruction = Instruction
    { summary :: Text -- ^ Summary instruction (e.g., "Walk to...")
    , detailed :: Maybe Text -- ^ Detailed instruction
    } deriving (Show, Generic)

-- | Data type representing a transport mode
data Mode = Mode
    { modeId :: Text -- ^ Identifier for the mode (e.g., "walking", "tube")
    , mName :: Text -- ^ Display name for the mode
    } deriving (Show, Generic)

-- | Data type representing a point (departure/arrival)
data Point = Point
    { pointName :: Text -- ^ Name of the point (usually station name)
    } deriving (Show, Generic)
