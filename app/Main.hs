module Main where

import System.Environment (getArgs)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Control.Monad (forM_)

import Fetch (downloadData, fetchStations, fetchJourney)
import Parse (parseLines, writeJson, parseStations, parseJourney)
import Database (initialiseDB, saveData, retrieveData, saveStations, searchStations, getSevereDelays)
import Types (Line(..), Station(..), LineStatus(..), JourneyResponse(..), Journey(..), Leg(..), Instruction(..), Mode(..), Point(..))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["create"] -> do
            putStrLn "Creating database..."
            initialiseDB
            putStrLn "Database created."
            
        ["loaddata"] -> do
            putStrLn "Downloading data..."
            json <- downloadData
            putStrLn "Parsing data..."
            case parseLines json of
                Left err -> putStrLn $ "Error parsing JSON: " ++ err
                Right lines -> do
                    putStrLn $ "Saving " ++ show (length lines) ++ " lines to database..."
                    saveData lines
                    
                    -- Fetch stations for each line
                    putStrLn "Fetching stations for each line (this might take a while)..."
                    forM_ lines $ \line -> do
                        putStrLn $ "Fetching stations for " ++ show (Types.name line)
                        stationJson <- fetchStations (Types.id line)
                        case parseStations stationJson of
                            Left err -> putStrLn $ "Error parsing stations for line " ++ show (Types.name line) ++ ": " ++ err
                            Right stations -> do
                                saveStations (Types.id line) stations
                                putStrLn $ "Saved " ++ show (length stations) ++ " stations."
                                
                    putStrLn "Data saved."

        ["dumpdata"] -> do
            putStrLn "Dumping data to data.json..."
            lines <- retrieveData
            writeJson "data.json" lines
            putStrLn "Data dumped."
            
        ("query":qArgs) -> do
            putStrLn $ "Running query: " ++ unwords qArgs
            -- Implement generic query execution here
            putStrLn "Query functionality not yet implemented."

        ["severe-delays"] -> do
            putStrLn "Checking for severe delays..."
            delays <- getSevereDelays
            if null delays
                then putStrLn "No severe delays found. Good Service on all lines!"
                else mapM_ (\s -> putStrLn $ show (Types.statusSeverityDescription s) ++ ": " ++ show (Types.reason s)) delays

        ["search", queryStr] -> do
            putStrLn $ "Searching for stations matching: " ++ queryStr
            results <- searchStations queryStr
            mapM_ (\s -> putStrLn $ show (Types.commonName s) ++ " (" ++ show (Types.stationId s) ++ ")") results

        ["plan-journey"] -> do
            putStr "From: "
            hFlush stdout
            from <- getLine
            
            putStr "To: "
            hFlush stdout
            to <- getLine
            
            putStr "Modes (comma separated, e.g. tube,bus, or enter for all): "
            hFlush stdout
            modesInput <- getLine
            let modes = if null modesInput then Nothing else Just modesInput
            
            putStr "Preferences (fastest, fewest-changes, least-walking, or enter for none): "
            hFlush stdout
            prefInput <- getLine
            let preference = case prefInput of
                    "fastest" -> Just "leasttime"
                    "fewest-changes" -> Just "leastinterchange"
                    "least-walking" -> Just "leastwalking"
                    _ -> Nothing
            
            putStrLn "Fetching journey options..."
            json <- fetchJourney from to modes preference
            case parseJourney json of
                Left err -> putStrLn $ "Error parsing journey data: " ++ err
                Right (JourneyResponse journeys) -> do
                    putStrLn $ "Found " ++ show (length journeys) ++ " options:"
                    forM_ (zip [1..] journeys) $ \(i, journey) -> do
                        let modes = map (Types.mName . Types.mode) (Types.legs journey)
                        let modeStr = T.intercalate (T.pack ", ") modes
                        putStrLn $ show i ++ ". Duration: " ++ show (Types.duration journey) ++ " min (" ++ T.unpack modeStr ++ ")"
                        putStrLn $ "   Start: " ++ show (Types.startDateTime journey) ++ ", Arrival: " ++ show (Types.arrivalDateTime journey)
                    
                    putStr "Select an option (number): "
                    hFlush stdout
                    selection <- getLine
                    case readMaybe selection of
                        Just n | n > 0 && n <= length journeys -> do
                            let selectedJourney = journeys !! (n - 1)
                            putStrLn "Journey Details:"
                            forM_ (Types.legs selectedJourney) $ \leg -> do
                                putStrLn $ "  - " ++ show (Types.legDuration leg) ++ " min (" ++ show (Types.mName $ Types.mode leg) ++ ")"
                                putStrLn $ "    From: " ++ show (Types.pointName $ Types.departurePoint leg)
                                putStrLn $ "    To:   " ++ show (Types.pointName $ Types.arrivalPoint leg)
                                putStrLn $ "    " ++ show (Types.summary $ Types.instruction leg)
                        _ -> putStrLn "Invalid selection."

        _ -> putStrLn "Usage: stack run -- [create|loaddata|dumpdata|query <args>|plan-journey]"
