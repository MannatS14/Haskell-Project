# Haskell FP Project Report - TfL API App

**Group Members:**
- Nishant Kumar (250354168)
- [Member 2]
- [Member 3]

## Introduction
This application is a stack-based Haskell project designed to harvest data from the Transport for London (TfL) API, store it in an SQLite database, and allow users to query the data. The application focuses on Tube line statuses and station information.

## Architecture & Design
The project is structured into modular components as required:

- **Types.hs**: Defines the core data types (`Line`, `LineStatus`, `Station`) matching the JSON structure from the TfL API. We used `GHC.Generics` and `Aeson` for automatic JSON parsing.
- **Fetch.hs**: Handles HTTP requests using `http-conduit`. It includes functions to fetch line statuses and stop points (stations) for each line. It manages API authentication using the provided App ID and Key.
- **Parse.hs**: Responsible for parsing the raw JSON ByteString into Haskell data types using `aeson`. It also handles writing data back to JSON files.
- **Database.hs**: Manages the SQLite database using `sqlite-simple`. It handles schema creation (`initialiseDB`), data insertion (`saveData`, `saveStations`), and retrieval/search (`retrieveData`, `searchStations`). We used normalized tables for `lines`, `statuses`, `stations`, and a join table `line_stations` to represent the many-to-many relationship between lines and stations.
- **Main.hs**: The entry point of the application. It parses command-line arguments and dispatches control to the appropriate modules.

## How to Run
The application is built using `stack`.

1.  **Build the project:**
    ```bash
    stack build
    ```

2.  **Initialize the database:**
    ```bash
    stack run -- create
    ```
    This creates `tfl.db` with the necessary tables.

3.  **Download and Load Data:**
    ```bash
    stack run -- loaddata
    ```
    This fetches the current status of all Tube lines and, for the extra feature, fetches all stations for each line. Note: This may take a minute as it makes multiple API calls.

4.  **Dump Data to JSON:**
    ```bash
    stack run -- dumpdata
    ```
    This exports the line data to `data.json`.

5.  **Search Stations (Extra Feature):**
    ```bash
    stack run -- search "Stratford"
    ```
    This searches the database for stations matching the query string.

## Extra Feature: Station Search & Connectivity
For the extra challenging feature, we implemented **Station Fetching and Search**.
- **Challenge**: The basic requirement was to fetch a single JSON document. We went beyond this by fetching the `StopPoints` (stations) for *every* tube line. This involved making multiple HTTP requests in a loop (`Fetch.fetchStations`) and parsing a different JSON structure.
- **Database Complexity**: We implemented a many-to-many relationship between Lines and Stations (`line_stations` table) because a single station (e.g., King's Cross) can be on multiple lines.
- **Functionality**: Users can search for stations by name using the `search` command, which queries the `stations` table using SQL `LIKE` pattern matching.

## Technical Details
- **JSON Parsing**: We used `aeson`'s generic derivation for `FromJSON` and `ToJSON` to reduce boilerplate code.
- **Database**: We used `sqlite-simple` for type-safe database interactions.
- **Error Handling**: We handled JSON parsing errors gracefully in `Main.hs`.

## Conclusion
The application successfully demonstrates functional programming concepts in Haskell, including IO handling, JSON parsing, database management, and modular design.
