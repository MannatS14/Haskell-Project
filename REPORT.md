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

<<<<<<< HEAD
5.  **Run Queries:**
    ```bash
    stack run -- query 10
    ```
    This runs a query against the database. For example, `10` searches for lines with "Good Service".

6.  **Plan a Journey (Extra Feature):**
    ```bash
    stack run -- plan-journey
    ```
    This interactive command allows you to plan a journey between two stations, selecting modes and preferences.

## Extra Feature: Interactive Journey Planner
For the extra challenging feature, we implemented a full **Introduction to Pathfinding via API Integration**.
- **Challenge**: Instead of just fetching static data, we integrated the complex `Journey` endpoint from the TfL API. This required handling dynamic user input sequences (From -> To -> Modes -> Preferences) and parsing a deeply nested JSON response containing `Journeys`, `Legs`, `Instructions`, and `Modes`.
- **Functionality**:
    *   **Interactive CLI**: Users are guided through a step-by-step process.
    *   **Station Resolution**: If a user types a partial station name (e.g., "Stratford"), the app searches the database and lets the user disambiguate from a list of matches.
    *   **Customizable**: Users can filter by transport mode (Tube, Bus, DLR, etc.) and routing preference (Fastest, Least Walking, etc.).
    *   **Result Display**: The app displays multiple journey options with duration, transfer details, and walking instructions, sorted by the user's preference.

## Technical Details
- **Architecture**: We strictly followed the Model-View-Controller (MVC) pattern adapted for a CLI app. `Types.hs` (Model), `Main.hs` (View/Controller), and `Fetch/Parse/Database` (Services).
- **JSON Parsing**: We used `aeson`'s generic derivation for `FromJSON` and `ToJSON`, handling optional fields (Many `Maybe Text`) correctly to avoid runtime crashes on missing API data.
- **Database**: We used `sqlite-simple` for type-safe database interactions, normalizing data into `lines`, `statuses`, `stations`, and `line_stations`.
- **Error Handling**: We implemented a robust error handling wrapper (`withErrorHandling`) in `Main.hs` to catch HTTP exceptions, Database errors, and parsing failures, presenting them as user-friendly messages instead of stack traces.
- **Documentation**: All modules are documented using Haddock syntax.

## Conclusion
The application successfully demonstrates functional programming concepts in Haskell, including IO handling, JSON parsing, database management, and modular design. It exceeds the requirements by implementing a complex, interactive Journey Planner that solves a real-world problem.

=======
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
>>>>>>> parent of 8c04d89 (Merged PR #1: Resolved conflicts in source code and accepted build file deletion.)
