# Love Letters

This project demonstrates the use of spatial clustering on census blocks in the Casper, WY area. 
The fictitious application is determining the number and placement of vans that deliver "handwritten" greeting cards within Casper.
The concept is based off of Theodore's job in the movie "Her".
<img width="904" alt="Screen Shot 2022-08-22 at 3 34 56 PM" src="https://user-images.githubusercontent.com/83082268/186483964-61a7f43b-d189-475b-a9ab-5b594ed8b05c.png">

## Director's Commentary
I've written blog posts discussing this project, take a look if you're interested or want some background!
- [Part 1: Data Cleaning & Setup](https://world.hey.com/catlin/spatial-clustering-love-letters-part-1-data-cleaning-setup-52b10d07)
- [Part 2: OSRM & Weighted Clustering](https://world.hey.com/catlin/spatial-clustering-love-letters-part-2-osrm-weighted-clustering-f702106b) (Look at this if you need help setting up OSRM)
- [Part 3: Equalify](https://world.hey.com/catlin/spatial-clustering-love-letters-part-3-equalify-496ccf5f)
- [Part 4: Discrete Event Simulation](https://world.hey.com/catlin/spatial-clustering-love-letters-part-4-discrete-event-simulation-5d8a0e3f)
- [Part 5: Shiny and Conclusion](https://world.hey.com/catlin/spatial-clustering-love-letters-part-5-5-shiny-and-conclusion-ae9b19e6)

Live shiny app: https://gcatl.in/love_letters/

## Data
Data were obtained from the [2020 Decennial Census P.L. 94-171 Redistricting Data](https://www.census.gov/programs-surveys/decennial-census/about/rdo/summary-files.html) and are in the cens_files folder. The zipped originals (name lookup tables, import scripts, data) are in the files_used folder.

## Structure
All R scripts are in the R folder, with names corresponding to the order they were used in. All R objects (cleaned data, etc.), are in the R_Objects folder.
