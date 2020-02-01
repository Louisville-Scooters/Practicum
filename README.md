# MUSA 800 Project Template

# Project Name:


# Project Team:

* Yijia Liu
* Xinyi Miao
* Eugene Chong

# File and Object Management:

## Folders

* The `~code` folder is organized by city. Any script in the top level folder should apply to all cities, e.g., `01 - Admin.R`.
* Numbering resets for each folder.
  * The first scripts in both the `Louisville` and `DC` folders start with "01".

## Scripts

* Naming convention: `[City] - [Number] - [What does it do?] ([Open/Closed]).R`
* Number scripts in the order they should be run, e.g., `LV - 01 - Read Data.R` should come before `LV - 02 - Clean Data.R`.
  * Sometimes this won't matter, like two different analysis tasks.
* Separate tasks into separate scripts.
  * E.g., scripts for reading in raw data and for making visualizations should be different files.
* Specify whether a script is `Open` or `Closed` in the file name. 
  * **Open** means it is still being modified / it's OK for anyone to modify it.
  * **Closed** means that it's been "finalized", and you should let everyone know that you are making a change to it.
* Include a summary at the top of each script.  For example, from `LV - 01 - Read Scooter and Base Map Data (Closed).R`:
  ```
  ##########################################################################
  # This script reads in:
  # 1. Louisville base map
  # 2. Scooter service area
  # 3. Scooter rebalance data
  # 4. Scooter Open Data
  #
  # This makes no changes to the raw data.
  ##########################################################################
  ```
* Instead of deleting a script, move it into the `~code/~archive` folder

## Objects

* Include the city at the beginning of each object name, e.g, `LV_rebal_sf`, not `rebal_sf`.
* Save time-intensive objects to `~data/~RData`.
  * For example: the below code to filter the LV rebalance data for the service area takes a long time.
  ```r
  LV_rebal_sf <- st_as_sf(LV_rebal_raw,
                              wkt = "location",
                              crs = 4326) %>%
    st_transform(LV_proj) %>%
    .[LV_SA,]
  ```
  * After we run it, we can save that object to our shared `~Rdata` folder.
  ```r
  LV_rebal_sf_RDS <- file.path(data_directory, "~RData/Louisville/LV_rebal_sf")
  
  saveRDS(LV_rebal_sf,
          file = LV_rebal_sf_RDS)
  ```        


# Use Case:

# Data Wrangling:

# Exploratory Data Analysis:

# Web Visualization:

# Model Development & Validation:

# Markdown Development: