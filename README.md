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
* Number scripts in the order they should be run, e.g., `LV - 01 - Read Scooter Data.R` should come before `LV - 02 - Clean Scooter Data.R`.
  * Sometimes this won't matter, like two different analysis tasks.
  * If you're working on a new task that's very different, keep some cushion between the numbers. If there's an `LV - 01 - Read Scooter Data.R` and you're about to create a file for census data, call it `LV - 20 - Read Census Data.R`.
* Separate tasks into separate scripts.
  * E.g., scripts for reading in raw data and for making visualizations should be different files.
  * When in doubt, make a new script. Easier to combine several small scripts than separate one big one.
* Specify whether a script is `Open` or `Closed` in the file name. 
  * **Open** means it is still being modified / it's OK for anyone to modify it.
  * **Closed** means that it's been "finalized". If you need to make a change to it, let everyone know on Slack.
* Include a summary at the top of each script.  For example, from `LV - 02 - Clean Scooter Data (Closed).R`:
  ```
  ##########################################################################
  # This script is for cleaning the LV scooter data
  # It:
  # 1. Names a projection to use for LV
  # 2. Re-projects the LV base map and service area in the new projection
  # 3. Re-projects the rebal and open scooter data.
  # 4. Filters out any rebal scooter data outside the service area
  # 5. Defines a function for creating LV scooter origin and destination sf objects, 
  #    which can be linked via the 'TripID' column to the rest of the dataset ('LV_open_raw')
  #
  # This script exports the following data:
  # 1. LV_rebal_sf
  ##########################################################################
  ```
* Instead of deleting a script, move it into the `~code/~archive` folder

## Objects

* Include the city at the beginning of each object name, e.g, `LV_rebal_sf`, not `rebal_sf`.
* Save time-intensive objects to `~data/~RData`.
  * While workspace images are helpful, other people cannot work on that object without re-running all the necessary code.
  * **Example:** the below code from `LV - 02` filters the LV rebalance data for the service area. It takes a long time.
  ```r
  LV_rebal_sf <- st_as_sf(LV_rebal_raw,
                              wkt = "location",
                              crs = 4326) %>%
    st_transform(LV_proj) %>%
    .[LV_SA,]
  ```
  * After we run it, we can save that object to our shared `~data/~RData` folder.
  ```r
  LV_rebal_sf_RDS <- file.path(data_directory, 
                               # Below: 'Louisville' is the subfolder. 
                               # 'LV_rebal_sf' is the object you're saving
                               "~RData/Louisville/LV_rebal_sf") 
  
  saveRDS(LV_rebal_sf,
          file = LV_rebal_sf_RDS)
  ```
  * Next time, someone can just read in the .rds file
  ```r
  LV_rebal_sf <- readRDS(LV_rebal_sf_RDS)
  ```
  * Here's how the code looks all together. If you need to change `LV_rebal_sf`, you can un-comment the code, and save a new version of the object
  ```r
  ### Make rebalance sf object ----
  # Make the object with the code below ('ctrl + shift + c' to un-comment multiple lines at once)
  
  # LV_rebal_sf <- st_as_sf(LV_rebal_raw,
  #                             wkt = "location",
  #                             crs = 4326) %>% 
  #   st_transform(LV_proj) %>% 
  #   .[LV_SA,] # filter out any trips outside the service area
  
  LV_rebal_sf_RDS <- file.path(data_directory, "~RData/Louisville/LV_rebal_sf")
  # 
  # saveRDS(LV_rebal_sf,
  #         file = LV_rebal_sf_RDS)
  
  # Read the saved object with the code below
  LV_rebal_sf <- readRDS(LV_rebal_sf_RDS)
  ```

# Use Case:

# Data Wrangling:

# Exploratory Data Analysis:

# Web Visualization:

# Model Development & Validation:

# Markdown Development: