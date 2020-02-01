# MUSA 800 Project Template

# Project Name:


# Project Team:

* Yijia Liu
* Xinyi Miao
* Eugene Chong

# File Management:

## Folder Organization

* The `~code` folder is organized by city. Any script in the top level folder should apply to all cities, e.g., `01 - Admin.R`.
* Numbering resets for each folder.
  * The first scripts in both the `Louisville` and `DC` folders start with "01".

## Script Organization

* Naming convention: `[City] - [Number] - [What does it do?] ([Open/Closed]).R`
* Number scripts in the order they should be run. E.g., `LV - 01 - Read Data.R` should come before `LV - 02 - Clean Data.R`.
  * Sometimes this won't matter, like for two different analysis tasks.
* Specify whether a script is `Open` or `Closed` in the file name. 
  * **Open** means it is still being modified / it's OK for anyone to modify it.
  * **Closed** means that it's been "finalized", and you should let everyone know that you are making a change to it.
* Include a summary at the top of each script.  For example:
```
##########################################################################
# This script reads in:
# 1. Louisville base map
# 2. Scooter service area
# 3. Scooter rebalance data
# 4. Scooter Open Data
#
# This makes no changes to the data.
##########################################################################
```

* Naming Convention

# Use Case:

# Data Wrangling:

# Exploratory Data Analysis:

# Web Visualization:

# Model Development & Validation:

# Markdown Development: