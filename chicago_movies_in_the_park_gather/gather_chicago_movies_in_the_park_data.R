#####################################################
##AUTHOR: R. Abram Beyer
##PROJECT NAME:  Chicago Park District Movies in Park data gather

## What this file does:

## This file contains all necessary functions to query the Chicago Open Data Portal Socrata API,
## download 2014-2019 Movies in the Park files
## clean, geocode and unify them.  
## write the unioned files to a csv file.



#####################################################


#####################################################
##UPDATE LOG





#####################################################

#load necessary libraries and functions
source('src/chicago_movies_in_the_park_gather_functions.R')

#run the main function.
#1. Creates a tibble of data years and Chicago Open Portal Socrata API jsons
#2. Request data via API and json strings.
#3. Clean each file.  All years have different features and column names.  
#   Need to clean them and then union together.  Rename columns, Add/Remove columns, geocode addresses.
#4. Write output to output folder.
run_chi_movies_in_park_gather()