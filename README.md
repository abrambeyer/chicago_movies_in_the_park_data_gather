# chicago_movies_in_the_park_data_gather
This R project gathers the ***[Chicago Data Portal](https://data.cityofchicago.org/)***  *Chicago Park District: Movies in the Parks* dataset for the years 2014-2019.  The conditionally cleans each year so all years have the same features, columns names, geocode information, etc.  Then row_binds (unions) the datasets together and writes a csv file to the output folder.

As of 2021, the *Movies in the Parks* datasets can be found under the [Parks and Recreation](https://data.cityofchicago.org/browse?category=Parks+%26+Recreation) section of the Chicago Data Portal.


# R scripts included in the project

1. ***chicago_movies_in_the_park_gather_functions.R***: Source file loads all necessary R libraries and holds all necessary custom functions for the project.  Located in the src folder.
2. ***gather_chicago_movies_in_the_park_data.R***:  Main script that runs the main run_chi_movies_in_park_gather() function.  Puts it all together.

# Steps taken by the main script function
1.  Load all necessary R libraries
2.  Creates a tibble of data years and Chicago Open Portal Socrata API jsons.  Currently, there are only files for 2014-2019 on the data portal.  You could update this tibble with new data release json objects, and it would gather more data.
3. Iterates over tibble and requests data via API and json API endpoint strings.
4. Clean each file.  All years have different features and column names.  i.e. Rename columns, Add/Remove columns, geocode addresses.
5. Write unioned output as a csv file to output folder.
