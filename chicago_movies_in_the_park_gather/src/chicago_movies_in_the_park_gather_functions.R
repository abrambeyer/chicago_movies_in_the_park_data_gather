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




#check if necessary packages are installed.  If not, install them.
list.of.packages <- c("RSocrata", "tidyverse","jsonlite","tidygeocoder")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#load necessary libraries
suppressWarnings(suppressMessages(library(RSocrata)))
suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(jsonlite)))
suppressWarnings(suppressMessages(library(tidygeocoder)))


#####################################################

download_chi_park_movie_data <- function(json_df){
  #takes a tibble of years and json objects
  #iterates over the rows, then calls the RSocrata api to pull the data files into a R dataframe.
  #returns list holding a dataset for each year with a new column called 'datayear' to identify which data release it is.
  
  
  #initialize empty list to hold all datasets
  dataset_list <- list()
  
  app_token <- readline('Enter the Socrata API App Token.')[1];
  email <- readline('Enter the Socrata API email address.')[1];
  password <- readline('Enter the Socrata API password.')[1];
  
  #iterate over the year/json tibble
  for (row in 1:nrow(json_df)) {
    #create variables for year and json api object
    year <- json_df[row, "year"]
    json_path  <- json_df[row, "json"]
    
    #request data from RSocrata
    #results to dataframe
    df <- read.socrata(
      json_path,
      app_token = app_token,
      email     = email,
      password  = password
    )
    
    #create new datayear column to identify which file it is.
    df['datayear'] <- year
    #print(dim(df))
    #print(head(df))
    
    #append dataframe to the list container
    dataset_list[[row]] = df
  }
  
  #return the list of dataframes
  return(dataset_list)
}


#####################################################



check_and_rename_col <- function(df,check_col_name,new_col_name){
  #function takes a dataframe as input.  Checks if a column exists in the dataframe.
  #if not, it renames to a new name.  Returns the dataframe.
  
  
  if (quo_name(check_col_name) %in% colnames(df) == TRUE){
    
    df <- df %>% rename(!!sym(new_col_name) := !!sym(check_col_name))
  }
  
  return(df)
}



#####################################################


check_and_add_col <- function(df,check_col_name,new_col_val){
  #function takes a dataframe as input.  Checks to see if a column name exists in the dataframe.
  #if not, it adds a placeholder NA column of that name.  Returns the dataframe.
  
  if (quo_name(check_col_name) %in% colnames(df) == FALSE){
    df <- df %>% add_column(!!quo_name(check_col_name) := !!quo_name(new_col_val)) 
  }
  return(df)
}

#####################################################

create_geocode_address_col <- function(df){
  
  #function takes a dataframe as input.  Very specific to the chicago park district movies in the park datasets.
  #Checks the datayear column.  If not 2019, then splits out the human address json object column to address, city, state,zip.
  #Then creates a new column called geocode_address which is a concatenation of the address, city, state, zip 
  #used to pass to the tidygeocoder geocode function.
  
  #if datayear is 2019, then manually add city, state, zip.   All data should be Chicago parks so just adding 'Chicago', and 
  #'IL', NA placeholder columns.
  
  #check if 2019
  if (head(df,n=1)['datayear'][[1]] != '2019'){
    
    #copy human address column  
    df$location.human_address2 <- df$location.human_address
    
    #split column of json objects  
    df <- df %>% 
      mutate(location.human_address = map(location.human_address, ~ fromJSON(.) %>% as.data.frame())) %>% 
      unnest(location.human_address)
    
    #clean up 2014 and 2015 datasets.
    #2014 & 2015, the 'address' column includes city, state, zip code so no need to concatenate.
    
    if (head(df,n=1)['datayear'][[1]] %in% c('2014','2015')){
      #address already includes city, state, zip.  Just copy it over as geocode_address.
      df = df %>% mutate(geocode_address = address)
      #make community column uppercase to make geocode output later.
      df = df %>% mutate(community = str_to_upper(community))
      
    } else {
      #concatenate address, city, state to create address tidygeocoder can use.
      df = df %>% mutate(geocode_address = paste(address,city,state,sep=', '))
      
    }
    
    
  } else {
    
    #copy human address column  
    df$location.human_address2 <- NA
    
    #2019 does not have human_address column,address, city, state, or zip.
    #use the park_address column to make these columns and concatenate.
    df$address <- df$park_address
    df <- df %>% add_column('city' = 'Chicago',
                            'state' = 'IL',
                            'zip' = NA) 
    
    #concatenate address, city, state to create address tidygeocoder can use.
    df = df %>% mutate(geocode_address = paste(address,city,state,sep=', '))
    
    
  }
  
  return(df)
  
}

#####################################################


check_and_exclude_col <- function(df,check_col_name){
  #function takes a dataframe as input.  Checks to see if a column name exists in the dataframe.
  #if the column exists, remove it from the dataset.
  
  if (quo_name(check_col_name) %in% colnames(df) == TRUE){
    
    df <- df %>% select(-quo_name(check_col_name))
  }
  
  
  return(df)
}

#####################################################


extract_community_column <- function(df){
  #takes a dataframe as input
  #extracts the community from the osm display_name column if community column does not exist in the dataframe
  #returns dataframe
  #2014 & 2015 have a community column already, however 2016-2019 do not.  We need to 
  #use the geocode display name to extract the community name.
  
  if ("community" %in% colnames(df) == FALSE) {
    
    
    df = df %>%
      rowwise() %>%
      mutate(display_name_split2 = str_squish(display_name)) %>%
      #mutate(display_name_split2 = str_replace_all(display_name_split2, " ", "")) %>%
      mutate(display_name_split2 = str_to_upper(display_name_split2)) %>%
      mutate(display_name_split2 = strsplit(display_name_split2, ","))
    
    is_chicago <- function(x) x == 'CHICAGO' | x == ' CHICAGO' | x == ' CHICAGO '
    
    is_township <- function(x) str_detect(x, 'TOWNSHIP') == TRUE | str_detect(x,'TWNSHIP') == TRUE | str_detect(x, ' TOWNSHIP') == TRUE | str_detect(x, ' TOWNSHIP ') == TRUE | str_detect(x, ' TWNSHIP ') == TRUE
    
    df <- df %>% add_column('community' = NA)
    
    
    for (i in 1:nrow(df)){
      
      if (is.na(df$display_name_split2[i]) == TRUE){
        
        df$community[i] <- NA
        
      } else if (df$address[i] == 'NULL'){
        
        df$community[i] <- NA
        
      } else if (rev(df$display_name_split2[i][[1]])[1] != 'UNITED STATES' && rev(df$display_name_split2[i][[1]])[1] != ' UNITED STATES' && rev(df$display_name_split2[i][[1]])[1] != ' UNITED STATES ') {
        
        df$community[i] <- NA
        
      } else{
        
        x <- df$display_name_split2[i][[1]] %>% detect_index(is_chicago,.dir = "backward") 
        
        if (x == 0){
          
          x = df$display_name_split2[i][[1]] %>% detect_index(is_township,.dir = "backward")
        }
        
        df$community[i] <- df$display_name_split2[i][[1]][x-1]
      }
      
    }
    
    
    
  }
  
  return(df)
  
}


#####################################################


clean_and_geocode_movie_datasets <- function(dataset_list) {
  
  #function loops over a list of chicago park district movies in the park datasets as dataframes.
  #Due to different data available each year, we need to clean all the datasets so they can be unioned together.
  #for each year, renames certain columns, adds/removes certain columns, creates calculated fields
  #and geocodes address so all years have the same information.
  
  final_dataset_list = list()
  
  #loop over dataframes in dataset_list
  #not all datasets are the same over the years.
  #We need to do some cleaning in order to union them together in a logical way.
  for (i in 1:length(dataset_list)){
    
    #handle address columns
    # 2014-2018 have a human address json object as address.
    # split this json object out into 3 new columns:  address, city, state, zip
    # Create a new, concatenated column called geocode address for geocoding the community/neighborhood
    
    #list of columns to rename in bulk, including location human address.
    cols_to_rename = list(list("location_1.human_address","location.human_address"),
                          list('moviename','title'),
                          list('location','park'),
                          list('parkname','park'),
                          list('movieclosedcaption','cc'),
                          list('movierating','rating'),
                          list("park_phone","phone"),
                          list("location_1.longitude","location.longitude"),
                          list("location_1.latitude","location.latitude"))
    
    #loop over column names and rename them
    
    for (col in 1:length(cols_to_rename)){
      #loop over list of column names and call check/rename function
      dataset_list[[i]] = check_and_rename_col(dataset_list[[i]],cols_to_rename[[col]][[1]],cols_to_rename[[col]][[2]])
      
    }
    
    
    #add a human_address column so all datasets are consistent.
    #some years do not have a human_address column.  Most do.  Add a NA placeholder for the 
    #few years that dont have it.
    #same thing for park_address.  If a dataset does not have this column,
    #create a placeholder
    cols_to_add = list(list("location.human_address",NA),
                       list("park_address",NA))
    
    for (col in 1:length(cols_to_add)){
      #loop over list of column names and call check/rename function
      dataset_list[[i]] = check_and_add_col(dataset_list[[i]],cols_to_add[[col]][[1]],cols_to_add[[col]][[2]])
      
    }
    
    #create address, city, state, zip columns from human_address column
    #or from park_address column
    
    dataset_list[[i]] = create_geocode_address_col(dataset_list[[i]])
    
    #if 'enddate column is in the dataset, rename to date and 
    #remove startdate, zipcode.  This is a special case.
    if ("enddate" %in% colnames(dataset_list[[i]]) == TRUE) {
      
      dataset_list[[i]] = dataset_list[[i]] %>% rename('date' = 'enddate')
      dataset_list[[i]] = dataset_list[[i]] %>% select(-'startdate')
      dataset_list[[i]] = dataset_list[[i]] %>% select(-'zipcode')
    }
    
    #if date is not in the dataset, add date column placeholder and day placeholder.
    if ("date" %in% colnames(dataset_list[[i]]) == FALSE) {
      
      dataset_list[[i]] = dataset_list[[i]] %>% add_column('date' = NA)
      dataset_list[[i]] = dataset_list[[i]] %>% add_column('day' = NA)
    } 
    
    #create day calculated filed as abbreviated day of the week name.
    if ("day" %in% colnames(dataset_list[[i]]) == FALSE) {
      
      dataset_list[[i]]$day <- weekdays(as.Date(dataset_list[[i]]$date), abbreviate=TRUE)
      
    } 
    
    #vector of column names to exclude, if they exist in the dataset
    cols_to_remove <- c('underwriter','url','url.1','contactname','contactemail','location_notes','zipcode',"eventname")
    
    #loop over column names.  Exclude column name, if exists in the data.
    for (col in 1:length(cols_to_remove)){
      dataset_list[[i]] = check_and_exclude_col(dataset_list[[i]],cols_to_remove[col])
    }
    
    
    dataset_list[[i]] <- dataset_list[[i]] %>% geocode(geocode_address,method='osm',full_results=TRUE)
    
    dataset_list[[i]] <- dataset_list[[i]] %>% select(-c('licence','boundingbox','importance','icon','osm_id'))
    
    ##print(paste('after geocode:',dim(df_copy)))
    #
    ##if dataset does not have a community column, then extract it from the osm display name
    dataset_list[[i]] <- extract_community_column(dataset_list[[i]])
    #
    ##with community column extracted, we no longer need the display_name_split2 column.
    dataset_list[[i]] <- check_and_exclude_col(dataset_list[[i]],"display_name_split2")
    
    #subset columns so all datasets match                                                            
    dataset_list[[i]] <- dataset_list[[i]] %>%
      select('title',
             'park',
             'date',
             'phone',
             'rating',
             'cc',
             "location.latitude",
             "location.longitude",
             #"location.human_address",
             "location.human_address2",
             "datayear",
             "day",
             "park_address",
             "address",
             "city",
             "state",
             "zip",
             "community",
             "geocode_address",
             "lat",
             "long",
             "osm_type",
             "display_name",
             "class",
             "type"
      )
    
    
    final_dataset_list[[i]] = dataset_list[[i]]
    
    
  }  
  return(final_dataset_list)
} 


#####################################################


run_chi_movies_in_park_gather <- function(){
  
  #create container tibble for 
  #all chicago park district movies in the park
  #datasets can be found here:  https://data.cityofchicago.org/browse?q=movies&sortBy=relevance
  my_movie_json_df <- tibble(
    'year' = character(),
    'json' = character(),
  )
  
  
  #add all years and json api objects
  #to the container
  #This is used to iterate over and tag each dataset with the appropriate data file year.
  my_movie_json_df <- my_movie_json_df %>% 
    add_row(year = '2014', json = 'https://data.cityofchicago.org/resource/cyqk-tzjs.json') %>% 
    add_row(year = '2015', json = 'https://data.cityofchicago.org/resource/v2a6-nxhe.json') %>% 
    add_row(year = '2016', json = 'https://data.cityofchicago.org/resource/6t9t-2gbi.json') %>% 
    add_row(year = '2017', json = 'https://data.cityofchicago.org/resource/a7gu-2qz6.json') %>% 
    add_row(year = '2018', json = 'https://data.cityofchicago.org/resource/e2v8-k3us.json') %>% 
    add_row(year = '2019', json = 'https://data.cityofchicago.org/resource/7piw-z6r6.json')
  
  
  movie_dataset_list <- download_chi_park_movie_data(my_movie_json_df)
  
  
  cleaned_dataset_list <- clean_and_geocode_movie_datasets(movie_dataset_list)
  
  
  movies_in_park_output_df <- bind_rows(cleaned_dataset_list)
  
  write_csv(movies_in_park_output_df,'output/chi_movies_in_the_park_2014_2019.csv')    
  print('Done.')
}






