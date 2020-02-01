# Set directory for DC data
DC_directory <- paste(data_directory, 
                        "/DC/",
                        sep = "")

# List of all scooter-related files
DC_scooter_trip_list <- list.files(path = DC_directory,
                                   pattern = "Lime|Bird|Scooters|skip|Spin|Razor|razor|Lyft")

## Check that this covers all scooter files
# DC_scooter_trip_list_b <- list.files(path = DC_directory) # all files in the folder

# setdiff(DC_scooter_trip_list_b, DC_scooter_trip_list) # difference between all files and just scooter files

DC_scooter_data <- DC_scooter_trip_list[1:10] %>%
  {.[!str_detect(., "2019-09_Lime_trips.csv")]} %>%  # this particular record is tab-separated rather than comma-separated
  {paste(DC_directory, ., sep = "")} %>% 
  map_dfr(., 
         ~ read_csv(.,
                    col_types = cols(.default = "c")) %>% # read all the columns as characters for simplicity
           set_names(., tolower(names(.))) %>% # some datasets have all caps columns, others have all lowercase
           mutate(dataset = .x,
                  dataset = str_match(dataset, paste(DC_directory, "(.*?)", "\\.csv", sep = ""))[, 2])) %>% 
  bind_rows(., 
            DC_scooter_trip_list %>%
              {.[str_detect(., "2019-09_Lime_trips.csv")]} %>% # add in the tab-separated dataset
              {paste(DC_directory, ., sep = "")} %>% 
              read_tsv(col_types = cols(.default = "c")) %>% 
              set_names(., tolower(names(.))) %>% 
              mutate(dataset = "2019-09_Lime_trips"))

test <- DC_scooter_data%>% 
  mutate(company = tolower(str_extract(dataset, "Lime|Bird|JUMP|skip|Spin|Razor|razor|Lyft")),
         start_date = str_extract(start_time, "[0-9]{1,2}\\/[0-9]{1,2}\\/[0-9]{1,2}"))
  


