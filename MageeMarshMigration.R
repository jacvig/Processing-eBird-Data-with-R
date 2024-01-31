#Analyse eBird data for a specific location to see what species are most likely to be seen at a specific time.
#e.g. Spring migration (May), Magee Marsh, Ohio between 2019-2023 i.e. what is the likelihood I'll see each species across each week in May

# DOWNLOAD DATA
  # Request and download data from eBird
  # Instructions: https://science.ebird.org/en/use-ebird-data/download-ebird-data-products
  # Download eBird Basic Data Set: https://ebird.org/data/download 
  # eBird data requires you to register and then request the data. This may take several days.
  # When the data is ready, it will be emailed to you. 
  # Download the zipped folder and place into your directory. Unzip it. There will be multiple files.



# EBIRD DATA
  # There are two main data files: The EBD or observation data and SED or checklist data labled "sampling".
  # In the EBD, each row corresponds to a single species in a checklist, and includes species-level information.
  # In the SED, each row corresponds to a checklist and includes checklist-level information.
  # They can be joined together using the checklist id. 
  # However, not all analysis requires both datasets.



# INSTALL PACKAGES
  # Due to the large size of eBird datasets, you may need to install the Unix command-line utility AWK.
  # First install Cygwin
  # Then install R packages including the R package auk 

  # if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
  # }
  # remotes::install_github("ebird/ebird-best-practices")

  install.packages("tidyverse")
  library(tidyverse) # Group of packages for data manipulation, exploration, and visualisation. Includes dplyr, ggplot
  library(auk)       # To work with huge eBird datasets
  library(lubridate) # To work with dates

  # Set your working directory
  getwd() # See which directory you are currently in
  setwd("/filepath") #To set the working directory, copy the pathway into the setwd() function. Be sure to use "" and change \ -> /

  # OR set path with auk if AWK is installed in a non-standard location
  # ?auk_set_awk_path
  # auk::auk_set_awk_path("/filepath", overwrite = TRUE)



# IMPORT DATA. Because the EBD file is so large, the auk auk package is required.
  # The auk package also allows you to filter before you import the dataset.

  # Inspect the files
    ebd_top<-read_tsv("ebd_US-OH_201905_202305_smp_relDec-2023.txt", n_max = 5)

  # Filter and import the observation data (EBD)
    auk_ebd("ebd_US-OH_201905_202305_smp_relDec-2023.txt") |> 
    # auk_county("Ottawa") |>                         # I couldn't get this filter to work. So I ran this and filtered out Ottawa afterwards.
    auk_date(c("*-05-01", "*-05-31")) |>              # filter by a specific date range or choose specific dates across all years e.g. only checklists from May.
    auk_protocol(c("Traveling", "Stationary")) |> 
    auk_complete() |>                                 # complete checklists
      auk_filter(file = "ebird_filter.txt", overwrite = TRUE)

    data<- read_ebd("ebird_filter.txt", unique = FALSE, rollup = FALSE)


  # Filter by COUNTY
    Ottawa<- data |> 
      filter(county == "Ottawa")

  # Write this dataset as a file
    write.csv(x = Ottawa, file ="Ottawa_county_Ohio.csv")


  # Import checklist data (SED)
    f_sed <- "ebd_US-OH_201905_202305_smp_relDec-2023_sampling.txt"
    checklists <- read_sampling(f_sed)

  # Filter checklist data
    checklists<- checklists |> 
      filter(county == "Ottawa")


  # Preliminary exploration of the data
    names(Ottawa)   # View column names
    names(checklists)
    head(Ottawa)    # View first six rows
    str(Ottawa)     # View vector types for columns e.g. character, numeric, integer

  # I will only be using the EBD data i.e. the Ottawa data



# PROCESS THE DATA

  # Select the columns you want to keep
    Ottawa <- Ottawa %>% 
      select(common_name, 
          scientific_name,
          observation_count, 
          locality, 
          observation_date,
          time_observations_started,
          duration_minutes,
          effort_distance_km)


  # Convert the column Count to a numeric vector
    Ottawa$observation_count<-as.numeric(Ottawa$observation_count) # If you get the error "NAs introduced by coercion", you can ignore.

  # Check the column vectors
    str(Ottawa) 


  # Filter by specific location(s). 
     # See all localities
     unique(Ottawa$locality)
  
  # I want only checklists from the Magee Marsh area, but there are a lot of localities with various names.
  # I will filter by multiple strings, that is, those localities containing specific words

    Ottawa<- Ottawa %>% 
      filter(grepl("Ottawa NWR|Magee Marsh|Turtle|Black|Strange", locality)) #Be aware that spaces are counted in strings.
    
    # Otherwise, I could have filterd by a single locality
    # Ottawa<- Ottawa |> 
    # filter(locality == "")

  # There are rows with unidentified birds e.g. "finch sp." or "small falcon sp."
  # I will filter out all "sp" strings in the common name column.
    
    Ottawa<- Ottawa |> 
      filter(!grepl("sp.", common_name))

  # Convert Data column to three separate columns for day, month, year
     
    Ottawa <-Ottawa %>% 
      separate(observation_date, c("Year", "Month", "Day"), "-")   # "-" This is the separator used in the dataframe
  
    Ottawa<- Ottawa |> 
      unite(Date,5:7, remove = FALSE, sep = "-") #Use remove = TRUE to remove the separate columns


  # Add a new column that provides the cumulative numerical value for that day of the year.
  # Because I will want to visualise species presence across the month of May.
  # 2020 was a leap year, so I will split the data into two data frames, run the modified code on each data frame then knit them together again.


  # For years 2019, 2021, 2022, 2023
  # Create a data frame for these years
    Unleap<-Ottawa %>% 
      filter(Year %in% c(2019, 2021, 2022, 2023))

  # Create two objects
    days = c(31,28,31,30,31)  # Number of days in each month. I only have May so no need to count the full year.
    cdays = c(0,31,59,90,120)


  # Split the Date and add the new column with values
    Unleap<-Unleap %>% 
      mutate(Date = as.Date(Date),   #The column Date needs to be in the data for this to work.
         Year = year(Date),
         Month = month(Date),
         Daym = day(Date),
         Dayc = day(Date) + cdays[Month])




  # Do the same for 2020. Separate 2020 as its own data frame
    Leap<-Ottawa %>% 
      filter(Year == 2020)

    dayleap = c(31,29,31,30,31)  # February 2020 needs 29 days 
    cdayleap = c(0,32,60,91,121) #Each month thereafter will be +1 days

  # Add columns and values into the data frame
    Leap<- Leap %>% 
    mutate(Date = as.Date(Date),
         Year = year(Date),
         Month = month(Date),
         Daym = day(Date),
         Dayc = day(Date) + cdayleap[Month]) #It doesn't seem to matter that the object dayleap was created.


  # Combine the two into a new data frame
    May <-rbind(Unleap, Leap)

    
    
# EXPLORE DATA

  # Total number of species
    length(unique(May$common_name)) # 269 species
    
  # View list of unique species
    unique(May$common_name)
  
  
    