#Visualise eBird data for a specific location to predict what species are most likely to be seen at a specific time.
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
  library(sf)
  
  
  # Set your working directory
  getwd() # See which directory you are currently in
  setwd("filepath") #To set the working directory, copy the pathway into the setwd() function. Be sure to use "" and change \ -> /

  # OR set path with auk if AWK is installed in a non-standard location
  # auk::auk_set_awk_path("/filepath", overwrite = TRUE)



# IMPORT DATA. Because the EBD file is so large, the auk auk package is required.
  # There are two ways to approach importing the EBD data:
  # A. filter before importing. This is useful if the data set is huge.
  # B. import and then filter. Can be useful to explore the data before deciding on filters.
  # NB. If you intend to combine the EBD and SED data, you need to filter in the exact same way.
 
  # Inspect the files
  ebd_top<-read_tsv("ebd_US-OH_201905_202305_smp_relDec-2023.txt", n_max = 5)
  
  # A. Filter before importing the observation data (EBD)
  auk_ebd("ebd_US-OH_201905_202305_smp_relDec-2023.txt") |>   # use auk_sampling for the SED file
    # auk_county("Ottawa") |>                         # I couldn't get this filter to work. So I ran this afterwards.
    auk_date(c("*-05-01", "*-05-31")) |>              # filter by a specific date range or choose specific dates across all years e.g. only checklists from May.
    auk_protocol(c("Traveling", "Stationary")) |> 
    auk_complete() |>                                 # only complete checklists
    auk_filter(file = "ebird_filter.txt", overwrite = TRUE) # can write it directly to a file
  
  data<- read_ebd("ebird_filter.txt", unique = FALSE, rollup = FALSE) # to specify if shared checklists and taxonomic levels will not be collapsed.

  
  # Filter EBD by location
  data<- data |> 
    filter(county == "Ottawa")
  
  # If you chose to not collapse taxonomy, you will have unidentified species and subspecies in the data.
  # e.g.there are rows with unidentified birds e.g. "finch sp." or "small falcon sp."
  # You can filter out unidentified or subspecies as required. 
  
  unique(Magee$common_name)
  
  data<- data |> 
    filter(!grepl("sp.", common_name))  # e.g. filter out all "sp" strings in the common name column.
  
    # Write this dataset as a file
  write.csv(data, file ="Ottawa_county_Ohio.csv")
  
        # To read object in
          Ottawa<- read.csv(file ="Ottawa_county_Ohio.csv")
  
          
          
  # B. Import data and then filter.

  # Import checklist data (SED)
    f_sed <- "ebd_US-OH_201905_202305_smp_relDec-2023_sampling.txt"
    checklists <- read_sampling(f_sed) # This automatically collapses shared checklists so you only get unique checklists
    
      glimpse(checklists)

      
  # Import observation data (EBD)
    # This may take several hours to import depending on the size.
      f_ebd <- "ebd_US-OH_201905_202305_smp_relDec-2023.txt"
      observations <- read_ebd(f_ebd)
     
       glimpse(observations)
      
  
  # filter checklist data
    checklists <- checklists |> 
      filter(all_species_reported,
             between(year(observation_date), 2019, 2023), # can specify year range, specific month, and location
             month(observation_date) == 5,
             county == "Ottawa")
 
  # filter the observation data
    observations <- observations |> 
      filter(all_species_reported,
             between(year(observation_date), 2019, 2023),
             month(observation_date) == 5,
             county == "Ottawa")   
    
          write.csv(checklists, file = "checklists.csv")   # It can be best practice to write large data objects as csv files to read-in for each new session so there is no need to save or run them again. 
          write.csv(observations, file = "observations.csv")
   
        # To read in
          checklists<- read.csv(file = "checklists.csv")   
          observations<- read.csv(file = "observations.csv") 
      
     

# Combine observation and checklist data by removing observations without matching checklists
    # use semi_join to keep only columns from the observations dataset.
    # use inner_join to keep columns from both datasets. I have used this.
    df <- semi_join(observations, checklists, by = "checklist_id") 
    
    write.csv(df, file = "df.csv")
    
      # Optional. Combine and zerofill observation and checklist data. Shows presence/absence for a species
      # NB this only works if both dataframes have been filtered the same.
         zf <- auk_zerofill(df, checklists, collapse = TRUE)
 
         
         write.csv(zf, file = "zf.csv")
         zf<- read.csv(file = "zf.csv")

  
  # Process and further filter the data

    # To make the data more useful for modeling:
    # convert time to a decimal value between 0 and 24
    # change the distance traveled to 0 for stationary checklists, i.e. replace NA with 0
    # convert x counts to NA
    # add speed column
    # add day of the year
    
    
    # first, create a function to convert time observation to hours since midnight
    time_to_decimal <- function(x) {
      x <- hms(x, quiet = TRUE)
      hour(x) + minute(x) / 60 + second(x) / 3600
    }
    
    # clean up variables
    observations <- observations |> 
      mutate(
        # convert count to integer and X to NA
        # ignore the warning "NAs introduced by coercion"
        observation_count = as.integer(observation_count),
        # effort_distance_km to 0 for stationary counts
        effort_distance_km = if_else(protocol_type == "Stationary", 
                                     0, effort_distance_km),
        # convert duration to hours
        effort_hours = duration_minutes / 60,
        # speed km/h
        effort_speed_kmph = effort_distance_km / effort_hours,
        # convert time to decimal hours since midnight
        hours_of_day = time_to_decimal(time_observations_started),
        # split date into year and day of year
        year = year(observation_date),
        day_of_year = yday(observation_date)
      )
    
    
  # Optional filtering (I have not used this)
    # Remove outliers such as extremely long duration, distance, or number of observers
    df <- df |> 
      filter(protocol_type %in% c("Stationary", "Traveling"),
             effort_hours <= 6,
             effort_distance_km <= 10,
             effort_speed_kmph <= 100,
             number_observers <= 10)
    
    
    # Filter by specific location(s). 
    # See all localities
    unique(observations$locality)
    
    # I want only checklists from the Magee Marsh area, but there are a lot of sub-localities with various names.
    # I will filter by multiple strings, that is, those localities containing specific words
    
    df<- df |>  
      filter(grepl("Maumee Bay|Howard Marsh|Metzger Marsh|Ottawa NWR|Magee Marsh|Turtle|Black|Strange", locality.x)) #Be aware that spaces are counted in strings.
    
    # Otherwise, I could have filterd by a single locality
    # df<- df |> 
    # filter(locality == "Magee Marsh")
    
    
    
  # Select only the columns I want to keep
    names(Magee)  
      
    
    Magee <- Magee |> 
      select(checklist_id, 
             observer_id, 
             scientific_name,
             common_name,
             observation_count, 
             state_code, 
             county,
             locality, 
             latitude, 
             longitude,
             protocol_type, 
             all_species_reported,
             observation_date, 
             year, 
             day_of_year,
             hours_of_day, 
             effort_hours, 
             effort_distance_km, 
             effort_speed_kmph,
             number_observers)
    
    write.csv(Magee, file = "Magee.csv")
    
  
    
    
        # Miscellaneous processing
          # Convert the column Count to a numeric vector
            Ottawa$observation_count<-as.numeric(Ottawa$observation_count) # If you get the error "NAs introduced by coercion", you can ignore.

        # Check the column vectors
          str(Ottawa) 

        # Remove NAs from counts
          Ottawa<- Ottawa |> 
          drop_na(observation_count)
 
  

    
    
  # Time of day
    # summarize data by hourly bins
    breaks <- seq(0, 24)
    labels <- breaks[-length(breaks)] + diff(breaks) / 2
    checklists_time <- observations |> 
      #select(common_name = "Kirtland's Warbler") 
      mutate(hour_bins = cut(hours_of_day, 
                             breaks = breaks, 
                             labels = labels,
                             include.lowest = TRUE),
             hour_bins = as.numeric(as.character(hour_bins))) |> 
      group_by(hour_bins) |> 
      summarise(n_checklists = n(),
                n_detected = sum(species_observed),
                det_freq = mean(species_observed))
    
    # histogram
    g_tod_hist <- ggplot(checklists_time) +
      aes(x = hour_bins, y = n_checklists) +
      geom_segment(aes(xend = hour_bins, y = 0, yend = n_checklists),
                   color = "grey50") +
      geom_point() +
      scale_x_continuous(breaks = seq(0, 24, by = 3), limits = c(0, 24)) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Hours since midnight",
           y = "# checklists",
           title = "Distribution of observation start times")
    
    # frequency of detection
    g_tod_freq <- ggplot(checklists_time |> filter(n_checklists > 100)) +
      aes(x = hour_bins, y = det_freq) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = seq(0, 24, by = 3), limits = c(0, 24)) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Hours since midnight",
           y = "% checklists with detections",
           title = "Detection frequency")
    
    # combine
    grid.arrange(g_tod_hist, g_tod_freq)
    
    
    
    
    
    
    
    
    # Preliminary exploration of the data
    names(Ottawa)   # View column names
    names(checklists)
    head(Ottawa)    # View first six rows
    str(Ottawa)     # View vector types for columns e.g. character, numeric, integer
  
  
  # Convert Data column to three separate columns for day, month, year
     
    Ottawa <-Ottawa |>  
      separate(observation_date, c("Year", "Month", "Day"), "-")   # "-" This is the separator used in the dataframe
  
    Ottawa<- Ottawa |> 
      unite(Date,5:7, remove = FALSE, sep = "-") #Use remove = TRUE to remove the separate columns
    
    
    
    
  # Add a new column that provides the cumulative numerical value for that day of the year.
  # Because I will want to visualise species presence across the month of May.
  # 2020 was a leap year, so I will split the data into two data frames, run the modified code on each data frame then knit them together again.


  # For years 2019, 2021, 2022, 2023
  # Create a data frame for these years
    Unleap<-Ottawa |>  
      filter(Year %in% c(2019, 2021, 2022, 2023))

  # Create two objects
    days = c(31,28,31,30,31)  # Number of days in each month. I only have May so no need to count the full year.
    cdays = c(0,31,59,90,120)


  # Split the Date and add the new column with values
    Unleap<-Unleap |>  
      mutate(Date = as.Date(Date),   #The column Date needs to be in the data for this to work.
         Year = year(Date),
         Month = month(Date),
         Daym = day(Date),
         Dayc = day(Date) + cdays[Month])




  # Do the same for 2020. Separate 2020 as its own data frame
    Leap<-Ottawa |>  
      filter(Year == 2020)

    dayleap = c(31,29,31,30,31)  # February 2020 needs 29 days 
    cdayleap = c(0,32,60,91,121) #Each month thereafter will be +1 days

  # Add columns and values into the data frame
    Leap<- Leap |>  
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
  
  
    # Create a species list
    Species<- May |>  
      distinct(common_name)

    write.csv(x = Species, file = "MageeMarshSpecies.csv")
    


    
# ANALYSE THE DATA
    
    # Which years reported had the most species?
    May |> 
      group_by(Year) |>  
      summarise(species = n_distinct(common_name))
    
     # 1  2019     238
     # 2  2020     217
     # 3  2021     238
     # 4  2022     243
     # 5  2023     232
    
    # Which species were reported most and which species least? i.e. total number of observations for each species
    # Or, which birds am I most likely to see?
    Observations <-May |> 
      group_by(common_name) |>  
      summarise(Total = sum(observation_count)) |>  
      arrange(-Total)
  
    write.csv(Observations, file = "MageeMarshObservations.csv")
    
    
    # Explore all records for a specific bird
    May |>  
      filter(common_name == "Kirtland's Warbler") %>% 
      View()
    
    # Most common duration of checklist i.e. mean
    May |>  
      drop_na(duration_minutes) |>  
      summarise(Length = median(duration_minutes)) # 81 minutes
    
    # Average duration of a checklist i.e. median
    May |>  
      drop_na(duration_minutes) |>  
      summarise(Average = mean(duration_minutes)) # 102 minutes
    
    # See the range of distributions. Use a histogram for time scale data.
    May |> 
      drop_na(duration_minutes) |> 
      ggplot(mapping = aes(x= duration_minutes))+
      geom_histogram()
    
    # Remove extremes i.e. Select checklists less than 4 hours 
    May |> 
      drop_na(duration_minutes) |> 
      filter(duration_minutes < 240) |> 
      ggplot(mapping = aes(x = duration_minutes))+
      geom_histogram()
    
    
    # Visualise correlation between duration and species count to estimate optimal time spent in the field.  
    checklists |> 
      drop_na(duration_minutes) |> 
      filter(duration_minutes < 440) |>  # Specify boundaries to remove outliers
      ggplot(mapping = aes(x=duration_minutes, y=common_name))+
      geom_point(size = 2, alpha = .5)+         
      geom_smooth()+
      theme_bw()+
      labs(title = "Species over time")     # The majority of checklists see 
    
    
    
    # Average distance of a checklist (having removed extreme outliers)
    May |>  
      drop_na(effort_distance_km) |>  
      filter(effort_distance_km < 15) |> 
      summarise(Average = mean(effort_distance_km)) # 3.84 km
    
    # Visualise the range (and remove extremes)
    May |> 
      drop_na(effort_distance_km) |> 
      filter(effort_distance_km <15) |> 
      ggplot(mapping = aes(x= effort_distance_km))+
      geom_histogram()
    
    
    
    # Which time of day has the highest number of species?
    # Or, when will I likely see the most species?
    
   May |> 
   filter(Year == "2022") |> 
     filter(duration_minutes <360) |> 
     filter(effort_distance_km < 15) |> 
   group_by(common_name) |> 
   ggplot(mapping = aes(x = time_observations_started))+
   geom_bar()
  # This may not be entirely useful as some checklists are many hours. Additionally, I would want to group the times incrimentally.