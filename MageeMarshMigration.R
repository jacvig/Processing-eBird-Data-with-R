# Visualise eBird data for a specific location to predict what species are most likely to be seen at a specific time.
# e.g. Spring migration (May), Magee Marsh, Ohio between 2019-2023 i.e. what is the likelihood I'll see each species across each week in May

# This code is adapted from Chapter 2, eBird Data of:
# Strimas-Mackey, M., W.M. Hochachka, V. Ruiz-Gutierrez, O.J. Robinson, E.T. Miller, 
# T. Auer, S. Kelling, D. Fink, A. Johnston. 2023. Best Practices for Using eBird Data. 
# Version 2.0. https://ebird.github.io/ebird-best-practices/. Cornell Lab of Ornithology, 
# Ithaca, New York. https://doi.org/10.5281/zenodo.3620739
# and
# Bird Count India. 2021. Analysing eBird data using R- Part 1. https://www.youtube.com/watch?v=jBGVy7K7dH8

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
  library(gridExtra) # To plot the histogram
  
  
  # Set your working directory
  getwd() # See which directory you are currently in
  setwd("C:/Users/jacvig/OneDrive - The University of Liverpool/R/Processing-eBird-Data-with-R/data/ebd_US-OH_201905_202305_smp_relDec-2023") #To set the working directory, copy the pathway into the setwd() function. Be sure to use "" and change \ -> /

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
          # use inner_join to keep columns from both datasets.
    # NB. This data frame will be used to visualise the data later. See PRESENCE/ABSENCE ANAYLYSIS
    df <- semi_join(observations, checklists, by = "checklist_id") 

    # write.csv(df, file = "df.csv")
    
    
 
       
#Filter by specific location(s). 
    # See all localities
    unique(observations$locality)
    
    # I want only checklists from the Magee Marsh area, but there are a lot of sub-localities with various names.
    # I will filter by multiple strings, that is, those localities containing specific words
    
    Magee<- observations |>  
      filter(grepl("Maumee Bay|Howard Marsh|Metzger Marsh|Ottawa NWR|Magee Marsh|Turtle|Black|Strange", locality)) #Be aware that spaces are counted in strings.
    
    # Otherwise, I could have filtered by a single locality
      # df<- df |> 
      # filter(locality == "Magee Marsh")
    

      
 # Create Species List

  # Total number of species
    length(unique(Magee$common_name)) # 247
    
  # View list of unique species
    unique(Magee$common_name)
  
  
    # Create a species list
    Species<- Magee |>  
      distinct(common_name)
    
    write.csv(x = Species, file = "MageeMarshSpecies.csv")
       
    

# PROCESS THE DATA
  # Keep only wanted columns
    
  names(Magee)
  
  Magee<- Magee |> 
    select(checklist_id,
             global_unique_identifier,
             common_name,
             taxonomic_order,
             scientific_name,
             observation_count,
             locality,
             latitude,
             longitude,
             observation_date,
             time_observations_started,
             observer_id,
             sampling_event_identifier,
             protocol_type,
             duration_minutes,
             effort_distance_km,
             number_observers,
             all_species_reported )
    
  
    # Convert Data column to three separate columns for day, month, year
    
  Magee <-Magee |>  
    separate(observation_date, c("Year", "Month", "Day"), "-")   # "-" This is the separator used in the dataframe
    
  Magee<- Magee |> 
    unite(observation_date,9:11, remove = FALSE, sep = "-") #Use remove = TRUE to remove the separate columns
    
    
  #Add a new column that provides the cumulative numerical value for that day of the year
  # Because 2020 was a leap year, I will split the data into two data frames, run the modified code on each data frame then knit them together again.
  
  #For years 2021, 2022, 2023
  #Create a data frame for these years
  Unleap<-Magee %>% 
    filter(Year %in% c(2019, 2021, 2022, 2023))
  
  #Create two objects
  days = c(31,28,31,30,31,30,31,31,30,31,30,31)  # Number of days in each month. 
  cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)
  
  
  #Split the Date and add the new column with values
  Unleap<-Unleap %>% 
    mutate(observation_date = as.Date(observation_date),   #The column Date needs to be in the data for this to work.
           Year = year(observation_date),
           Month = month(observation_date),
           Daym = day(observation_date),
           Dayc = day(observation_date) + cdays[Month])
  
  
  
  
  #Do the same for 2020. Separate 2020 as it's own data frame
  Leap<-Magee %>% 
    filter(Year == 2020)
  
  dayleap = c(31,29,31,30,31,30,31,31,30,31,30,31)  # February 2020 needs 29 days 
  cdayleap = c(0,32,60,91,121,152,182,213,244,274,305,335) #Each month thereafter will be +1 days
  
  #Add columns and values into the data frame
  Leap<- Leap %>% 
    mutate(observation_date = as.Date(observation_date),
           Year = year(observation_date),
           Month = month(observation_date),
           Daym = day(observation_date),
           Dayc = day(observation_date) + cdayleap[Month]) #It doesn't seem to matter that the object dayleap was created.
  
  
  
  #Combine the two into a new data frame
  MageeMarsh <-rbind(Unleap, Leap)
    
    
   # Convert the column Count to a numeric vector
    MageeMarsh$observation_count<-as.numeric(MageeMarsh$observation_count) # If you get the error "NAs introduced by coercion", you can ignore.
    
   # Check the column vectors
    str(MageeMarsh)
    
    write.csv(MageeMarsh, file = "Mageemarsh.csv")
    
    
    
# EXPLORE THE DATA    
  # Which years reported had the most species?
      MageeMarsh |> 
        group_by(Year) |>  
        summarise(species = n_distinct(common_name))
      
      # 1 2019      215
      # 2 2020      193
      # 3 2021      224
      # 4 2022      225
      # 5 2023      218
    
        
  # Which species were reported most and which species least? i.e. total number of observations for each species
  # Or, which birds am I most likely to see?
      
      # Find and remove NAs from counts
      which(is.na(MageeMarsh$observation_count), arr.ind=TRUE)
      MageeMarsh<- MageeMarsh |> 
        drop_na(observation_count)
      
    # Total number of each species
    SpeciesList<- MageeMarsh |> 
      group_by(common_name) |>  
      summarise(Total = sum(observation_count)) |>  
      arrange(-Total)
    
    write.csv(SpeciesList, file = "outputs/MageeSpeciesList.csv") 
    
    

       
       
   # Create a species list in taxonomic order. NB eBird uses the Clements taxon
    # Arrange by taxonomic order
    
    Taxon<- MageeMarsh |> 
      select(taxonomic_order,
             scientific_name,
             common_name)
    
    Taxon <- Taxon |> 
      arrange(taxonomic_order) |> 
      distinct(common_name)
   
    
 
    
         
    # Explore all records for a specific species
    MageeMarsh |>  
      filter(common_name == "Kirtland's Warbler") |> 
      View()
    
    MageeMarsh |> 
      filter(common_name == "Blackburnian Warbler") |> 
      View()
    
    # Most common duration of checklist i.e. mean
    MageeMarsh |>  
      drop_na(duration_minutes) |>  
      summarise(Length = median(duration_minutes)) # 90 minutes
    
    # Average duration of a checklist i.e. median
    MageeMarsh |>  
      drop_na(duration_minutes) |>  
      summarise(Average = mean(duration_minutes)) # 113 minutes
    
    # Average distance of a checklist (having removed extreme outliers)
    MageeMarsh |>  
      drop_na(effort_distance_km) |>  
      filter(effort_distance_km < 15) |> 
      summarise(Average = mean(effort_distance_km)) # 4.45 km


    
    # Frequencies of common species, i.e. the percentage a species occurs in the checklists
    
    Mageefq<- MageeMarsh |> 
      # filter(all_species_reported = TRUE) |>             # only complete checklists. Use if auk_complete() was not an original filter
      group_by(common_name, sampling_event_identifier) |>  # group by common_name and duplicate checklists
      slice(1) |>                                          # choose 1 of the duplicates
      ungroup() |> 
      #group_by(locality == "") OR group_by(year == ####)      # further specify location or year for which frequencies will be calculated 
      mutate(lists = n_distinct(sampling_event_identifier)) |> # create a new column with the number of distinct checklists. This will be the number frequencies are divided by (the fraction denominator)
      #ungroup()                                               # use ungroup() if previous grouping occured, e.g. location or year      
      group_by(common_name) |>    #group_by(location, common_name) # group by common name to calculate frequency for each
      summarise(freq = n()/max(lists)) |>                      # number of each species divided by total number of checklists
      arrange(desc(freq))  
      
    #> common_name            freq
    #1 Red-winged Blackbird    0.77483893    # 77% of checklists report this species in May 
    #2 Yellow Warbler          0.69695942
    #3 Canada Goose            0.68384763
    #4 Tree Swallow            0.65728496
    #5 Great Egret             0.63286990
    
    
    write.csv(Mageefq, file = "outputs/SpeciesFrequency_Magee.csv")
    
        
# VISUALISE THE DATA
    # Range of distributions for duration. Use a histogram for time scale data.
    MageeMarsh |> 
      drop_na(duration_minutes) |> 
      ggplot(mapping = aes(x= duration_minutes))+
      geom_histogram()
    
    # Remove extremes i.e. Select checklists less than 4 hours 
    MageeMarsh |> 
      drop_na(duration_minutes) |> 
      filter(duration_minutes < 240) |> 
      ggplot(mapping = aes(x = duration_minutes))+
      geom_histogram()
    
    
   # Range of distribution for distance (and remove extremes)
    MageeMarsh |> 
      drop_na(effort_distance_km) |> 
      filter(effort_distance_km <15) |> 
      ggplot(mapping = aes(x= effort_distance_km))+
      geom_histogram()
    
    

  # Plot a species occurrence across the month to get an idea which day/week it is most common
    
    # create a dataframe with the day of month and species
    KW<-MageeMarsh |>        # KW is so seldom seen, that this plot works well.
      filter(common_name == "Kirtland's Warbler") |> 
      distinct(Daym, common_name)
    # plot
    hist(KW$Daym, breaks = 0:31)
    
    
   # For Blackburnian Warbler 
     BW<- MageeMarsh |> 
      filter(common_name == "Blackburnian Warbler") |> 
      filter(Year== 2019) |> 
      group_by(Daym) |> 
      summarise(count = n_distinct(observation_count)) |> 
      ungroup()
    
    barplot(BW$count,BW$Daym, width = 2, space = NULL)
  
   
  # Magnolia Warbler
    MW<- MageeMarsh |> 
      filter(common_name == "Magnolia Warbler") |> 
      filter(Year== 2019) |> 
      group_by(Daym) |> 
      summarise(count = n_distinct(observation_count)) |> 
      ungroup()
    
    barplot(MW$count,MW$Daym, width = 2, space = NULL)
    
  # Or, for each year
    MW<- MageeMarsh |> 
      filter(common_name == "Magnolia Warbler") |> 
      group_by(Year, Daym) |> 
      summarise(count = n_distinct(observation_count)) |> 
      ungroup()

    ggplot(MW, aes(x = Daym, y = count)) +
      geom_bar(stat = "identity", breaks = 0:31) +    
      facet_wrap(~Year) +
      labs(title = "Number of Magnolia Warbler Observations across May",
          x = "Day of Month",
          y = "Count") +
      theme_bw()
    

   # Black and White Warbler 
    BWW<- MageeMarsh |> 
      filter(common_name == "Black-and-white Warbler") |> 
      group_by(Year, Daym) |> 
      summarise(count = n_distinct(observation_count)) |> 
      ungroup()
    
    ggplot(BWW, aes(x = Daym, y = count)) +
      geom_bar(stat = "identity", breaks = 0:31) +    
      facet_wrap(~Year) +
      labs(title = "Number of Black and White Warbler observations across May",
           x = "Day of Month",
           y = "Count") +
      theme_bw() 
    

    

    
    
    
# PRESENCE/ABSENCE ANAYLYSIS
    
    # Combine and zerofill observation and checklist data. Shows presence/absence for a species
      # NB this only works if both dataframes have been filtered the same.
         zf <- auk_zerofill(df, checklists, collapse = TRUE)   #This removes the common_name, but adds a species_observed variable

         # write.csv(zf, file = "zf.csv")
        

  
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
    zf <- zf |> 
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
    
    
  # Optional filtering
    # Remove outliers such as extremely long duration, distance, or number of observers
    zf <- zf |> 
      filter(protocol_type %in% c("Stationary", "Traveling"),
             effort_hours <= 6,
             effort_distance_km <= 10,
             effort_speed_kmph <= 100,
             number_observers <= 10)
    
   # Select only the columns I want to keep
    names(zf)  
      
    
    Ohiozf <- zf |> 
      select(checklist_id, 
             observer_id, 
             scientific_name,
             observation_count, 
             species_observed,
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
    
    write.csv(Ohiozf, file = "Ohiozf.csv")
    write.csv(zf, file = "zf.csv")



 
# Visualise detections for a specific species e.g. Kirtland's Warbler

Kirtlands<-Ohiozf |> 
  filter(scientific_name == "Setophaga kirtlandii") # Remember, many of these "observations" are absences
    
BlackBurn<- Ohiozf |> 
  filter(scientific_name == "Setophaga fusca")
    
  # Time of day
    # summarize data by hourly bins
    breaks <- seq(0, 24)
    labels <- breaks[-length(breaks)] + diff(breaks) / 2
    checklists_time <- Kirtlands |> 
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
  

    # For KW, checklists are distributed between the normal hours of daylight (i.e. 8.00-19.00) with highest number in the afternoon.
    # Detection of KW seems to be highest late morning and midday. NB, there are very few KW sightings, so this will affect the model.
    
    
  # Checklist duration
    # summarize data by hour long bins
    breaks <- seq(0, 6)
    labels <- breaks[-length(breaks)] + diff(breaks) / 2
    checklists_duration <- Kirtlands |> 
      mutate(duration_bins = cut(effort_hours, 
                                 breaks = breaks, 
                                 labels = labels,
                                 include.lowest = TRUE),
             duration_bins = as.numeric(as.character(duration_bins))) |> 
      group_by(duration_bins) |> 
      summarise(n_checklists = n(),
                n_detected = sum(species_observed),
                det_freq = mean(species_observed))
    
    # histogram
    g_duration_hist <- ggplot(checklists_duration) +
      aes(x = duration_bins, y = n_checklists) +
      geom_segment(aes(xend = duration_bins, y = 0, yend = n_checklists),
                   color = "grey50") +
      geom_point() +
      scale_x_continuous(breaks = breaks) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Checklist duration [hours]",
           y = "# checklists",
           title = "Distribution of checklist durations")
    
    # frequency of detection
    g_duration_freq <- ggplot(checklists_duration |> filter(n_checklists > 100)) +
      aes(x = duration_bins, y = det_freq) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = breaks) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Checklist duration [hours]",
           y = "% checklists with detections",
           title = "Detection frequency")
    
    # combine
    grid.arrange(g_duration_hist, g_duration_freq)
    
    # Majority of checklists are under 1 hour, but longer searches have a higher chance of detecting KW.
 
       
  # # summarize data by 1 km bins
    breaks <- seq(0, 10)
    labels <- breaks[-length(breaks)] + diff(breaks) / 2
    checklists_dist <- Kirtlands |> 
      mutate(dist_bins = cut(effort_distance_km, 
                             breaks = breaks, 
                             labels = labels,
                             include.lowest = TRUE),
             dist_bins = as.numeric(as.character(dist_bins))) |> 
      group_by(dist_bins) |> 
      summarise(n_checklists = n(),
                n_detected = sum(species_observed),
                det_freq = mean(species_observed))
    
    # histogram
    g_dist_hist <- ggplot(checklists_dist) +
      aes(x = dist_bins, y = n_checklists) +
      geom_segment(aes(xend = dist_bins, y = 0, yend = n_checklists),
                   color = "grey50") +
      geom_point() +
      scale_x_continuous(breaks = breaks) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "Distance travelled [km]",
           y = "# checklists",
           title = "Distribution of distance travelled")
    
    # frequency of detection
    g_dist_freq <- ggplot(checklists_dist |> filter(n_checklists > 100)) +
      aes(x = dist_bins, y = det_freq) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = breaks) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Distance travelled [km]",
           y = "% checklists with detections",
           title = "Detection frequency")
    
    # combine
    grid.arrange(g_dist_hist, g_dist_freq)

    # Majority of observations are from checklists between 2-4 km. 
    
  
  #Number of observers  
  # summarize data
    breaks <- seq(0, 10)
    labels <- seq(1, 10)
    checklists_obs <- Kirtlands |> 
      mutate(obs_bins = cut(number_observers, 
                            breaks = breaks, 
                            label = labels,
                            include.lowest = TRUE),
             obs_bins = as.numeric(as.character(obs_bins))) |> 
      group_by(obs_bins) |> 
      summarise(n_checklists = n(),
                n_detected = sum(species_observed),
                det_freq = mean(species_observed))
    
    # histogram
    g_obs_hist <- ggplot(checklists_obs) +
      aes(x = obs_bins, y = n_checklists) +
      geom_segment(aes(xend = obs_bins, y = 0, yend = n_checklists),
                   color = "grey50") +
      geom_point() +
      scale_x_continuous(breaks = breaks) +
      scale_y_continuous(labels = scales::comma) +
      labs(x = "# observers",
           y = "# checklists",
           title = "Distribution of the number of observers")
    
    # frequency of detection
    g_obs_freq <- ggplot(checklists_obs |> filter(n_checklists > 100)) +
      aes(x = obs_bins, y = det_freq) +
      geom_line() +
      geom_point() +
      scale_x_continuous(breaks = breaks) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "# observers",
           y = "% checklists with detections",
           title = "Detection frequency")
    
    # combine
    grid.arrange(g_obs_hist, g_obs_freq)

    # The majority of checklists have 1-2 observers, but detection frequency increases 
    # with more observers.