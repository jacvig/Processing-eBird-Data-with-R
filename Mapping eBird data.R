# Plotting eBird data on a geographic location
# And using the data to inform a birding trip, including which time of year to go,
# which areas or hotspots record the most species, and which hotspots are best
# for target species. Also, creating a species list for a specific place and time.

# Data were exported from eBird on 2nd March 2026 covering Nepal from 2022 to 2025.

# Install packages
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)


getwd() #See which directory you are currently in
setwd("C:/Users/filepath")
# To set the working directory, copy the pathway into the setwd() function.
# Be sure to use "" and change \ to /

# Import data
data <- read.delim("ebd_NP_smp_relJan-2026.txt", sep = "\t",
                   header = TRUE, quote = "", stringsAsFactors = FALSE,
                   na.strings = c("", " ", NA))

# Specify only desired columns to keep
Nepal <- data |>
  select(SAMPLING.EVENT.IDENTIFIER, COMMON.NAME, SCIENTIFIC.NAME,
         OBSERVATION.COUNT, STATE, COUNTY, LOCALITY, LATITUDE, LONGITUDE,
         Year, Month, Day, TIME.OBSERVATIONS.STARTED, OBSERVATION.TYPE,
         DURATION.MINUTES, EFFORT.DISTANCE.KM, NUMBER.OBSERVERS,
         ALL.SPECIES.REPORTED)


##TRICK## To import only desired columns of dataframe
## Create an object to look at columns
df_header <- read.delim("ebd_NP_smp_relJan-2026.txt", nrows = 1, sep = "\t",
                        header = TRUE, quote = "",
                        stringsAsFactors = FALSE, na.strings = c("", " ", NA))

df_header(names)


## Create an object with those column values you want to keep
df_col <- c("SAMPLING.EVENT.IDENTIFIER", "GROUP.IDENTIFIER", "LAST.EDITED.DATE",
            "CATEGORY", "COMMON.NAME", "SCIENTIFIC.NAME", "OBSERVATION.COUNT",
            "STATE", "COUNTY", "LOCALITY", "LATITUDE", "LONGITUDE", "OBSERVATION.DATE",
            "TIME.OBSERVATIONS.STARTED", "OBSERVATION.TYPE", "DURATION.MINUTES",
            "EFFORT.DISTANCE.KM", "NUMBER.OBSERVERS", "ALL.SPECIES.REPORTED", "APPROVED")

## Create arguments to keep those columns specified in the object
df_header <- names(df_header)
df_header[!df_header %in% df_col] <- "NULL"
df_header[df_header %in% df_col] <- NA

## Read in the df with specified columns
Nepal <- read.delim("ebd_NP_smp_relJan-2026.txt", colClasses = df_header, sep = "\t",
                    header = TRUE, quote = "",
                    stringsAsFactors = FALSE, na.strings = c("", " ", NA))

# Dates
library(lubridate) # To work with dates (e.g. the as.Date function) use the Lubridate package

# Separate the OBSERVATION.DATE column into three separate columns: Year, Month, Day
Nepal <- Nepal |>
  separate(OBSERVATION.DATE, c("Year", "Month", "Day"), "-")  # This is the separator used in the dataframe



# Process the dataset

# Create new column of unique id's to help remove duplicates later
# This will populate a new column with either a Group id or a sampling event id
Nepal <- Nepal |>
  mutate(UNIQUE.ID = ifelse(is.na(GROUP.IDENTIFIER), SAMPLING.EVENT.IDENTIFIER,
                            GROUP.IDENTIFIER))

# Convert the column Count to a numeric vector
Nepal$OBSERVATION.COUNT <- as.numeric(Nepal$OBSERVATION.COUNT) # If you get the error "NAs introduced by coercion", you can ignore.

# Remove incomplete checklists
# This could be optional. Worth considering if looking at presence/absence data
# May wish to keep if e.g. looking at rarer species which may have been recorded incidentally.
Nepal <- Nepal |>
  filter(ALL.SPECIES.REPORTED == "1")

# Remove Observation counts of X (or NA if converted to integers)
# Again, optional if interested in presence only
Nepal <- Nepal |>
  filter(OBSERVATION.COUNT != "X")



# Remove "spuh's" and "slashes" i.e. Species recorded with "sp." or / in name

# First identify to be sure 
Nepal |>
  filter(str_detect(COMMON.NAME, "sp\\."))

# Remove
Nepal <- Nepal |>
  filter(!str_detect(COMMON.NAME, "sp\\."))

# Remove species with / (meaning that a species is recorded as either or)
Nepal <- Nepal |>
  filter(!str_detect(COMMON.NAME, "\\/"))


##TRICK## Easier way to do this
Nepal <- Nepal |>
  filter(CATEGORY %in% c("species", "issf")) # Would have to have kept CATEGORY column



# Remove observation dates before 2022
# I am doing this to have a better understanding of current species localities and
# what I am likely to encounter as opposed to historic data
Nepal <- Nepal |>
  filter(Year %in% 2022:2025)




# Explore the data

# Total number of unique species
length(unique(Nepal$COMMON.NAME))
# 814

# View list of unique species
unique(Nepal$COMMON.NAME)


# Most frequently encountered (recorded in a checklist) species
Nepal |>
  count(COMMON.NAME) |>
  arrange(desc(n))
# House Crow

# Most infrequent species
Nepal |>
  count(COMMON.NAME) |>
  arrange(n)
          

# Total number of checklists for each species
table(Nepal$COMMON.NAME)


# Total number of observations for each species
Nepal|>
  drop_na(OBSERVATION.COUNT) |> # Use if you did not remove X's
  group_by(COMMON.NAME) |>
  summarise(Total = sum(OBSERVATION.COUNT)) |>
  arrange(Total) |>
  View()


# Number of unique species in each COUNTY
Nepal |>
  group_by(COUNTY) |>
  summarise(Total = n_distinct(COMMON.NAME)) |>
  arrange(Total) |>
  View()


# Frequency of species in each COUNTY i.e. the proportion of checklists in which a species has been recorded
# This will help identify which counties you are more likely to find specific species
common_species <- Nepal |>
  group_by(COMMON.NAME, UNIQUE.ID) |>
  slice(1) |>
  ungroup() |>
  group_by(COUNTY) |>
  mutate(lists = n_distinct(UNIQUE.ID)) |>
  ungroup() |>
  group_by(COUNTY, COMMON.NAME) |>
  summarise(freq = n()/max(lists)) |>
  arrange(desc(freq))

write.csv(common_species, "speciesfrequency.csv", row.names = FALSE)


#Average duration of a checklist
Nepal |>
  drop_na(DURATION.MINUTES)  |>
  summarise(Average = mean(DURATION.MINUTES))
# 141.3 minutes


#Average distance of a checklist
Nepal |>
  drop_na(EFFORT.DISTANCE.KM) |>
  summarise(Average = mean(EFFORT.DISTANCE.KM))
# 5.09 km



# Explore which month(s) overall record the most species

Nepal |>
  group_by(Month, COMMON.NAME) |>
  ggplot(mapping = aes(x=Month)) +
  geom_bar()
# December and March 


# Explore which STATE/COUNTY have most records for which month

states <- Nepal |>
  group_by(STATE, Month) |>
  summarise(species = n_distinct(COMMON.NAME)) |>
  ungroup()


ggplot(states, aes(x= Month, y = species)) +
  geom_bar(stat = "identity") +
  facet_wrap(~STATE) +
  labs(title = "Total unique species recorded",
       x = "Month",
       y = "Number of species") +
  theme_bw()

# Bagmati, Gandaki, and Province 1 seem to have the highest records.
# March and April seem to have most number of species recorded


counties <- Nepal |>
  group_by(COUNTY, Month) |>
  summarise(species = n_distinct(COMMON.NAME)) |>
  ungroup()


ggplot(counties, aes(x = Month, y = species)) +
  geom_bar(stat = "identity")+
  facet_wrap(~COUNTY)+
  labs(title = "Total unique species recorded",
       x = "Month",
       y = "Number of species")+
  theme_bw()
# Chitawan, Kaski, Kathmandu, Bardiya, Lalitpur...
# Again, March and April seem to have highest number of species.






# Explore a specific species

# Filter for a specific species
WBTW <- Nepal |>
  filter(COMMON.NAME == "White-browed Tit-Warbler")

HM <- Nepal |>
  filter(COMMON.NAME == "Himalayan Monal")

ST <- Nepal |>
  filter(COMMON.NAME == "Satyr Tragopan")

SB <- Nepal |>
  filter(COMMON.NAME == "Spiny Babbler")

# Counts per month
WBTW |>
  drop_na(OBSERVATION.COUNT) |>
  group_by(Month) |>
  ggplot(mapping = aes(x=Month)) +
  geom_bar()
# appears November and April are best months

# Counts per month for a specific year
WBTW |>
  drop_na((OBSERVATION.COUNT)) |>
  filter(Year == 2025) |>
  ggplot(mapping = aes(x=Month)) +
  geom_bar()


HM |>
  drop_na(OBSERVATION.COUNT) |>
  group_by(Month) |>
  ggplot(mapping = aes(x=Month)) +
  geom_bar()
# April and May

ST |>
  drop_na(OBSERVATION.COUNT) |>
  group_by(Month) |>
  ggplot(mapping = aes(x = Month)) +
  geom_bar()
# April and May


SB |>
  drop_na(OBSERVATION.COUNT) |>
  group_by(Month) |>
  ggplot(mapping = aes(x = Month)) +
  geom_bar()
# April, May, and November



# Top hotspots for a species (for a specific month)
WBTW |>
  filter(Month == "04") |>
  drop_na(OBSERVATION.COUNT) |>
  group_by(LOCALITY) |>
  summarise(Total = sum(OBSERVATION.COUNT)) |>
  arrange(desc(Total))

  # For a specific month
    wbtw_may<- WBTW |>
      filter(Month == "04")
    
  

HM |>
  drop_na(OBSERVATION.COUNT) |>
  group_by(LOCALITY) |>
  summarise(Total = sum(OBSERVATION.COUNT)) |>
  arrange(desc(Total))


ST |>
  drop_na(OBSERVATION.COUNT) |>
  group_by(LOCALITY) |>
  summarise(Total = sum(OBSERVATION.COUNT)) |> 
  arrange(desc(Total))


SB |> 
  drop_na(OBSERVATION.COUNT) |> 
  group_by(LOCALITY) |> 
  summarise(Total = sum(OBSERVATION.COUNT)) |>
  arrange(desc(Total))


# Map the data using r-naturalearth

install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)


np_sf <- ne_countries(scale = "medium", country = "Nepal", returnclass = "sf")


# Add cities/labels 
cities <- ne_download(scale = 50,
                      type = "populated_places_simple",
                      category = "cultural",
                      returnclass = "sf")
# Keep cities that fall inside Nepal
nepal_cities <- st_join(cities, np_sf, join = st_within) |>
  filter(admin == "Nepal")



# Map
ggplot(data = np_sf) +
  geom_sf(fill = "gray90", color = "black") +
  geom_point(data = WBTW, aes(x = LONGITUDE, y = LATITUDE, colour = OBSERVATION.COUNT), size = 1) + # colour based on value of "count"
  scale_color_viridis_c()+ # colour style 
  # facet_wrap(~ Year) + # shows all the years separately
  geom_sf(data = nepal_cities, color = "purple", size = 2) +
  geom_sf_text(data = subset(cities, name == "Kathmandu"), aes(label = name), nudge_y = 0.15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "White-browed Tit Warbler", x = "Longitude", y = "Latitude")





# Map data using Open Street Maps
install.packages("osmdata")
library(osmdata)
library(sf)
library(ggplot2)

# Get Nepal boundary from OpenStreetMap
np_osm <- opq("Nepal") |>
  add_osm_feature(key = "boundary", value = "administrative") |>
  add_osm_feature(key = "admin_level", value = "4")


# OSM reference of admin levels
# admin_level=2: Country (e.g., United Kingdom)
# admin_level=4: Constituent country (e.g., England, Scotland)
# admin_level=6: Metropolitan counties, non-metropolitan counties, or unitary authorities
# admin_level=8: Districts, boroughs, cities, or civil parishes (often used for city boundaries)

np_poly <- osmdata_sf(np_osm)


bb <- getbb("Nepal")


# Get major cities / provincial capitals from OSM
q_cities <- opq(bbox = bb) |>
  add_osm_feature(key = "place", value = c("city", "town"))

cities_osm <- osmdata_sf(q_cities)

osm_cities <- cities_osm$osm_points |>
  filter(!is.na(name))

# Pick major cities to label
major_cities <- osm_cities |>
  filter(int_name %in% c("Kathmandu", "Pokhara", "Biratnagar", "Janakpur",   
                     "Bharatpur", "Lalitpur", "Butwal", "Dharan"))
# Not all cities had a international name in the dataset. I used only those that were available.

# plot
ggplot() +
  geom_sf(data = np_poly$osm_multipolygons, fill = NA, colour = "black", linewidth = 0.5) +
  geom_sf(data = major_cities, color = "red", size = 1.5, alpha = 0.6) +
  geom_sf_text(data = major_cities, aes(label = int_name), nudge_y = 0.05, size = 3) +
  theme_minimal()



# There are a few issues:
# 1. Could not get all cities with rnaturalearth
# 2. Boundaries lines for OSM were too wide
# 3. I therefore combined the cities code for osm with the boundaries code of rne for a better map:

# Final map!

# White-browed Tit Warbler
ggplot(data = np_sf) +
  geom_sf(fill = "gray90", color = "black") +
  geom_point(data = WBTW, aes(x = LONGITUDE, y = LATITUDE, colour = OBSERVATION.COUNT), size = 1) + # colour based on value of "count"
  scale_color_viridis_c()+ # colour style 
  # facet_wrap(~ Year) + # shows each year separately
  facet_wrap (~ Month) + # shows each month
  geom_sf(data = major_cities, color = "purple", size = 1) +
  geom_sf_text(data = major_cities, aes(label = int_name), nudge_y = 0.05, size = 2) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "White-browed Tit Warbler", x = "Longitude", y = "Latitude")

# Best found in Annapurna in April and November



# Satyr Tragopan
ggplot(data = np_sf) +
  geom_sf(fill = "gray90", color = "black") +
  geom_point(data = ST, aes(x = LONGITUDE, y = LATITUDE, colour = OBSERVATION.COUNT), size = 1) + # colour based on value of "count"
  scale_color_viridis_c()+ # colour style 
  # facet_wrap(~ Year) + # shows all the years separately
  facet_wrap (~ Month) + # shows each month
  geom_sf(data = nepal_cities, color = "purple", size = 2) +
  geom_sf_text(data = major_cities, aes(label = int_name), nudge_y = 0.05, size = 3) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Satyr Tragopan", x = "Longitude", y = "Latitude")

# Best found in Pokhara and Langtang in May, Best in Langtang in Apirl


# Himalayan Monal
ggplot(data = np_sf) +
  geom_sf(fill = "gray90", color = "black") +
  geom_point(data = HM, aes(x = LONGITUDE, y = LATITUDE, colour = OBSERVATION.COUNT), size = 1) + # colour based on value of "count"
  scale_color_viridis_c()+ # colour style 
  # facet_wrap(~ Year) + # shows all the years separately
  facet_wrap (~ Month) + # shows each month
  geom_sf(data = nepal_cities, color = "purple", size = 2) +
  geom_sf_text(data = major_cities, aes(label = int_name), nudge_y = 0.05, size = 3) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Himalayan Monal", x = "Longitude", y = "Latitude")

# Best found in Pokhara and Langtang in March, April and May. Best found in Langtang October and November


# Spiny Babbler
ggplot(data = np_sf) +
  geom_sf(fill = "gray90", color = "black") +
  geom_point(data = SB, aes(x= LONGITUDE, y=LATITUDE, colour = OBSERVATION.COUNT), size = 1)+
  scale_color_viridis_c() +
  facet_wrap(~ Month) +
  geom_sf(data = nepal_cities, color = "purple", size = 2) +
  geom_sf_text(data = major_cities, aes(label = int_name), nudge_y = 0.05, size = 3) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Spiny Babbler", x = "Longitude", y = "Latitude")
# Pokhara and Kathmandu




# Create a species list for a specific period of time: April

sp_may <- Nepal |>
  filter(Month == "04")

length(unique(sp_may$COMMON.NAME))
# 729

# View list of Counties
unique(sp_may$COUNTY)


# Choose an area:
# Gandaki (STATE) - Annapurna (or Kaski, Manang, Mustang, Lamjung)
# Nuwakot, Rasuwa, Sindhupalchok - Langtang
# Chitawan

Langtang <- Nepal |>
  filter(Month == "04") |>
  filter(COUNTY %in% c("Rasuwa", "Nuwakot", "Sindhupalchok"))


length(unique(Langtang$COMMON.NAME))
# 330

# Create a species list for Langtang in May of most common to least common species
sp_lang <- Langtang |>
  count(COMMON.NAME) |>
  arrange(desc(n))


Kathmandu <- Nepal |>
  filter(Month == "04") |>
  filter(COUNTY == "Kathmandu")

length(unique(Kathmandu$COMMON.NAME))
# 292 species

Chitawan <- Nepal |>
  filter(Month == "04") |>
  filter(COUNTY == "Chitawan")

length(unique(Chitawan$COMMON.NAME))
# 418 species
