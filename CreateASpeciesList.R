#Create a species list for eBird checklist hotspots for a specific location.
#e.g. eBird data for Andalucia, Spain from Jan 2016 - Sept 2023. 

# Download  eBird data.
##Log into ebird.org to download the data
##The dataset will be emailed to you. When it is, download the zip file.
##Create a folder on your computer and move the file into this. This folder will be the working directory in R.


# In R Studio Desktop, install and run packages
install.packages("tidyverse")
library(tidyverse)


# Set your working directory
getwd() # See which directory you are currently in
setwd("C:/Users/filepath") #To set the working directory, copy the pathway into the setwd() function. Be sure to use "" and change \ -> /



# Import data 
data <-read.csv("filepath.csv") #eBird files come in Excel, but if your data is in .txt format, use the function:

# data<-read.delim("filename.txt", sep ="\t", 
  #           header = T, quote = "", 
  #          stringsAsFactors =F, na.strings =c(""," ", NA))




# Preliminary exploration of the data

names(data)   # View column names
head(data)    # View first six rows
str(data)     # View vector types for columns e.g. character, numeric, integer
unique(data$COUNTY) #View all Counties listed. unique(data$LOCALITY)




# Process the data

#Filter by COUNTY  
data<- data %>% 
  filter(COUNTY %in% c("Cádiz", "Málaga"))

#Remove X in COUNT column
#View Counts with X
data[data$OBSERVATION.COUNT == "X",]

#Remove rows where Count = X
data<-data[data$OBSERVATION.COUNT != "X",]



#Create a smaller dataset by defining the data you want to analyse
data <- data %>% 
  select(GLOBAL.UNIQUE.IDENTIFIER, COMMON.NAME, SCIENTIFIC.NAME, OBSERVATION.COUNT, COUNTY, LOCALITY, OBSERVATION.DATE,
         TIME.OBSERVATIONS.STARTED, SAMPLING.EVENT.IDENTIFIER, PROTOCOL.TYPE, DURATION.MINUTES, EFFORT.DISTANCE.KM,
         NUMBER.OBSERVERS, ALL.SPECIES.REPORTED, GROUP.IDENTIFIER, TRIP.COMMENTS, SPECIES.COMMENTS)


#Filter by date. First separate Y, M, and D into own columns

library(lubridate)

data<-data %>% 
  separate(OBSERVATION.DATE, c("Year", "Month", "Day"), "-")  

#Filter for all observations in a specific month 
data<-data %>% 
  filter(Month == "09")

#Filter by complete checklists
data<-data %>% 
  filter(ALL.SPECIES.REPORTED == 1)


#Convert the column Count to a numeric vector
data$OBSERVATION.COUNT<-as.numeric(data$OBSERVATION.COUNT) # If you get the error "NAs introduced by coercion", you can ignore.


str(data) #To check it has been converted


#Rename columns
data<-data %>% 
  rename(Duration = DURATION.MINUTES, Distance = EFFORT.DISTANCE.KM, Count = OBSERVATION.COUNT,
         )

names(data) # Check they are renamed


#Remove duplicated observations 
#data %>%
 # distinct(GROUP.IDENTIFIER, .keep_all = TRUE)


#Create and save the smaller data file from the new data frame. Can delete the larger one.
write.csv(data, "filename.csv", row.names = FALSE)



# Create lists

#Create a species list
Species<- data %>% 
  distinct(COMMON.NAME)


#Identify checklist localities
data %>% 
  distinct(LOCALITY)

#Create a species list for a specific checklist locality
Guadalhorce<-data %>% 
  filter(LOCALITY == "Desembocadura del Guadalhorce--Área General")

Guadalhorce<- Guadalhorce %>% 
  distinct(COMMON.NAME, SCIENTIFIC.NAME)

#Create a list of species for an area with multiple checklist localities
Ronda<-data %>% 
  filter(LOCALITY %in% c("Sierra de Grazalema PNat--Llanos de Libar, Área General","Ronda--Pueblo" ))

#Create a species list that shows the species for each locality in the area
Ronda<-Ronda %>%
  distinct(COMMON.NAME, LOCALITY)


#Create a species list that shows which checklist localities each species can be seen
Trip<-data %>% 
  filter(LOCALITY %in% c("Desembocadura del Guadalhorce--Área General",
                         "Casares--Castillo",
                         "Sierra de Grazalema PNat--Llanos de Libar, Área General",
                         "Ronda--Pueblo",
                         "La Janda--Área General",
                         "Mirador del Estrecho",
                         "El Estrecho PNat--Punta Camorro",
                         "El Estrecho PNat--Isla de Las Palomas",
                         "El Estrecho PNat--Los Lances",
                         "Algeciras--Parque María Cristina",
                         "Doñana PNat--Salinas de Bonanza",
                         "Chipiona--Puerto",
                         "Lagunas de Camino Colorado",
                         "La Breña y Marismas de Barbate PNat--Área General",
                         "El Palmar--Campiña",
                         "Los Alcornocales PNat--Vega del Puente")) %>% 
  arrange(LOCALITY)

#Change the order of columns to show which species can be seen at each locality
Locations<- Trip %>% 
  relocate(COMMON.NAME, .after = LOCALITY)


# Save the checklists
write.csv(data, "filename.csv", row.names = FALSE)



# Analyse the data

#Find which year(s) have seen the most species
Guadalhorce %>%
  group_by(Year) %>% 
  summarise(species = n_distinct(COMMON.NAME))
  

#If the data cover multiple months, find which months have the most species
Guadalhorce %>% 
  group_by(Month) %>% 
  summarise(species = n_distinct(COMMON.NAME))

