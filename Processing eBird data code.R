#Cleaning, analysing, and visualising your personal eBird data with R.

#1. Download your eBird data.
##Log into ebird.org and click "My eBird". In the left-hand menu, click "Download my data" and then "Submit"
##The dataset will be emailed to you. When it is, download the zip file.
##Create a folder on your computer and move the file into this. This folder will be the working directory in R.

#2. Open R Studio Desktop
##If you do not have RStudio Desktop, you can download the free version from https://support--rstudio-com.netlify.app/products/rstudio/download/

#3. In R Studio Desktop, install and run packages
install.packages("tidyverse")
library(tidyverse)


#4. Set your working directory
getwd() #See which directory you are currently in 
setwd("C:/Users/filepath") #To set the working directory, copy the pathway into the setwd() function. Be sure to use "" and change \ to /

#5.Import data 
data <-read.csv("data/MyeBirdData.csv") 
#eBird files come in Excel, but if your data is in .txt format, use the function:
#read.delim("file.txt", sep ="\t", 
#             header = T, quote = "", 
#             stringsAsFactors =F, na.strings =c(""," ", NA))


#6. Preliminary exploration of the data
names(data)   # View column names
head(data)    # View first six rows
str(data)     # View vector types for columns e.g. character, numeric, integer

#Filter by a location
data %>% 
  filter(Location == "Highfield Country Park LNR")

#Filter by multiple locations
data %>% 
  filter(Location %in% c("Sale Water Park", "Audenshaw Reservoirs"))

#Filter to exclude a location
data %>% 
  filter(County != "Manchester") 

#Filter to remove multiple locations 
data %>% 
  filter(!State.Province %in% c("SE-BD", "US-MI", "US-CO", "US-WI"))

#Filter by complete checklists
data %>% 
  filter(All.Obs.Reported == 1)



#7. Create a smaller dataset by defining the data you want to analyse
Highfield <- data %>% 
  filter(Location == "Highfield Country Park LNR") %>% 
  select(Submission.ID, Common.Name, Scientific.Name, Count, Date, Time, Duration..Min., Distance.Traveled..km.)




#8. Process the data

#Convert the column Count to a numeric vector
Highfield$Count<-as.numeric(Highfield$Count) # If you get the error "NAs introduced by coercion", you can ignore.

#Check the column vectors
str(Highfield) 


#Remove NA values and X counts (Optional)
# Find rows with NA
Highfield[!complete.cases(Highfield),] # This will show which rows and columns contain NA before you remove them.

#Remove rows with NA
Highfield <- na.omit(Highfield)

#View Counts with X
Highfield[Highfield$Count == "X",]

#Remove rows where Count = X
Highfield<-Highfield[Highfield$Count != "X",]


#Remove rows with unidentified species. These are the records where only the genus is indicated.  
# This can be done by indicating row number. Identify by looking at the data
test<-Highfield %>% 
  slice(-c(6, 1079, 1080))

#Remove rows by a specific value(s) in a column
Highfield<- subset(Highfield, !(Common.Name %in% c("goose sp.", "Willow Warbler/Common Chiffchaff")))


#Rename columns
Highfield<-Highfield %>% 
  rename(Duration = Duration..Min., Distance = Distance.Traveled..km.)

# Check they are renamed
names(Highfield) 





#Dates
library(lubridate)     #To work with dates (e.g. the as.Date function) use the Lubridate package

test<-Highfield  # Create a new data frame to test 

#Separate the date column into three separate columns: Year, Month, Day
test <-Highfield %>% 
  separate(Date, c("Year", "Month", "Day"), "-")  # This is the seperator used in the dataframe

#Recombine the columns to keep the Date column
test<-test %>% 
  unite(Date,5:7, remove = FALSE, sep = "-") #Use remove = TRUE to remove the separate columns



#Add a new column that provides the cumulative numerical value for that day of the year
# Because 2020 was a leap year, I will split the data into two data frames, run the modified code on each data frame then knit them together again.


#For years 2021, 2022, 2023
#Create a data frame for these years
Unleap<-test %>% 
  filter(Year %in% c(2021, 2022, 2023))

#Create two objects
days = c(31,28,31,30,31,30,31,31,30,31,30,31)  # Number of days in each month. 
cdays = c(0,31,59,90,120,151,181,212,243,273,304,334)


#Split the Date and add the new column with values
Unleap<-Unleap %>% 
  mutate(Date = as.Date(Date),   #The column Date needs to be in the data for this to work.
         Year = year(Date),
         Month = month(Date),
         Daym = day(Date),
         Dayc = day(Date) + cdays[Month])




#Do the same for 2020. Separate 2020 as it's own data frame
Leap<-test %>% 
  filter(Year == 2020)

dayleap = c(31,29,31,30,31,30,31,31,30,31,30,31)  # February 2020 needs 29 days 
cdayleap = c(0,32,60,91,121,152,182,213,244,274,305,335) #Each month thereafter will be +1 days

#Add columns and values into the data frame
Leap<- Leap %>% 
  mutate(Date = as.Date(Date),
         Year = year(Date),
         Month = month(Date),
         Daym = day(Date),
         Dayc = day(Date) + cdayleap[Month]) #It doesn't seem to matter that the object dayleap was created.



#Combine the two into a new data frame
HighfieldData <-rbind(Unleap, Leap)






#9. Explore the data

#Total number of species
length(unique(HighfieldData$Common.Name))

#View list of unique species
unique(HighfieldData$Common.Name)

#Total number of checklists
length(unique(HighfieldData$Submission.ID)) # If the file has the column sampling.event.identifier, use this

#Total number of checklists for each species
table(HighfieldData$Common.Name)


#Total number of observations for each species
HighfieldData %>% 
  drop_na(Count) %>% 
  group_by(Common.Name) %>% 
  summarise(Total = sum(Count)) %>% 
  arrange(Total) %>% 
  View()


#Average duration of a checklist
HighfieldData %>% 
  drop_na(Duration) %>% 
  summarise(Average = mean(Duration)) #73.3 minutes


#Average distance of a checklist
HighfieldData %>% 
  drop_na(Distance) %>% 
  summarise(Average = mean(Distance)) #1.69 km



#Ranges: low, average, and high counts for each species
HighfieldData %>% 
  drop_na(Count) %>% 
  group_by(Common.Name) %>% 
  summarise(min = min(Count),
            average = mean(Count),
            max = max(Count)) %>% 
  View()



#Select checklists between two time durations (or distances)
HighfieldData %>% 
  drop_na(Duration) %>%  #Use drop.na() if you have not removed NA from this column
  filter(Duration > 30) %>% 
  filter(Duration < 120) %>% 
  arrange(Duration)


#Total and average number of birds per checklist
HighfieldData %>% 
  drop_na(Count) %>% 
  group_by(Submission.ID) %>% 
  summarise(Min = min(Count),
            Average = mean(Count),
            Max = max(Count),      # This gives max number of a single bird
            Total = sum(Count))


#Total, min, max, and average number of birds per year
HighfieldData %>% 
  drop_na(Count) %>% 
  group_by(Year) %>% 
  summarise(Min = min(Count),
            Average = mean(Count),
            Max = max(Count),
            Total = sum(Count))


#Total, min, max, and average for each bird
HighfieldData %>% 
  drop_na(Count) %>% 
  group_by(Common.Name) %>% 
  summarise(Min = min(Count),
            Average = mean(Count),
            Max = max(Count),
            Total = sum(Count))


#Total, min, max, and average for a specific bird
HighfieldData %>% 
  drop_na(Count) %>% 
  filter(Common.Name == "Redwing") %>% 
  summarise(Min = min(Count),
            Average = mean(Count),
            Max = max(Count),
            Total = sum(Count))

#To look at all records of a specific bird
HighfieldData %>% 
  drop_na(Count) %>% 
  filter(Common.Name == "Redwing") %>% 
  View()



#High counts for each species. 
HighfieldData %>% 
  drop_na(Count) %>% 
  select(Common.Name, Year, Count) %>%   #specify which columns to include in the final table
  group_by(Common.Name) %>% 
  slice(which.max(Count)) %>%   #slice pulls out the max value of each group.
  View()


#High counts of each species for a specific year
HighfieldData %>% 
  drop_na(Count) %>% 
  filter(Year == 2022) %>% 
  group_by(Common.Name) %>% 
  summarise(HighCount = max(Count)) %>% 
  View()




#10. Data Analysis and visualisation


#Look at the correlation between duration and count. Use a Historgram to visualise this
HighfieldData %>% 
  drop_na(Count) %>% 
  drop_na(Duration) %>%  
  ggplot(mapping = aes(x=Duration))+        # Duration along the X-axis while Count is on the Y-axis.
  geom_histogram()+
  theme_bw()


#Correlation between duration and count visualised as a scatterplot. Scatterplots require both the X and Y axes to be defined.
#This can highlight the optimal amount of time spent in the field
HighfieldData %>% 
  drop_na(Count) %>% 
  drop_na(Duration) %>%
  filter(Duration > 45) %>%     #Specify boundaries to remove outliers
  filter(Duration < 90) %>% 
  filter(Count <100) %>% 
  ggplot(mapping = aes(x=Duration, y=Count))+
  geom_point(size = 2, alpha = .5)+         # Define the aesthetics of the points
  geom_smooth()+
  theme_bw()+
  labs(title = "Species counts")


#Correlation between distance and count 
HighfieldData %>% 
  drop_na(Count) %>% 
  drop_na(Distance) %>% 
  filter(Count <100) %>% 
  ggplot(mapping = aes(x=Distance, y=Count))+
  geom_point(size = 2, alpha= .5)+
  theme_bw()



#Counts per month
#This can highlight the optimal month of the year to see the greatest number of birds
#Though it might be better to have species per month instead 
HighfieldData %>% 
  drop_na(Count) %>% 
  group_by(Month) %>% 
  ggplot(mapping = aes(x=Month))+
  geom_bar()


#Counts per month for a specific year
HighfieldData %>% 
  drop_na(Count) %>% 
  filter(Year == 2022) %>% 
  ggplot(mapping = aes(x=Month))+
  geom_bar()



#Number of species per month to see which months have the greatest number of species present
#I need to pull out all unique instances for each CommonName and Month and create a new data frame
SpMonth <-HighfieldData %>% distinct(Common.Name, Month)


#Plot by species 
#This show presence/absence for each species by month
SpMonth %>% 
  group_by(Month) %>% 
  ggplot(mapping = aes(x=Month, y=Common.Name))+
  geom_point()

#Plot by month
SpMonth %>% 
  group_by(Month) %>% 
  ggplot(mapping = aes(x=Month))+
  geom_bar()

#To test if this shows number of species by month:
#SpMonth %>% 
#filter(Month == 1)





#Plot a specific bird's presence over a year  
HighfieldData %>% 
  drop_na(Count) %>% 
  filter(Common.Name == "Redwing") %>% 
  ggplot(mapping = aes(x=Dayc, y=Count))+
  geom_point()+
  xlim(1,366)    #set the x-axis range. To set the y-axis range, use ylim()





#Visualise correlation between counts and duration of checklists across each year
HighfieldData %>% 
  drop_na(Duration) %>% 
  ggplot(mapping = aes(Duration, Count,
                       colour = Year))+
  geom_point(size = 2, alpha = 2)+
  coord_cartesian(xlim =c(0, 100), ylim = c(0, 100))+   #put xlim and ylim in same argument
  facet_wrap(~Year)+                                    #separates into own graphs
  labs("Total Highfield counts over time")



#Visualise correlation between counts and distance of checklist across each year
HighfieldData %>% 
  drop_na(Distance) %>% 
  ggplot(mapping = aes(Distance, Count,
                       colour = Year))+
  geom_point(size = .5, alpha = 2)+
  coord_cartesian(xlim = c(0,4), ylim = c(0,55))+
  facet_wrap(~Year)




#Density plots
HighfieldData %>% 
  drop_na(Count) %>% 
  filter(Common.Name %in% c('Eurasian Blackbird', 'Song Thrush', 'Redwing', 'Mistle Thrush')) %>% 
  ggplot(mapping = aes(Count,  
                       color = Common.Name,
                       fill = Common.Name))+
  geom_density(alpha = 0.2)+
  facet_wrap(~ Common.Name)
theme_test()






#11. Create a species list 

#Create a data frame of the unique species
SpList<- Highfield %>% 
  distinct(Common.Name, Scientific.Name)

#Create a file from a dataframe
write.csv(SpList, "Highfield Species List.csv", row.names = FALSE)