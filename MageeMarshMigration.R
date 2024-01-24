#Analyse eBird data for a specific location to see what species are most likely to be seen.
#e.g. Spring migration (May), Magee Marsh, Ohio

# Request and download data from eBird
# Instructions: https://science.ebird.org/en/use-ebird-data/download-ebird-data-products
# Download eBird Basic Data Set: https://ebird.org/data/download 



# Install packages
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


