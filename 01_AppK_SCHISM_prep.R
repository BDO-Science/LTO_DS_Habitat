# Delta Smelt SCHISM output for calculating habitat suitability
# Step 1: combine all data in to one file for each alternative * water year type
# Created by: Kristi Arend, USBR
# Created on: 09/19/2023
# Last modified: 02/08/2024

# Note: SCHISM coordinates are in NAD 1983 (EPSG:4269) UTM Zone 10N (EPSG: 32610)

# Load libraries
library(tidyverse)
library(dplyr)
library(docxtractr)
library(lubridate)
library(here)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import all of the SCHISM output files; set naming variables; combine into a single dataframe ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create empty dataframe to populate
SCHISM <- data.frame()

# Dowload QAQC check; create dataframe to store number of rows in each file
rowcount <- data.frame()

## specify the alternative's first three letters: Alt, NAA, or EXP
AltID <- "Alt"

# FOR WET YEARS, identify if using output from DWR 2011, DWR 2019, or NARR 1997 air temperatures: T11, T19, N97
TempYr <- ""

# Go to the directory with all the text (.txt) files for the desired WYT and alternative 
## FOR NON-WET YEARS: Create a list of file names 
fileNames <- Sys.glob("SCHISM_raw/Alt3_C_2015/*.txt")

## FOR WET YEARS: subfolder choices are: NARR_Temp, DWR_Temp_2011, and DWR_Temp_2019
#fileNames <- Sys.glob("SCHISM_raw/Alt4_W_1997/DWR_Temp_2019/*.txt")

view(fileNames)

#function uses file name list to compile individual files into a single dataframe
for (fileName in fileNames) {
  
  # Read in the file
  temp_df <- read.delim(fileName, header = TRUE, sep = ",", dec = ".")

  #extract the date from the file name
  temp_df$date <- mdy(gsub(".*[_]|[.].*", "", fileName))
  
  #convert character columns to numeric
  temp_df2 <- temp_df %>%
    mutate_at(c(6:14), as.numeric)
  
  #remove all rows that have NAs from temp_df2; remove the Subregion "n/a"
  temp_df3 <- temp_df2 %>%
    drop_na() %>%
    filter(Subregion != "n/a")
      
  #binds extracted data back to the empty dataframe
  SCHISM <- rbind(SCHISM,temp_df3)
  
  # count rows in dataframe
  currrows <- data.frame(temp_df$date[1], nrow(temp_df))
  
  # bind to rowcount dataframe
  rowcount <- rbind(rowcount, currrows)
  
}

# Make sure everything is numeric that needs to be
str(SCHISM)

# If temp is character
#SCHISM$Avg_Temp <- as.numeric(as.character(SCHISM$Avg_Temp))

# check that all files downloaded fully by checking that:
## all files have 12 subregions each
S.check <- unique(SCHISM[c("Subregion", "date")])
S.check.summ <- S.check %>%
  group_by(date) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter (n > 1L)

view(S.check.summ)

## all files have 319579 rows each
view(rowcount)


#~~~~~~~~~
# Clean up -----
#~~~~~~~~~
rm(temp_df, temp_df2, temp_df3, fileNames, currrows, rowcount, S.check, S.check.summ)

  
#~~~~~~~~~~~~~~~~~~~~~~~
# Save SCHISM dataframe ------
#~~~~~~~~~~~~~~~~~~~~~~~

# pull out the alternative name

if (AltID == "Alt") {
  alt <- str_extract(fileName, "Alt\\w+_\\d{4}")
} else if (AltID == "NAA") {
  alt <- str_extract(fileName, "NAA\\w+_\\d{4}")
} else {
  alt <- str_extract(fileName, "EXP\\w+_\\d{4}")
}

# Save file WITH CORRECT ALT_YEAR

# FOR WET YEARS, save file with CORRECT ALT_YEAR_TEMPYR: T11, T19, N97
if (TempYr == "") {
  saveRDS(SCHISM, file = paste("SCHISM_combo/",alt,".rds",sep=""))
} else {
  saveRDS(SCHISM, file = paste("SCHISM_combo/",alt,TempYr,".rds",sep=""))
}






