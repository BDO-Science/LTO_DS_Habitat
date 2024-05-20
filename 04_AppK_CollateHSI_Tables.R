# Delta Smelt SCHISM output for calculating habitat suitability
# Step 4: create HSI summary tables that include all alternatives by water year type
# Created by: Kristi Arend, USBR
# Created on: 09/22/2023
# Last modified: 05/09/2024

library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(gridExtra)
library(here)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import all of the summary output files; combine into a single dataframe ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Go to the directories with all the text (.txt) files for the desired WYT and alternative 
## All but wet year files are in HSI_Clean; pull wet year files from the appropriate
##folder: NARR 1997 temp wet years, DWR 2011 temp wet years, DWR 2019 temp wet years 

## Create a list of file names 
fileNames1 <- Sys.glob("HSI_clean/HSIRtable_*.rds")
fileNames2 <- Sys.glob("HSI_clean/DWR 2019 temp wet years/HSIRtable_*.rds")
fileNames <- c(fileNames1, fileNames2)
view(fileNames)

## Specify temperature source: "NARR97", "DWR2011", or "DWR2019"
TempSource <- "DWR2019"

# Create empty dataframe to populate
HSIRsumm_all <- data.frame()


#function uses file name list
for (fileName in fileNames) {
  
  # Read in the file
  temp_df <- read_rds(fileName)
  
  #extract the year from the file name and put it into a "year" column
  temp_df$year <- str_sub(fileName, -8, -5)
 
  #extract the alternative and WYT name from the file name and put it into an "alt" column
  temp_df$alt <- str_match(fileName, "^(?:[^_]*_){2}([^_]+(?:_[^_]+)*)_.*$")[,2]
   
  #convert character columns to numeric
  temp_df2 <- temp_df %>%
    mutate_at(c(6:14), as.numeric)
  
  #binds extracted data back to the empty dataframe
  HSIRsumm_all <- rbind(HSIRsumm_all,temp_df2)
}

# Check and convert any characters as needed
str(HSIRsumm_all)

## convert year from character to date
HSIRsumm_all$year <- as.Date(as.character(HSIRsumm_all$year), format = "%Y")

# check all alternatives by water year are included
unique(HSIRsumm_all$alt)

#~~~~~~~~~
# Clean up -----
#~~~~~~~~~
rm(fileName, fileNames, temp_df, temp_df2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create tables of Bever HSIR for all alternatives and water year types -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# HSIR means and st devs only; round; combine mn and sd into one column
BevHSIRsumm_only <- HSIRsumm_all %>%
  select(c(alt, Region, BevHSIR.mn, BevHSIR.sd)) %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(BevHSIR.mn.sd = str_c(BevHSIR.mn, " +/- ", BevHSIR.sd))

# Separate the alt name from the water year type into two columns  
BevHSIRsumm_only <- BevHSIRsumm_only %>%
  separate_wider_delim(alt, delim = "_", names = c("alt", "WYT"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BA Table for reporting Bever HSIR means and standard deviations -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Delete .mn and .sd columns
BevHSIR.mn.sd <- BevHSIRsumm_only %>%
  select(!c(BevHSIR.mn, BevHSIR.sd))

# Convert to wide so alternatives are columns
BevHSIR.BA <- BevHSIR.mn.sd %>%
  pivot_wider(names_from = alt, values_from = BevHSIR.mn.sd) %>%
  # reorder the columns
  select(c(WYT, Region, EXP1, EXP3, NAA, Alt2v1wTUCP, Alt2v1woTUCP, Alt2v2, Alt2v3)) %>%
  rename(Alt2wTUCPwoVA = Alt2v1wTUCP, Alt2woTUCPwoVA = Alt2v1woTUCP, 
         Alt2woTUCPDeltaVA = Alt2v2, Alt2woTUCPAllVA = Alt2v3)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EIS Table for reporting Bever HSIR means and standard deviations -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Convert to wide so alternatives are columns
BevHSIR.EIS <- BevHSIR.mn.sd %>%
  pivot_wider(names_from = alt, values_from = BevHSIR.mn.sd) %>%
  # reorder the columns
  select(c(WYT, Region, NAA, Alt1, Alt2v1wTUCP, Alt2v1woTUCP, Alt2v2, Alt2v3, Alt3, Alt4)) %>%
  rename(Alt2wTUCPwoVA = Alt2v1wTUCP, Alt2woTUCPwoVA = Alt2v1woTUCP, 
         Alt2woTUCPDeltaVA = Alt2v2, Alt2woTUCPAllVA = Alt2v3)


#~~~~~~~~~
# Clean up -----
#~~~~~~~~~
rm(BevHSIR.mn.sd)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table for reporting Bever HSIR percent change -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Same as previous table but include percent difference from NAA scenario in parenthesis
BevHSIR.perdiff <- BevHSIRsumm_only %>%
  select(!c(BevHSIR.sd, BevHSIR.mn.sd)) %>%
  pivot_wider(names_from = alt, values_from = BevHSIR.mn)

# calculate the percent difference between NAA and each alternative
BevHSIR.perdiff2 <- BevHSIR.perdiff %>%
  mutate(across(c(3:ncol(BevHSIR.perdiff)), .fns = ~(. - NAA) / NAA))  %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate_at("NAA", ~na_if(., 0))

## rename columns to indicate are percent difference
colnames(BevHSIR.perdiff2) <- paste0(colnames(BevHSIR.perdiff2), "_pdiff")

# combine Bever.perdiff and Bever.perdiff2 so that percent difference is in parenthesis
BevHSIR.perdiff3 <- cbind(BevHSIR.perdiff, BevHSIR.perdiff2) %>%
  select(!c(WYT_pdiff, Region_pdiff)) %>%
  mutate(Alt1.d = str_c(Alt1, " (", Alt1_pdiff, ")")) %>%
  mutate(Alt2v1wTUCP.d = str_c(Alt2v1wTUCP, " (", Alt2v1wTUCP_pdiff, ")")) %>%
  mutate(Alt2v1woTUCP.d = str_c(Alt2v1woTUCP, " (", Alt2v1woTUCP_pdiff, ")")) %>%
  mutate(Alt2v2.d = str_c(Alt2v2, " (", Alt2v2_pdiff, ")")) %>%
  mutate(Alt2v3.d = str_c(Alt2v3, " (", Alt2v3_pdiff, ")")) %>%
  mutate(EXP1.d = str_c(EXP1, " (", EXP1_pdiff, ")")) %>%
  mutate(EXP3.d = str_c(EXP3, " (", EXP3_pdiff, ")")) %>%
  mutate(Alt3.d = str_c(Alt3, " (", Alt3_pdiff, ")")) %>%
  mutate(Alt4.d = str_c(Alt4, " (", Alt4_pdiff, ")")) %>%
  # keep just WYT, Region, NAA, and the columns with percent different in parenthesis
  select(c(WYT, Region, EXP1.d, EXP3.d, NAA, Alt1.d, Alt2v1wTUCP.d, Alt2v1woTUCP.d, Alt2v2.d, 
           Alt2v3.d, Alt3.d, Alt4.d)) %>%
  #rename columns
  rename(EXP1 = EXP1.d, EXP3 = EXP3.d, Alt1 = Alt1.d, Alt2wTUCPwoVA = Alt2v1wTUCP.d, Alt2woTUCPwoVA = Alt2v1woTUCP.d, 
         Alt2woTUCPDeltaVA = Alt2v2.d, Alt2woTUCPAllVA = Alt2v3.d, Alt3 = Alt3.d, Alt4 = Alt4.d)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BA Table for reporting Bever HSIR percent difference -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
BevHSIR.perdiff.BA <- BevHSIR.perdiff3 %>%
  select(c(WYT, Region, EXP1, EXP3, NAA, Alt2wTUCPwoVA, Alt2woTUCPwoVA, Alt2woTUCPDeltaVA, Alt2woTUCPAllVA))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EIS Table for reporting Bever HSIR percent difference -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
BevHSIR.perdiff.EIS <- BevHSIR.perdiff3 %>%
  select(c(WYT, Region, NAA, Alt1, Alt2wTUCPwoVA, Alt2woTUCPwoVA, Alt2woTUCPDeltaVA, Alt2woTUCPAllVA, Alt3, Alt4))

#~~~~~~~~~
# Clean up -----
#~~~~~~~~~
rm(BevHSIRsumm_only, BevHSIR.perdiff, BevHSIR.perdiff2, BevHSIR.perdiff3)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create tables of HSIR 22C for all alternatives and water year types -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# HSIR means and st devs only; round; combine mn and sd into one column
HSIRsumm_only <- HSIRsumm_all %>%
  select(c(alt, Region, HSIR.mn, HSIR.sd)) %>%
#  mutate(across(where(is.numeric),round,3)) %>% # this is deprecated and produces an error
  mutate_if(is.numeric, round, 3) %>%
  mutate(HSIR.mn.sd = str_c(HSIR.mn, " +/- ", HSIR.sd))

# Separate the alt name from the water year type into two columns  
HSIRsumm_only <- HSIRsumm_only %>%
  separate_wider_delim(alt, delim = "_", names = c("alt", "WYT"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BA Table for reporting HSIR 22C means and standard deviations -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Delete .mn and .sd columns
HSIR.mn.sd <- HSIRsumm_only %>%
  select(!c(HSIR.mn, HSIR.sd))

# Convert to wide so alternatives are columns
HSIR.BA <- HSIR.mn.sd %>%
  pivot_wider(names_from = alt, values_from = HSIR.mn.sd) %>%
  # reorder the columns
  select(c(WYT, Region, EXP1, EXP3, NAA, Alt2v1wTUCP, Alt2v1woTUCP, Alt2v2, Alt2v3)) %>%
  rename(Alt2wTUCPwoVA = Alt2v1wTUCP, Alt2woTUCPwoVA = Alt2v1woTUCP, 
         Alt2woTUCPDeltaVA = Alt2v2, Alt2woTUCPAllVA = Alt2v3)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EIS Table for reporting HSIR 22C means and standard deviations -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Convert to wide so alternatives are columns
HSIR.EIS <- HSIR.mn.sd %>%
  pivot_wider(names_from = alt, values_from = HSIR.mn.sd) %>%
  # reorder the columns
  select(c(WYT, Region, NAA, Alt1, Alt2v1wTUCP, Alt2v1woTUCP, Alt2v2, Alt2v3, Alt3, Alt4)) %>%
  rename(Alt2wTUCPwoVA = Alt2v1wTUCP, Alt2woTUCPwoVA = Alt2v1woTUCP, 
         Alt2woTUCPDeltaVA = Alt2v2, Alt2woTUCPAllVA = Alt2v3)


#~~~~~~~~~
# Clean up -----
#~~~~~~~~~
rm(HSIRsumm_all, HSIR.mn.sd)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table for reporting HSIR 22 C percent difference -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Same as previous table but include percent difference from NAA scenario in parenthesis
HSIR.perdiff <- HSIRsumm_only %>%
  select(!c(HSIR.sd, HSIR.mn.sd)) %>%
  pivot_wider(names_from = alt, values_from = HSIR.mn)

# calculate the percent difference between NAA and each alternative
HSIR.perdiff2 <- HSIR.perdiff %>%
  mutate(across(c(3:ncol(HSIR.perdiff)), .fns = ~(. - NAA) / NAA))  %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate_at("NAA", ~na_if(., 0))

## rename columns to indicate are percent difference
colnames(HSIR.perdiff2) <- paste0(colnames(HSIR.perdiff2), "_pdiff")

# combine HSIR.perdiff and HSIR.perdiff2 so that percent difference is in parenthesis
HSIR.perdiff3 <- cbind(HSIR.perdiff, HSIR.perdiff2) %>%
  select(!c(WYT_pdiff, Region_pdiff)) %>%
  mutate(Alt1.d = str_c(Alt1, " (", Alt1_pdiff, ")")) %>%
  mutate(Alt2v1wTUCP.d = str_c(Alt2v1wTUCP, " (", Alt2v1wTUCP_pdiff, ")")) %>%
  mutate(Alt2v1woTUCP.d = str_c(Alt2v1woTUCP, " (", Alt2v1woTUCP_pdiff, ")")) %>%
  mutate(Alt2v2.d = str_c(Alt2v2, " (", Alt2v2_pdiff, ")")) %>%
  mutate(Alt2v3.d = str_c(Alt2v3, " (", Alt2v3_pdiff, ")")) %>%
  mutate(EXP1.d = str_c(EXP1, " (", EXP1_pdiff, ")")) %>%
  mutate(EXP3.d = str_c(EXP3, " (", EXP3_pdiff, ")")) %>%
  mutate(Alt3.d = str_c(Alt3, " (", Alt3_pdiff, ")")) %>%
  mutate(Alt4.d = str_c(Alt4, " (", Alt4_pdiff, ")")) %>%
  # keep just WYT, Region, NAA, and the columns with percent different in parenthesis
  select(c(WYT, Region, EXP1.d, EXP3.d, NAA, Alt1.d, Alt2v1wTUCP.d, Alt2v1woTUCP.d, Alt2v2.d, 
           Alt2v3.d, Alt3.d, Alt4.d)) %>%
  #rename columns
  rename(EXP1 = EXP1.d, EXP3 = EXP3.d, Alt1 = Alt1.d, Alt2wTUCPwoVA = Alt2v1wTUCP.d, Alt2woTUCPwoVA = Alt2v1woTUCP.d, 
         Alt2woTUCPDeltaVA = Alt2v2.d, Alt2woTUCPAllVA = Alt2v3.d, Alt3 = Alt3.d, Alt4 = Alt4.d)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BA Table for reporting HSIR 22C percent difference -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
HSIR.perdiff.BA <- HSIR.perdiff3 %>%
  select(c(WYT, Region, EXP1, EXP3, NAA, Alt2wTUCPwoVA, Alt2woTUCPwoVA, Alt2woTUCPDeltaVA, Alt2woTUCPAllVA))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EIS Table for reporting HSIR 22C percent difference -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
HSIR.perdiff.EIS <- HSIR.perdiff3 %>%
  select(c(WYT, Region, NAA, Alt1, Alt2wTUCPwoVA, Alt2woTUCPwoVA, Alt2woTUCPDeltaVA, Alt2woTUCPAllVA, Alt3, Alt4))


#~~~~~~~~~
# Clean up -----
#~~~~~~~~~
rm(HSIRsumm_only, HSIR.perdiff, HSIR.perdiff2, HSIR.perdiff3)


#~~~~~~~~~~~~
# Save tables ------
#~~~~~~~~~~~~

# Save summary Bever HSI BA tables as .csv
write.csv(BevHSIR.BA, file = paste("HSI_figs_tables/BevHSIall_Table_BA_sd_",TempSource,".csv", sep =  ""), row.names=FALSE)

write.csv(BevHSIR.perdiff.BA, file = paste("HSI_figs_tables/BevHSIall_Table_BA_perdiff_",TempSource,".csv", sep =  ""), row.names=FALSE)

# Save summary HSIR 22 C BA tables as .csv
write.csv(HSIR.BA, file = paste("HSI_figs_tables/HSIall_Table_BA_sd_",TempSource,".csv", sep =  ""), row.names=FALSE)

write.csv(HSIR.perdiff.BA, file = paste("HSI_figs_tables/HSIall_Table_BA_perdiff_",TempSource,".csv", sep =  ""), row.names=FALSE)

# Save summary Bever HSI EIS tables as .csv
write.csv(BevHSIR.EIS, file = paste("HSI_figs_tables/BevHSIall_Table_EIS_sd_",TempSource,".csv", sep =  ""), row.names=FALSE)

write.csv(BevHSIR.perdiff.EIS, file = paste("HSI_figs_tables/BevHSIall_Table_EIS_perdiff_",TempSource,".csv", sep =  ""), row.names=FALSE)

# Save summary HSIR 22 C EIS tables as .csv
write.csv(HSIR.EIS, file = paste("HSI_figs_tables/HSIall_Table_EIS_sd_",TempSource,".csv", sep =  ""), row.names=FALSE)

write.csv(HSIR.perdiff.EIS, file = paste("HSI_figs_tables/HSIall_Table_EIS_perdiff_",TempSource,".csv", sep =  ""), row.names=FALSE)


