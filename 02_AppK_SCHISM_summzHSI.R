# Delta Smelt SCHISM output for calculating habitat suitability
# Step 2: calculate and summarize HSI
# Created by: Kristi Arend, USBR
# Created on: 09/21/2023
# Last modified: 02/08/2023

library(tidyverse)
library(dplyr)
library(lubridate)
library(here)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Saved Spatial SCHISM file and set naming variables ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Open file for DESIRED ALT_YEAR
fileName <- "SCHISM_combo/Alt4_BN_2012.rds"
SCHISM <- read_rds(fileName)

# pull out the alternative name 
AltID <- "Alt"

if (AltID == "Alt") {
  alt <- str_extract(fileName, "Alt\\w+_\\d{4}")
} else if (AltID == "NAA") {
  alt <- str_extract(fileName, "NAA\\w+_\\d{4}")
} else {
  alt <- str_extract(fileName, "EXP\\w+_\\d{4}")
}

# FOR WET YEARS, save file with CORRECT ALT_YEAR_TEMPYR: T11, T19, N97
TempYr <- ""

# If wet year, save in correct temp source folder: non-wet year = ""; wet year =  "DWR 2011 Temp wet years/", "DWR 2019 Temp wet years/",
## or "NARR 1997 Temp wet years/"
Tfolder <- ""

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate depth-averaged HSI by subregion ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate Bever without temp
# For each biweekly time period, multiply suitability index * area for each xy coor: area*Bever, area*HSIB, and area*HSIB_24

HSI_prep <- SCHISM %>%
  mutate(BevHSI = (1-Probility_Turb.12)*((0.67*BeverS + 0.33*BeverV)*0.42)+(Probility_Turb.12)*(0.67*BeverS + 0.33*BeverV)) %>%
  mutate(BevHSI_A = BevHSI*Cell_Area) %>%
  mutate(HSIB_A = HSIB*Cell_Area) %>%
  mutate(HSIB_24_A = HSIB_24*Cell_Area)


# sum across all xy coords within subregion, divide by total area subregion
HSI_avg_wgt <- HSI_prep %>%
  group_by(Subregion, Subregion_ID, date) %>%
  summarise(BevHSIR = sum(BevHSI_A) / sum(Cell_Area),
            HSIR = sum(HSIB_A) / sum(Cell_Area),
            HSIR_24 = sum(HSIB_24_A / sum(Cell_Area)),
            Fr.Sal6.mn = mean(Fraction_Salinity.6),
            Fr.Temp22.mn = mean(Fraction_Temp.22),
            Fr.Temp24.mn = mean(Fraction_Temp.24),
            Pr.Turb12.mn = mean(Probility_Turb.12),
            Sal.mn = mean(Avg_Sal),
            Temp.mn = mean(Avg_Temp),
            MaxSpd.mn = mean(Max_Speed)
            )

# Create columns that identify by number the first and second weeks of 
## each biweekly time period

HSI_R_df <- HSI_avg_wgt %>%
  mutate(week_end = isoweek(ymd(date)),
         week_start = week_end - 1)
         

#~~~~~~~~~~~~~~
# explore data -----
#~~~~~~~~~~~~~~

# Look at general distribution

# Scatterplot function
Scatter <- function(df, param) {
  df %>%
    ggplot(aes(x=date, y = param, color = Subregion)) +
    geom_point() +
    theme_bw() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
          axis.text.y = element_text(size = 12))
}
  

# Bever HSI scatter
Bev_Scat <- Scatter(df = HSI_R_df , param = HSI_R_df$BevHSIR)

# HSI scatter
HSI_Scat <- Scatter(df = HSI_R_df , param = HSI_R_df$HSIR)

# HSI 24 scatter
HSI24_Scat <- Scatter(df = HSI_R_df , param = HSI_R_df$HSIR_24)

# Temp scatter
Temp_Scat <- Scatter(df = HSI_R_df , param = HSI_R_df$Temp.mn)

# Salinity scatter
Sal_Scat <- Scatter(df = HSI_R_df , param = HSI_R_df$Sal.mn)

# Prob turbidity scatter
PrTurb_Scat <- Scatter(df = HSI_R_df , param = HSI_R_df$Pr.Turb12.mn)

# Max current speed scatter
MxSpd_Scat <- Scatter(df = HSI_R_df , param = HSI_R_df$MaxSpd.mn)


#~~~~~~~~~
# Clean up -----
#~~~~~~~~~
# Remove plots
rm(Bev_Scat, HSI_Scat, HSI24_Scat, Temp_Scat, Sal_Scat, PrTurb_Scat, MxSpd_Scat)

# Remove files
rm(SCHISM, HSI_avg_wgt, HSI_prep)
  

#~~~~~~~~~~~~~~~~
# Save final data ------
#~~~~~~~~~~~~~~~~

# Save file with CORRECT ALT_YEAR_TEMPYR and in the correct folder
if (TempYr == "") {
  saveRDS(HSI_R_df, file = paste("HSI_clean/HSIR_",alt,".rds",sep=""))
} else {
  saveRDS(HSI_R_df, file = paste("HSI_clean/",Tfolder,"HSIR_",alt,TempYr,".rds",sep=""))
}

