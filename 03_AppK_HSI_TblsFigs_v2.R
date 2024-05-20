# Delta Smelt SCHISM output for calculating habitat suitability
# Step 3: create HSI summary tables and figures for each alternative and water year type
# Created by: Kristi Arend, USBR
# Created on: 09/22/2023
# Last modified: 03/04/2023

library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(patchwork)
library(here)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Saved weighted average HSI data and set naming variables ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# set file_name to desired ALT_YEAR; for wet years choose from: "NARR 1997 temp wet years/", "DWR 2019 temp wet years/", "DWR 2011 temp wet years/" 
## and add "N97", "T19", or "T11" to the file name after the year
fileName <- "HSI_clean/HSIR_Alt4_BN_2012.rds"

# Load the file...OR...
HSI_R_final <- read_rds(fileName)

# ...start with the dataframe from 02 script
#HSI_R_final <- HSI_R_df

# Extract the alternative from the filename
## used for plot titles and file names
## AltID: "Alt", "EXP", or "NAA"

AltID <- "Alt"

if (AltID == "Alt") {
  alt <- str_extract(fileName, "Alt\\w+_\\d{4}")
} else if (AltID == "NAA") {
  alt <- str_extract(fileName, "NAA\\w+_\\d{4}")
} else {
  alt <- str_extract(fileName, "EXP\\w+_\\d{4}")
}

# Set Tyear for plotting: non-wet year = ""; wet year = "DWR 2011", "DWR 2019", or "NARR 1997"
Tyear <- ""

#Set Tyear2 for saving: non-wet year = ""; wet year = "T11", "T19", or "N97"
Tyear2 <- ""

# If wet year, save in correct temp source folder: non-wet year = ""; wet year =  "DWR 2011 Temp wet years/", "DWR 2019 Temp wet years/",
## or "NARR 1997 Temp wet years/"
Tfolder <- ""

#~~~~~~~~~~~~~~~~~~~~~~
# Create summary tables ------
#~~~~~~~~~~~~~~~~~~~~~~

# Summarize means and standard deviations by subregion 
HSIR_smmy <- HSI_R_final %>%
  group_by(Subregion, Subregion_ID) %>%
  summarise(BevHSIR.mn = mean(BevHSIR),
            BevHSIR.sd = sd(BevHSIR),
            HSIR.mn = mean(HSIR),
            HSIR.sd = sd(HSIR),
            HSIR_24.mn = mean(HSIR_24),
            HSIR_24.sd = sd(HSIR_24),
            Fr.Sal6.mn2 = mean(Fr.Sal6.mn),
            Fr.Sal6.sd = sd(Fr.Sal6.mn),
            Fr.Temp22.mn2 = mean(Fr.Temp22.mn),
            Fr.Temp22.sd = sd(Fr.Temp22.mn),
            Fr.Temp24.mn2 = mean(Fr.Temp24.mn),
            Fr.Temp24.sd = sd(Fr.Temp24.mn),
            Pr.Turb12.mn2 = mean(Pr.Turb12.mn),
            Pr.Turb12.sd = sd(Pr.Turb12.mn),
            Sal.mn2 = mean(Sal.mn),
            Sal.sd = sd(Sal.mn),
            Temp.mn2 = mean(Temp.mn),
            Temp.sd = sd(Temp.mn),
            MaxSpd.mn2 = mean(MaxSpd.mn),
            MaxSpd.sd = sd(MaxSpd.mn)
  ) %>%
  ungroup()


# For entire Delta 
HSIR_Delta <- HSI_R_final %>%
  select(!c("week_end", "week_start")) %>%
  group_by() %>%   # prevents grouping by subregion
  summarise(BevHSIR.mn = mean(BevHSIR),
            BevHSIR.sd = sd(BevHSIR),
            HSIR.mn = mean(HSIR),
            HSIR.sd = sd(HSIR),
            HSIR_24.mn = mean(HSIR_24),
            HSIR_24.sd = sd(HSIR_24),
            Fr.Sal6.mn2 = mean(Fr.Sal6.mn),
            Fr.Sal6.sd = sd(Fr.Sal6.mn),
            Fr.Temp22.mn2 = mean(Fr.Temp22.mn),
            Fr.Temp22.sd = sd(Fr.Temp22.mn),
            Fr.Temp24.mn2 = mean(Fr.Temp24.mn),
            Fr.Temp24.sd = sd(Fr.Temp24.mn),
            Pr.Turb12.mn2 = mean(Pr.Turb12.mn),
            Pr.Turb12.sd = sd(Pr.Turb12.mn),
            Sal.mn2 = mean(Sal.mn),
            Sal.sd = sd(Sal.mn),
            Temp.mn2 = mean(Temp.mn),
            Temp.sd = sd(Temp.mn),
            MaxSpd.mn2 = mean(MaxSpd.mn),
            MaxSpd.sd = sd(MaxSpd.mn)
  ) %>%
  ungroup() %>%
  mutate(Region = "Delta", .before = BevHSIR.mn)   # add column 1 that IDs region


# Just Subregions of interest: "Confluence", "Lower Sacramento River", "NW Suisun",
## "Suisun Marsh", "Yolo Bypass"
HSIR_SFH <- HSI_R_final %>%
  select(!c("week_end", "week_start")) %>%
  #filter for subregions of greatest relevance to Delta smelt summer fall habitat
  filter(Subregion == "Confluence"|Subregion == "Lower Sacramento River"|
           Subregion == "NW Suisun"|Subregion == "Suisun Marsh"|Subregion == "Yolo") %>%
  group_by() %>%   # prevents grouping by subregion
  summarise(BevHSIR.mn = mean(BevHSIR),
            BevHSIR.sd = sd(BevHSIR),
            HSIR.mn = mean(HSIR),
            HSIR.sd = sd(HSIR),
            HSIR_24.mn = mean(HSIR_24),
            HSIR_24.sd = sd(HSIR_24),
            Fr.Sal6.mn2 = mean(Fr.Sal6.mn),
            Fr.Sal6.sd = sd(Fr.Sal6.mn),
            Fr.Temp22.mn2 = mean(Fr.Temp22.mn),
            Fr.Temp22.sd = sd(Fr.Temp22.mn),
            Fr.Temp24.mn2 = mean(Fr.Temp24.mn),
            Fr.Temp24.sd = sd(Fr.Temp24.mn),
            Pr.Turb12.mn2 = mean(Pr.Turb12.mn),
            Pr.Turb12.sd = sd(Pr.Turb12.mn),
            Sal.mn2 = mean(Sal.mn),
            Sal.sd = sd(Sal.mn),
            Temp.mn2 = mean(Temp.mn),
            Temp.sd = sd(Temp.mn),
            MaxSpd.mn2 = mean(MaxSpd.mn),
            MaxSpd.sd = sd(MaxSpd.mn)
  ) %>%
  ungroup() %>%
  mutate(Region = "SummerFall", .before = BevHSIR.mn)   # add column 1 that IDs region

# Combine subregion, Delta, and SFH dataframes into one for export  
HSIR_Table <- HSIR_smmy %>%
  rename(Region = Subregion) %>%
  select(!Subregion_ID) %>%
  rbind(HSIR_SFH,HSIR_Delta)


#~~~~~~~~~
# Clean up -----
#~~~~~~~~~
rm(HSIR_smmy,HSIR_Delta, HSIR_SFH)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rename strata and set order for plotting ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Rename subregions for plotting

# Check subregion names
unique(HSI_R_final$Subregion)
#[1] "Confluence"              "East Delta"              "Lower Sacramento River"  "Lower San Joaquin River"
#[5] "NE Suisun"               "NW Suisun"               "SE Suisun"               "SW Suisun"              
#[9] "Sacramento River"        "South Delta"             "Suisun Marsh"            "Yolo Bypass"

Stratum_lkup <- c("Confluence" = "Confl", "East Delta" = "E_Delta", "Lower Sacramento River" = "SacR_low",
                  "Lower San Joaquin River" = "SJR_low",  "NE Suisun" = "NE_SnBay", "NW Suisun" = "NW_SnBay", 
                  "SE Suisun" = "SE_SnBay", "SW Suisun" = "SW_SnBay", "Sacramento River" = "SacR",
                  "South Delta" = "S_Delta", "Suisun Marsh" = "SnMarsh", "Yolo Bypass" = "Yolo")

HSI_R_final$Sub2 <- as.character(Stratum_lkup[HSI_R_final$Subregion])

# Set strata levels for plotting
HSI_R_final$Sub2 = factor(HSI_R_final$Sub2, levels=c("SacR", "Yolo", "SacR_low", "SJR_low", 
                                                     "Confl", "SnMarsh", "NE_SnBay", "NW_SnBay",
                                                     "SE_SnBay", "SW_SnBay", "E_Delta", "S_Delta"))


#~~~~~~~~~~~~~~~~~~
# Plot by subregion -----
#~~~~~~~~~~~~~~~~~~

# create a function for plotting habitat suitability
PlotHSI <- function(df, param, Tthresh, Tyear) {
  df %>%
    ggplot(aes(x=Sub2, y=param)) +
    geom_boxplot() +
    stat_summary(fun = "mean", geom = "point", shape = 8,
                 size = 2, color = "black") +
    ggtitle(paste("Delta Smelt Habitat Suitability Index:", Tthresh, "\n", alt, Tyear, sep=" ")) + 
    # set y axis to between 0 and 1
    ylim(0,1) +
    # rename the y axis label
    ylab("Volume-weighted average HSI") +
    # choose the black-and-white theme for graphical display
    theme_bw() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
          axis.text.y = element_text(size = 12))
}


# Plot Bever HSI (no Temp threshold)
PlotBev_Subreg <- PlotHSI(df = HSI_R_final , param = HSI_R_final$BevHSIR, Tthresh = "Bever", Tyear = Tyear)


# Plot HSIR (20 C threshold)
PlotHSIR_Subreg <- PlotHSI(df = HSI_R_final , param = HSI_R_final$HSIR, Tthresh = "22 C", Tyear = Tyear)


# Plot HSIR_24 (24 C threshold)
PlotHSIR24_Subreg <- PlotHSI(df = HSI_R_final , param = HSI_R_final$HSIR_24, Tthresh = "24 C", Tyear = Tyear)


# Plot environmental conditions
## create long df
HSI_R_final_long <- HSI_R_final %>%
  select(!c(Fr.Sal6.mn, Fr.Temp22.mn)) %>%
  select(!Fr.Temp24.mn) %>%
  pivot_longer(cols = c("Sal.mn", "Temp.mn", "MaxSpd.mn", "Pr.Turb12.mn"),
               names_to = "env_var",
               values_to = "mean"
               )

## Create temperature and salinity df
Temp_Sal <- HSI_R_final_long %>%
  filter(env_var != "MaxSpd.mn") %>%
  filter(env_var != "Pr.Turb12.mn")

### Plot temperature and salinity
PlotTempSal_Subreg <- Temp_Sal %>%
  ggplot(aes(x=Sub2, y = mean, fill = env_var)) +
  geom_boxplot() +
  # add horizontal lines for temp thresholds
  geom_hline(yintercept=22, linetype = 'dashed', color = "#0072B2", linewidth = 1) +
  geom_hline(yintercept=24, linetype = 'dashed', color = "#0072B2", linewidth = 1) +
  # add horizontal line for salinity threshold
  geom_hline(yintercept=6, linetype = 'longdash', color = "#999999", linewidth = 1) +
  scale_fill_manual(values=c("#999999", "#0072B2"), 
                    name="Experimental\nCondition",
                    breaks=c("Sal.mn", "Temp.mn"),
                    labels=c("Salinity", "Temperature")) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle(paste("Summer-Fall temperature and salinity:\n", alt, Tyear, sep=" ")) + 
  # rename the y axis label
  ylab("Temperature (deg C), Salinity (psu)") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))


### plot probability turbidity > 12 NTU
PlotTurb_Subreg <- HSI_R_final_long %>%
  filter(env_var == "Pr.Turb12.mn") %>%
  ggplot(aes(x=Sub2, y = mean, fill=env_var)) +
  geom_boxplot() +
  scale_fill_manual(values="#999999") +
  ggtitle(paste("Summer-Fall turbidity:\n", alt, sep="")) + 
  # rename the y axis label
  ylab("Probability turbidity > 12 NTU") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))


### plot current speed
PlotCurrSp_Subreg <- HSI_R_final_long %>%
  filter(env_var == "MaxSpd.mn") %>%
  ggplot(aes(x=Sub2, y = mean, fill=env_var)) +
  geom_boxplot() +
  scale_fill_manual(values="#999999") +
  ggtitle(paste("Summer-Fall max current speed:\n", alt, sep="")) + 
  # rename the y axis label
  ylab("Maximum current speed") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(legend.position = "none", axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))


#~~~~~~~~~
# Clean up -----
#~~~~~~~~~
rm(HSI_R_final_long, Temp_Sal, Stratum_lkup)


#~~~~~~~~~~~~~~~~~~~~~~~~~
# Patchwork plots together -----
#~~~~~~~~~~~~~~~~~~~~~~~~~
# Make sure you loaded patchwork
AllPlots <- (PlotBev_Subreg + PlotHSIR_Subreg) / (PlotTempSal_Subreg + PlotCurrSp_Subreg)


#~~~~~~~~~~~~~~~~~~~~~~
# Save plots and tables ------
#~~~~~~~~~~~~~~~~~~~~~~

# Save summary table with DESIRED ALT_YEAR as .rds
#saveRDS(HSIR_Table, file = paste("HSI_clean/HSIRtable_" ,alt,".rds", sep=""))

if (Tfolder == "") {
  saveRDS(HSIR_Table, file = paste("HSI_clean/HSIRtable_",alt,".rds",sep=""))
} else {
  saveRDS(HSIR_Table, file = paste("HSI_clean/",Tfolder,"HSIRtable_",alt,Tyear2,".rds",sep=""))
}

# Save summary table with DESIRED ALT_YEAR as .csv
if (Tfolder == "") {
  write.csv(HSIR_Table, file = paste("HSI_clean/HSIRtable_",alt,".csv", sep=""), row.names=FALSE)
} else {
  write.csv(HSIR_Table, file = paste("HSI_clean/",Tfolder,"HSIRtable_",alt,Tyear2,".csv", sep=""), row.names=FALSE)
}

# Save Bever subregion plot with DESIRED ALT_YEAR
jpeg(paste("HSI_figs_tables/BeverHSI_subregion_",alt,Tyear2,".jpg", sep=""),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(PlotBev_Subreg)
dev.off()

# Save HSIR subregion plot with DESIRED ALT_YEAR
jpeg(paste("HSI_figs_tables/HSIR_subregion_",alt,Tyear2,".jpg", sep=""),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(PlotHSIR_Subreg)
dev.off()

# Save HSIR_24 subregion plot with DESIRED ALT_YEAR
jpeg(paste("HSI_figs_tables/HSIR24_subregion_",alt,Tyear2,".jpg", sep=""),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(PlotHSIR24_Subreg)
dev.off()


# Save salinity/temp subregion plot with DESIRED ALT_YEAR
jpeg(paste("HSI_figs_tables/TempSal_subregion_",alt,Tyear2,".jpg", sep=""),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(PlotTempSal_Subreg)
dev.off()

# Save HSIR subregion plot with DESIRED ALT_YEAR
jpeg(paste("HSI_figs_tables/PrTurb_subregion_",alt,Tyear2,".jpg", sep=""),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(PlotTurb_Subreg)
dev.off()

# Save all plots with DESIRED ALT_YEAR
jpeg(paste("HSI_figs_tables/AllPlots_",alt,Tyear2,".jpg", sep=""),
     height = 8,
     width = 10,
     units = "in",
     res = 300)
plot(AllPlots)
dev.off()

