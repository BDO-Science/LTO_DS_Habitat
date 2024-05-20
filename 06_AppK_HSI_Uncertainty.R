# Delta Smelt SCHISM output for calculating habitat suitability
# Step 6: create figures for discussing uncertainties in air temperature source and temperature threshold approach
# Created by: Kristi Arend, USBR
# Created on: 2/09/2024
# Last modified: 2/09/2024

library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(gridExtra)
library(ggplot2)
library(patchwork)
library(here)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import all of the summary output files; combine into a single dataframe ------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Go to the directory with all the text (.txt) files for the desired WYT and alternative 
## Create a list of file names 
fileNames1 <- Sys.glob("HSI_clean/DWR 2011 temp wet years/HSIR_*.rds")
fileNames2 <- Sys.glob("HSI_clean/DWR 2019 temp wet years/HSIR_*.rds")
fileNames3 <- Sys.glob("HSI_clean/NARR 1997 temp wet years/HSIR_*.rds")

# DWR 2011 temp source
## Create empty dataframe to populate
HSIR_wet1 <- data.frame()

#function uses file name list
for (fileName in fileNames1) {
  
  # Read in the file
  temp_df <- read_rds(fileName)
  
  #extract the alternative and WYT name from the file name and put it into an "alt" column
  temp_df$alt <- str_match(fileName, "^(?:[^_]*_){2}([^_]+(?:_[^_]+)*)_.*$")[,2]
  
  #binds extracted data back to the empty dataframe
  HSIR_wet1 <- rbind(HSIR_wet1,temp_df)
}

# Separate WYT and alt into two columns, add TempSource column
HSIR_wet1 <- HSIR_wet1 %>%
  separate_wider_delim(alt, delim = "_", names = c("alt", "WYT")) %>%
  mutate(TempSource = "DWR2011")


# DWR 2019 temp source
## Create empty dataframe to populate
HSIR_wet2 <- data.frame()

#function uses file name list
for (fileName in fileNames2) {
  
  # Read in the file
  temp_df <- read_rds(fileName)
  
  #extract the alternative and WYT name from the file name and put it into an "alt" column
  temp_df$alt <- str_match(fileName, "^(?:[^_]*_){2}([^_]+(?:_[^_]+)*)_.*$")[,2]
  
  #binds extracted data back to the empty dataframe
  HSIR_wet2 <- rbind(HSIR_wet2,temp_df)
}

# Separate WYT and alt into two columns, add TempSource column
HSIR_wet2 <- HSIR_wet2 %>%
  separate_wider_delim(alt, delim = "_", names = c("alt", "WYT")) %>%
  mutate(TempSource = "DWR2019")


# NARR 1997 temp source
## Create empty dataframe to populate
HSIR_wet3 <- data.frame()

#function uses file name list
for (fileName in fileNames3) {
  
  # Read in the file
  temp_df <- read_rds(fileName)
  
  #extract the alternative and WYT name from the file name and put it into an "alt" column
  temp_df$alt <- str_match(fileName, "^(?:[^_]*_){2}([^_]+(?:_[^_]+)*)_.*$")[,2]
  
  #binds extracted data back to the empty dataframe
  HSIR_wet3 <- rbind(HSIR_wet3,temp_df)
}

# Separate WYT and alt into two columns, add TempSource column
HSIR_wet3 <- HSIR_wet3 %>%
  separate_wider_delim(alt, delim = "_", names = c("alt", "WYT")) %>%
  mutate(TempSource = "NARR1997")

# One dataframe to rule them all
HSIR_wet <- rbind(HSIR_wet1, HSIR_wet2, HSIR_wet3)
# remove EIS-only alts
HSIR_wet <- HSIR_wet %>%
  filter(alt != "Alt1") %>%
  filter(alt != "Alt3") %>%
  filter(alt != "Alt4")

## check it out
view(HSIR_wet)

## Check structure and convert any characters as needed
str(HSIR_wet)

## check all alternatives and temp sources are included
unique(c(HSIR_wet$alt, HSIR_wet$TempSource))


#~~~~~~~~~
# Clean up -----
#~~~~~~~~~

rm(temp_df, fileName, fileNames1, fileNames2, fileNames3, HSIR_wet1, HSIR_wet2, HSIR_wet3)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Uncertainty 1: Air temperature source -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot setting

# Compare water temperatures across air temperature sources 
## create long df
HSIRwet_long <- HSIR_wet %>%
  select(!c(Fr.Sal6.mn, Fr.Temp22.mn)) %>%
  select(!Fr.Temp24.mn) %>%
  pivot_longer(cols = c("Sal.mn", "Temp.mn", "MaxSpd.mn", "Pr.Turb12.mn"),
               names_to = "env_var",
               values_to = "mean"
  )

## Set levels for air temperature source
HSIRwet_long$TempSource = factor(HSIRwet_long$TempSource, levels=c("NARR1997", "DWR2011", "DWR2019"))

## Set colors for the air temp sources
MyColors1 <- c("#88CCEE", "#FF671F", "#AA4499")

## assign levels
names(MyColors1) <- levels(HSIRwet_long$TempSource)


## Plot temperature
PlotTemp_Source <-HSIRwet_long %>%
  filter(env_var == "Temp.mn") %>%
  ggplot(aes(x=alt, y = mean, fill = TempSource)) +
  geom_boxplot() +
  # add horizontal lines for temp thresholds
  geom_hline(yintercept=22, linetype = 'dashed', color = "forestgreen", linewidth = 1) +
  geom_hline(yintercept=24, linetype = 'dashed', color = "forestgreen", linewidth = 1) +
  scale_fill_manual("Air temperature source", values = MyColors1) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle(paste("Summer-Fall temperature")) + 
  # rename the y axis label
  ylab("Temperature (deg C)") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

# Compare HSIR22 in Suisun Marsh and the Confluence across air temperature sources
HSIR_Arc <- HSIR_wet %>%
  filter(Subregion == "Confluence"|Subregion == "Suisun Marsh"|Subregion == "Yolo Bypass") %>%
  rename(BevHSI = BevHSIR, HSI = HSIR, HSI_24 = HSIR_24)

## Set levels for air temperature source
HSIR_Arc$TempSource = factor(HSIR_Arc$TempSource, levels=c("NARR1997", "DWR2011", "DWR2019"))

## Set colors for the air temp sources
MyColors2 <- c("#88CCEE", "#FF671F", "#AA4499")

## assign levels
names(MyColors2) <- levels(HSIR_Arc$TempSource)

Plot_HSI22source <- HSIR_Arc %>%
  ggplot(aes(x=alt, y=HSI, fill=TempSource)) +
  geom_boxplot(alpha = 0.7) +
  facet_grid(Subregion~.) +
  ggtitle("Effect of temperature source on HSI with 22 C threshold") + 
  # set y axis to between 0 and 1
  ylim(0,1) +
  # rename the y axis label
  ylab("Volume-weighted average HSI") +
  # specify legend title and box colors
  scale_fill_manual("Air temperature source", values = MyColors2) +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

# Compare HSIR24 in Suisun Marsh and the Confluence across air temperature sources
Plot_HSI24source <- HSIR_Arc %>%
  ggplot(aes(x=alt, y=HSI_24, fill=TempSource)) +
  geom_boxplot(alpha = 0.7) +
  facet_grid(Subregion~.) +
  ggtitle("Effect of temperature source on HSI with 24 C threshold") + 
  # set y axis to between 0 and 1
  ylim(0,1) +
  # rename the y axis label
  ylab("Volume-weighted average HSI") +
  # specify legend title and box colors
  scale_fill_manual("Air temperature source", values = MyColors2) +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Uncertainty 2: Air temperature threshold -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Compare Bever HSI, HSIR22, and HSIR24 for each air temperature source
## create long df
HSIRwet_longHSIR <- HSIR_wet %>%
  rename(BevHSI = BevHSIR, HSI = HSIR, HSI_24 = HSIR_24) %>%
  select(c(Subregion, BevHSI, HSI, HSI_24, alt, TempSource)) %>%
  pivot_longer(cols = c("BevHSI", "HSI", "HSI_24"),
               names_to = "HSI_msr",
               values_to = "HSI"
  )


# Set level for HSI threshold approach
HSIRwet_longHSIR$HSI_msr <- factor(HSIRwet_longHSIR$HSI_msr, levels=c("BevHSI", "HSI", "HSI_24"))

## assign colors
HSIcolors <- c("#9a3324", "#007396", "#DDCBA4")

## assign levels
names(HSIcolors) <- levels(HSIRwet_longHSIR$HSI_msr)
  

## Plot different HSIs
### DWR 2011 temperature source
PlotHSI_DWR11 <-HSIRwet_longHSIR %>%
  filter(TempSource == "DWR2011") %>%
  ggplot(aes(x=alt, y = HSI, fill = HSI_msr)) +
  geom_boxplot() +
  scale_fill_manual("HSI approach", values = HSIcolors) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle(paste("Effect of temperature threshold approach on HSI \n DWR 2011")) + 
  # rename the y axis label
  ylab("Volume-weighted average HSI") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

### DWR 2019 temperature source
PlotHSI_DWR19 <-HSIRwet_longHSIR %>%
  filter(TempSource == "DWR2019") %>%
  ggplot(aes(x=alt, y = HSI, fill = HSI_msr)) +
  geom_boxplot() +
  scale_fill_manual("HSI approach", values = HSIcolors) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle(paste("Effect of temperature threshold approach on HSI \n DWR 2019")) + 
  # rename the y axis label
  ylab("Volume-weighted average HSI") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

### NARR 1997 temperature source
PlotHSI_NARR97 <-HSIRwet_longHSIR %>%
  filter(TempSource == "NARR1997") %>%
  ggplot(aes(x=alt, y = HSI, fill = HSI_msr)) +
  geom_boxplot() +
  scale_fill_manual("HSI approach", values = HSIcolors) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle(paste("Effect of temperature threshold approach on HSI \n NARR 1997")) + 
  # rename the y axis label
  ylab("Volume-weighted average HSI") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))



#~~~~~~~~~~~
# Save plots ----
#~~~~~~~~~~~

#Uncertainty 1, Temperature
jpeg(paste("HSI_figs_tables/Uncertainty_Temps.jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(PlotTemp_Source)
dev.off()

#Uncertainty 1, HSI22
jpeg(paste("HSI_figs_tables/Uncertainty_HSI22_TempSource.jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_HSI22source)
dev.off()

#Uncertainty 1, HSI24
jpeg(paste("HSI_figs_tables/Uncertainty_HSI24_TempSource.jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_HSI24source)
dev.off()

#Uncertainty 2, DWR11
jpeg(paste("HSI_figs_tables/Uncertainty_HSI_DWR2011.jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(PlotHSI_DWR11)
dev.off()

#Uncertainty 2, DWR19
jpeg(paste("HSI_figs_tables/Uncertainty_HSI_DWR2019.jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(PlotHSI_DWR19)
dev.off()


#Uncertainty 2, NARR1997
jpeg(paste("HSI_figs_tables/Uncertainty_HSI_NARR1997.jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(PlotHSI_NARR97)
dev.off()