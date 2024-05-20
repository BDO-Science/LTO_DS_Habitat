
# Delta Smelt SCHISM output for calculating habitat suitability
# Step 5: create final HSI summary figures that include all alternatives by water year type
# Created by: Kristi Arend, USBR
# Created on: 12/22/2023
# Last modified: 2/11/2024

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

# Go to the directories with all the text (.txt) files for the desired WYT and alternative 
## All but wet year files are in HSI_Clean; pull wet year files from the appropriate
##HSI_clean subfolder: NARR 1997 temp wet years, DWR 2011 temp wet years, DWR 2019 temp wet years 

## Create a list of file names 
fileNames1 <- Sys.glob("HSI_clean/HSIR_*.rds")
fileNames2 <- Sys.glob("HSI_clean/DWR 2019 temp wet years/HSIR_*.rds")
fileNames <- c(fileNames1, fileNames2)
view(fileNames)


## Specify temperature source: "NARR97", "DWR2011", or "DWR2019"
TempSource <- "DWR2019"

# Create empty dataframe to populate
HSIRplot_all <- data.frame()

#function uses file name list
for (fileName in fileNames) {
  
  # Read in the file
  temp_df <- read_rds(fileName)
  
  #extract the alternative and WYT name from the file name and put it into an "alt" column
  temp_df$alt <- str_match(fileName, "^(?:[^_]*_){2}([^_]+(?:_[^_]+)*)_.*$")[,2]
  
  #binds extracted data back to the empty dataframe
  HSIRplot_all <- rbind(HSIRplot_all,temp_df)
}

# Check and convert any characters as needed
str(HSIRplot_all)

# check all alternatives by water year are included
unique(HSIRplot_all$alt)

# Separate WYT and alt into two columns
HSIRplot_all <- HSIRplot_all %>%
  separate_wider_delim(alt, delim = "_", names = c("alt", "WYT"))


#~~~~~~~~~
# Clean up -----
#~~~~~~~~~

rm(temp_df, fileName, fileNames, fileNames1, fileNames2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rename alternatives and subregions for plotting -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Rename the alternatives to match the naming convention

## View alt names
unique(HSIRplot_all$alt)
# "Alt1"         "Alt2v1wTUCP"  "Alt2v1woTUCP" "Alt2v2"       "Alt2v3"       "Alt3"         "Alt4"         "EXP1"         "EXP3"         "NAA"

## Assign new names
Alt_lkup <- c("Alt1" = "Alt1", 
              "Alt2v1wTUCP" = "Alt2wTUCPwoVA", "Alt2v1woTUCP" = "Alt2woTUCPwoVA", "Alt2v2" = "Alt2woTUCPDeltaVA", "Alt2v3" = "Alt2woTUCPAllVA", 
              "Alt3" = "Alt3",
              "Alt4" = "Alt4",
              "EXP1" = "EXP1",
              "EXP3" = "EXP3",
              "NAA" = "NAA")

## Create column with new names, based on assignments above
HSIRplot_all$f.alt <- as.character(Alt_lkup[HSIRplot_all$alt])

# Rename subregions

## Assign subregion nicknames
Stratum_lkup <- c("Confluence" = "Confl", "East Delta" = "E_Delta", "Lower Sacramento River" = "SacR_low",
                  "Lower San Joaquin River" = "SJR_low",  "NE Suisun" = "NE_SnBay", "NW Suisun" = "NW_SnBay", 
                  "SE Suisun" = "SE_SnBay", "SW Suisun" = "SW_SnBay", "Sacramento River" = "SacR",
                  "South Delta" = "S_Delta", "Suisun Marsh" = "SnMarsh", "Yolo Bypass" = "Yolo")

## Create column with new nicknames, based on assignments above
HSIRplot_all$Sub2 <- as.character(Stratum_lkup[HSIRplot_all$Subregion])


#~~~~~~~~~
# Clean up -----
#~~~~~~~~~
rm(Alt_lkup, Stratum_lkup)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BA: Set variable levels for plotting HSIR and abiotic conditions for alternatives and water year types -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Filter out EIS alternatives
HSIRplot_all.BA <- HSIRplot_all %>%
  filter(f.alt != "Alt1") %>%
  filter(f.alt != "Alt3") %>%
  filter(f.alt != "Alt4")
  
# Set strata levels for plotting
HSIRplot_all.BA$Sub2 = factor(HSIRplot_all.BA$Sub2, levels=c("SacR", "Yolo", "SacR_low", "SJR_low", 
                                                     "Confl", "SnMarsh", "NE_SnBay", "NW_SnBay",
                                                     "SE_SnBay", "SW_SnBay", "E_Delta", "S_Delta"))  

# Set alternative levels for plotting
HSIRplot_all.BA$f.alt = factor(HSIRplot_all.BA$f.alt, levels=c("EXP1", "EXP3", "NAA", "Alt2wTUCPwoVA", "Alt2woTUCPwoVA",
                                               "Alt2woTUCPDeltaVA", "Alt2woTUCPAllVA"))

HSIRplot_all.BA$WYT = factor(HSIRplot_all.BA$WYT, levels=c("C", "D", "BN", "AN", "W"))
                                                   
# Set colors for the alternatives

## Nick's full palette
#my_palette=c('#003E51','#007396', '#C69214', '#DDCBA4','#FF671F', '#215732','#4C12A1','#9a3324', "#88CCEE","#AA4499")
## Alts: Alt1, Alt2wTUCPwoVA, Alt2woTUCPAllVA, Alt2woTUCPDeltaVA, Alt2woTUCPwoVA, Alt3, Alt4, EXP1, EXP3, NAA  

## My palette for current alts
MyColors <- c("#9a3324", "#88CCEE", "#AA4499", "#007396", "#FF671F", "#DDCBA4", "#C69214")

## assign levels
names(MyColors) <- levels(HSIRplot_all.BA$f.alt)

#~~~~~~~~~~~~~~~~~~~~~~~~~~
# BA: HSI PLOTTING FUNCTION -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~

#Delta and SHFA subregions combined

PlotHSIR <- function(df, xpar, ypar, pfill, Plottitle) {
  df %>%
    ggplot(aes(x=xpar, y=ypar, fill=pfill)) +
    geom_boxplot(alpha = 0.7) +
    ggtitle(Plottitle) + 
# set y axis to between 0 and 1
    ylim(0,1) +
# rename the y axis label
    ylab("Volume-weighted average HSI") +
# specify legend title and box colors
    scale_fill_manual("Alternatives", values = MyColors) +
# choose the black-and-white theme for graphical display
    theme_bw() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
          axis.text.y = element_text(size = 12))
}

#Individual subregions
PlotsubHSIR <- function(df, xpar, ypar, pfill, Plottitle) {
  df %>%
    ggplot(aes(x=xpar, y=ypar, fill=pfill)) +
    geom_boxplot(alpha = 0.7) +
    facet_grid(WYT~.) +
    ggtitle(Plottitle) + 
    # set y axis to between 0 and 1
    ylim(0,1) +
    # rename the y axis label
    ylab("Volume-weighted average HSI") +
    # specify legend title and box colors
    scale_fill_manual("Alternatives", values = MyColors) +
    # choose the black-and-white theme for graphical display
    theme_bw() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
          axis.text.y = element_text(size = 12))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~
# BA: HSIR:plot Delta mean ----
#~~~~~~~~~~~~~~~~~~~~~~~~~

## sum across all subregions for each WYT, date, and alt
DeltaHSIR <- HSIRplot_all.BA %>%
  select(c(WYT, f.alt, Sub2, date, BevHSIR, HSIR, HSIR_24)) %>%
  group_by(WYT, f.alt, date) %>%
  summarise(BevHSIR.mn =mean(BevHSIR),
            BevHSIR.sd = sd(BevHSIR),
            HSIR.mn = mean(HSIR),
            HSIR.sd = sd(HSIR),
            HSIR_24.mn = mean(HSIR_24),
            HSIR_24.sd = sd(HSIR_24)) %>%
  ungroup()

# Plot Bever et al HSIR (no temp threshold)
Plot_DeltaBev <- PlotHSIR(df = DeltaHSIR, xpar = DeltaHSIR$WYT, ypar = DeltaHSIR$BevHSIR.mn, pfill = DeltaHSIR$f.alt,
                              Plottitle = paste("Delta-wide Bever HSI \n",TempSource, sep = " "))

# Plot HSIR (22 C threshold)
Plot_DeltaHSIR22 <- PlotHSIR(df = DeltaHSIR, xpar = DeltaHSIR$WYT, ypar = DeltaHSIR$HSIR.mn, pfill = DeltaHSIR$f.alt,
           Plottitle = paste("Delta-wide HSI 22C \n",TempSource, sep = " "))


# Plot HSIR_24 (24 C threshold)
Plot_DeltaHSIR24 <- PlotHSIR(df = DeltaHSIR, xpar = DeltaHSIR$WYT, ypar = DeltaHSIR$HSIR_24.mn, pfill = DeltaHSIR$f.alt,
                             Plottitle = paste("Delta-wide HSI 24C \n",TempSource, sep = " "))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BA: HSIR: plot summer fall habitat mean ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate means and standard deviation for subregions relevant for summer-fall habitat
SFHHSIR <- HSIRplot_all.BA %>%
  filter(Subregion == "Confluence"|Subregion == "Lower Sacramento River"|
           Subregion == "NW Suisun"|Subregion == "Suisun Marsh"|Subregion == "Yolo") %>%
  select(c(WYT, f.alt, Sub2, date, BevHSIR, HSIR, HSIR_24)) %>%
  group_by(WYT, f.alt, date) %>%
  summarise(BevHSIR.mn =mean(BevHSIR),
            BevHSIR.sd = sd(BevHSIR),
            HSIR.mn = mean(HSIR),
            HSIR.sd = sd(HSIR),
            HSIR_24.mn = mean(HSIR_24),
            HSIR_24.sd = sd(HSIR_24)) %>%
  ungroup()

# Plot Bever et al. 2016 HSIR (no temp threshold)
Plot_SFHBev <- PlotHSIR(df = SFHHSIR, xpar = SFHHSIR$WYT, ypar = SFHHSIR$BevHSIR.mn, pfill = SFHHSIR$f.alt,
                        Plottitle = paste("Summer-fall habitat subregion Bever HSI \n",TempSource, sep = " "))

# Plot HSIR (22 C threshold)
Plot_SFHHSIR22 <- PlotHSIR(df = SFHHSIR, xpar = SFHHSIR$WYT, ypar = SFHHSIR$HSIR.mn, pfill = SFHHSIR$f.alt,
                         Plottitle = paste("Summer-fall habitat subregion HSI 22C \n",TempSource, sep = " "))
  
  
# Plot HSIR_24 (24 C threshold)
Plot_SFHHSIR24 <- PlotHSIR(df = SFHHSIR, xpar = SFHHSIR$WYT, ypar = SFHHSIR$HSIR_24.mn, pfill = SFHHSIR$f.alt,
                           Plottitle = paste("Summer-fall habitat subregion HSI 24C \n",TempSource, sep = " "))
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BA HSIR: plot all subregions mean ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot Bever et al. (2016) HSIR (no temp threshold) faceting by WYT
Plot_SubRegBevHSIR <- PlotsubHSIR(df = HSIRplot_all.BA, xpar = HSIRplot_all.BA$Sub2, ypar = HSIRplot_all.BA$BevHSIR, pfill = HSIRplot_all.BA$f.alt,
                                  Plottitle = paste("Subregion Bever HSI \n",TempSource, sep = ""))

Plot_SubregHSIR_NAA <- PlotsubHSIR(df = HSIRplot_NAA, xpar = HSIRplot_NAA$Sub2, ypar = HSIRplot_NAA$HSIR, pfill = HSIRplot_NAA$f.alt,
                                      Plottitle = paste("Subregion Bever HSI \n",TempSource, sep = ""))  

# Plot HSIR (22 C threshold) faceting by WYT
Plot_SubRegHSIR22 <- PlotsubHSIR(df = HSIRplot_all.BA, xpar = HSIRplot_all.BA$Sub2, ypar = HSIRplot_all.BA$HSIR, pfill = HSIRplot_all.BA$f.alt,
                               Plottitle = paste("Subregion HSI 22C \n",TempSource, sep = ""))
  
# Plot HSIR (24 C threshold) faceting by WYT
Plot_SubRegHSIR24 <- PlotsubHSIR(df = HSIRplot_all.BA, xpar = HSIRplot_all.BA$Sub2, ypar = HSIRplot_all.BA$HSIR_24, pfill = HSIRplot_all.BA$f.alt,
                               Plottitle = paste("Subregion HSI 24C \n",TempSource, sep = ""))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BA Abiotic: plot Delta mean ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## create long df
HSIRplot_long.BA <- HSIRplot_all.BA %>%
  select(!c(Fr.Sal6.mn, Fr.Temp22.mn)) %>%
  select(!Fr.Temp24.mn) %>%
  pivot_longer(cols = c("Sal.mn", "Temp.mn", "MaxSpd.mn", "Pr.Turb12.mn"),
               names_to = "env_var",
               values_to = "mean"
  )

## Plot temperature
PlotTemp_SubregWYT <-HSIRplot_long.BA %>%
  filter(env_var == "Temp.mn") %>%
  ggplot(aes(x=Sub2, y = mean, fill = f.alt)) +
  geom_boxplot() +
  # add horizontal lines for temp thresholds
  geom_hline(yintercept=22, linetype = 'dashed', color = "forestgreen", linewidth = 1) +
  geom_hline(yintercept=24, linetype = 'dashed', color = "forestgreen", linewidth = 1) +
  facet_grid(WYT~.) +
  scale_fill_manual("Alternatives", values = MyColors) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle(paste("Summer-Fall temperature")) + 
  # rename the y axis label
  ylab("Temperature (deg C)") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

## Plot salinity
PlotSal_SubregWYT <-HSIRplot_long.BA %>%
  filter(env_var == "Sal.mn") %>%
  ggplot(aes(x=Sub2, y = mean, fill = f.alt)) +
  geom_boxplot() +
  # add horizontal lines for temp thresholds
  geom_hline(yintercept=6, linetype = 'dashed', color = "forestgreen", linewidth = 1) +
  facet_grid(WYT~.) +
  scale_fill_manual("Alternatives", values = MyColors) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle(paste("Summer-Fall salinity")) + 
  # rename the y axis label
  ylab("Salinity (ppt)") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

## Plot turbidity
### note: turbidity doesn't vary by alt
PlotTurb_SubregWYT <-HSIRplot_long.BA %>%
  filter(env_var == "Pr.Turb12.mn") %>%
  ggplot(aes(x=Sub2, y = mean, fill = f.alt)) +
  geom_boxplot() +
  facet_grid(WYT~.) +
  scale_fill_manual("Alternatives", values = MyColors) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle(paste("Summer-Fall turbidity")) + 
  # rename the y axis label
  ylab("Probability turbidity > 12 NTU") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

## Plot current speed
PlotVeloc_SubregWYT <-HSIRplot_long.BA %>%
  filter(env_var == "MaxSpd.mn") %>%
  ggplot(aes(x=Sub2, y = mean, fill = f.alt)) +
  geom_boxplot() +
  facet_grid(WYT~.) +
  scale_fill_manual("Alternatives", values = MyColors) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle(paste("Summer-Fall current speed")) + 
  # rename the y axis label
  ylab("Current speed") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save BA Plots: full set per temp source -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Delta-wide, Bever
jpeg(paste("HSI_figs_tables/BeverHSIR_Delta_",TempSource,".jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_DeltaBev)
dev.off()

# Delta-wide, 22C
jpeg(paste("HSI_figs_tables/HSIR_Delta_",TempSource,".jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_DeltaHSIR22)
dev.off()

# Delta-wide, 24C
jpeg(paste("HSI_figs_tables/HSIR_24_Delta_",TempSource,".jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_DeltaHSIR24)
dev.off()

# Summer-fall habitat subregions, Bever
jpeg(paste("HSI_figs_tables/BeverHSIR_SFHAarc_",TempSource,".jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_SFHBev)
dev.off()

# Summer-fall habitat subregions, 22C
jpeg(paste("HSI_figs_tables/HSIR_SFHAarc_",TempSource,".jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_SFHHSIR22)
dev.off()

# Summer-fall habitat subregions, 24C
jpeg(paste("HSI_figs_tables/HSIR_24_SFHAarc_",TempSource,".jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_SFHHSIR24)
dev.off()


# subregions, faceted by WYT, Bever et al (no temp)
jpeg(paste("HSI_figs_tables/BeverHSIR_Subregion_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(Plot_SubRegBevHSIR)
dev.off()


# subregions, faceted by WYT, 22C
jpeg(paste("HSI_figs_tables/HSIR_Subregion_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(Plot_SubRegHSIR22)
dev.off()

# subregions, faceted by WYT, 24C
jpeg(paste("HSI_figs_tables/HSIR_24_Subregion_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(Plot_SubRegHSIR24)
dev.off()

# Temperature subregions, faceted by WYT, 24C
jpeg(paste("HSI_figs_tables/Temp_SubregWYT_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(PlotTemp_SubregWYT)
dev.off()

# Salinity subregions, faceted by WYT, 24C
jpeg(paste("HSI_figs_tables/Sal_SubregWYT_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(PlotSal_SubregWYT)
dev.off()

# Turbidity subregions, faceted by WYT, 24C
jpeg(paste("HSI_figs_tables/Turb_SubregWYT_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(PlotTurb_SubregWYT)
dev.off()

# Current speed subregions, faceted by WYT, 24C
jpeg(paste("HSI_figs_tables/Veloc_SubregWYT_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(PlotVeloc_SubregWYT)
dev.off()



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EIS: Set variable levels for plotting HSIR and abiotic conditions for alternatives and water year types -----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Filter out BA alternatives
HSIRplot_all.EIS <- HSIRplot_all %>%
  filter(f.alt != "EXP1") %>%
  filter(f.alt != "EXP3") 

# Set strata levels for plotting
HSIRplot_all.EIS$Sub2 = factor(HSIRplot_all.EIS$Sub2, levels=c("SacR", "Yolo", "SacR_low", "SJR_low", 
                                                             "Confl", "SnMarsh", "NE_SnBay", "NW_SnBay",
                                                             "SE_SnBay", "SW_SnBay", "E_Delta", "S_Delta"))  

# Set alternative levels for plotting
HSIRplot_all.EIS$f.alt = factor(HSIRplot_all.EIS$f.alt, levels=c("NAA", "Alt1", "Alt2wTUCPwoVA", "Alt2woTUCPwoVA",
                                                               "Alt2woTUCPDeltaVA", "Alt2woTUCPAllVA", "Alt3", "Alt4"))

HSIRplot_all.EIS$WYT = factor(HSIRplot_all.EIS$WYT, levels=c("C", "D", "BN", "AN", "W"))

# Set colors for the alternatives

## Nick's full palette
#my_palette=c('#003E51','#007396', '#C69214', '#DDCBA4','#FF671F', '#215732','#4C12A1','#9a3324', "#88CCEE","#AA4499")
## Alts: Alt1, Alt2wTUCPwoVA, Alt2woTUCPAllVA, Alt2woTUCPDeltaVA, Alt2woTUCPwoVA, Alt3, Alt4, EXP1, EXP3, NAA  

## My palette for current alts
MyColors.EIS <- c("#AA4499", "#003E51", "#007396", "#FF671F", "#DDCBA4", "#C69214", "#215732", "#4C12A1")

## assign levels
names(MyColors.EIS) <- levels(HSIRplot_all.EIS$f.alt)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EIS: HSI PLOTTING FUNCTION -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Delta and SHFA subregions combined

PlotHSIR.EIS <- function(df, xpar, ypar, pfill, Plottitle) {
  df %>%
    ggplot(aes(x=xpar, y=ypar, fill=pfill)) +
    geom_boxplot(alpha = 0.7) +
    ggtitle(Plottitle) + 
    # set y axis to between 0 and 1
    ylim(0,1) +
    # rename the y axis label
    ylab("Volume-weighted average HSI") +
    # specify legend title and box colors
    scale_fill_manual("Alternatives", values = MyColors.EIS) +
    # choose the black-and-white theme for graphical display
    theme_bw() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
          axis.text.y = element_text(size = 12))
}


#Individual subregions
PlotsubHSIR.EIS <- function(df, xpar, ypar, pfill, Plottitle) {
  df %>%
    ggplot(aes(x=xpar, y=ypar, fill=pfill)) +
    geom_boxplot(alpha = 0.7) +
    facet_grid(WYT~.) +
    ggtitle(Plottitle) + 
    # set y axis to between 0 and 1
    ylim(0,1) +
    # rename the y axis label
    ylab("Volume-weighted average HSI") +
    # specify legend title and box colors
    scale_fill_manual("Alternatives", values = MyColors.EIS) +
    # choose the black-and-white theme for graphical display
    theme_bw() +
    theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
          axis.text.y = element_text(size = 12))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~
# EIS: HSIR:plot Delta mean ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~

## sum across all subregions for each WYT, date, and alt
DeltaHSIR.EIS <- HSIRplot_all.EIS %>%
  select(c(WYT, f.alt, Sub2, date, BevHSIR, HSIR, HSIR_24)) %>%
  group_by(WYT, f.alt, date) %>%
  summarise(BevHSIR.mn =mean(BevHSIR),
            BevHSIR.sd = sd(BevHSIR),
            HSIR.mn = mean(HSIR),
            HSIR.sd = sd(HSIR),
            HSIR_24.mn = mean(HSIR_24),
            HSIR_24.sd = sd(HSIR_24)) %>%
  ungroup()

# Plot Bever et al HSIR (no temp threshold)
Plot_DeltaBev.EIS <- PlotHSIR.EIS(df = DeltaHSIR.EIS, xpar = DeltaHSIR.EIS$WYT, ypar = DeltaHSIR.EIS$BevHSIR.mn, pfill = DeltaHSIR.EIS$f.alt,
                          Plottitle = paste("Delta-wide Bever HSI \n",TempSource, sep = " "))

# Plot HSIR (22 C threshold)
Plot_DeltaHSIR22.EIS <- PlotHSIR.EIS(df = DeltaHSIR.EIS, xpar = DeltaHSIR.EIS$WYT, ypar = DeltaHSIR.EIS$HSIR.mn, pfill = DeltaHSIR.EIS$f.alt,
                             Plottitle = paste("Delta-wide HSI 22C \n",TempSource, sep = " "))


# Plot HSIR_24 (24 C threshold)
Plot_DeltaHSIR24.EIS <- PlotHSIR.EIS(df = DeltaHSIR.EIS, xpar = DeltaHSIR.EIS$WYT, ypar = DeltaHSIR.EIS$HSIR_24.mn, pfill = DeltaHSIR.EIS$f.alt,
                             Plottitle = paste("Delta-wide HSI 24C \n",TempSource, sep = " "))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EIS: HSIR: plot summer fall habitat mean ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Calculate means and standard deviation for subregions relevant for summer-fall habitat
SFHHSIR.EIS <- HSIRplot_all.EIS %>%
  filter(Subregion == "Confluence"|Subregion == "Lower Sacramento River"|
           Subregion == "NW Suisun"|Subregion == "Suisun Marsh"|Subregion == "Yolo") %>%
  select(c(WYT, f.alt, Sub2, date, BevHSIR, HSIR, HSIR_24)) %>%
  group_by(WYT, f.alt, date) %>%
  summarise(BevHSIR.mn =mean(BevHSIR),
            BevHSIR.sd = sd(BevHSIR),
            HSIR.mn = mean(HSIR),
            HSIR.sd = sd(HSIR),
            HSIR_24.mn = mean(HSIR_24),
            HSIR_24.sd = sd(HSIR_24)) %>%
  ungroup()

# Plot Bever et al. 2016 HSIR (no temp threshold)
Plot_SFHBev.EIS <- PlotHSIR.EIS(df = SFHHSIR.EIS, xpar = SFHHSIR.EIS$WYT, ypar = SFHHSIR.EIS$BevHSIR.mn, pfill = SFHHSIR.EIS$f.alt,
                        Plottitle = paste("Summer-fall habitat subregion Bever HSI \n",TempSource, sep = " "))

# Plot HSIR (22 C threshold)
Plot_SFHHSIR22.EIS <- PlotHSIR.EIS(df = SFHHSIR.EIS, xpar = SFHHSIR.EIS$WYT, ypar = SFHHSIR.EIS$HSIR.mn, pfill = SFHHSIR.EIS$f.alt,
                           Plottitle = paste("Summer-fall habitat subregion HSI 22C \n",TempSource, sep = " "))


# Plot HSIR_24 (24 C threshold)
Plot_SFHHSIR24.EIS <- PlotHSIR.EIS(df = SFHHSIR.EIS, xpar = SFHHSIR.EIS$WYT, ypar = SFHHSIR.EIS$HSIR_24.mn, pfill = SFHHSIR.EIS$f.alt,
                           Plottitle = paste("Summer-fall habitat subregion HSI 24C \n",TempSource, sep = " "))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EIS HSIR: plot all subregions mean ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot Bever et al. (2016) HSIR (no temp threshold) faceting by WYT
Plot_SubRegBevHSIR.EIS <- PlotsubHSIR.EIS(df = HSIRplot_all.EIS, xpar = HSIRplot_all.EIS$Sub2, ypar = HSIRplot_all.EIS$BevHSIR, pfill = HSIRplot_all.EIS$f.alt,
                                  Plottitle = paste("Subregion Bever HSI \n",TempSource, sep = ""))


# Plot HSIR (22 C threshold) faceting by WYT
Plot_SubRegHSIR22.EIS <- PlotsubHSIR.EIS(df = HSIRplot_all.EIS, xpar = HSIRplot_all.EIS$Sub2, ypar = HSIRplot_all.EIS$HSIR, pfill = HSIRplot_all.EIS$f.alt,
                                 Plottitle = paste("Subregion HSI 22C \n",TempSource, sep = ""))

# Plot HSIR (24 C threshold) faceting by WYT
Plot_SubRegHSIR24.EIS <- PlotsubHSIR.EIS(df = HSIRplot_all.EIS, xpar = HSIRplot_all.EIS$Sub2, ypar = HSIRplot_all.EIS$HSIR_24, pfill = HSIRplot_all.EIS$f.alt,
                                 Plottitle = paste("Subregion HSI 24C \n",TempSource, sep = ""))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EIS Abiotic: plot Delta mean ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## create long df
HSIRplot_long.EIS <- HSIRplot_all.EIS %>%
  select(!c(Fr.Sal6.mn, Fr.Temp22.mn)) %>%
  select(!Fr.Temp24.mn) %>%
  pivot_longer(cols = c("Sal.mn", "Temp.mn", "MaxSpd.mn", "Pr.Turb12.mn"),
               names_to = "env_var",
               values_to = "mean"
  )

## Plot temperature
PlotTemp_SubregWYT.EIS <-HSIRplot_long.EIS %>%
  filter(env_var == "Temp.mn") %>%
  ggplot(aes(x=Sub2, y = mean, fill = f.alt)) +
  geom_boxplot() +
  # add horizontal lines for temp thresholds
  geom_hline(yintercept=22, linetype = 'dashed', color = "forestgreen", linewidth = 1) +
  geom_hline(yintercept=24, linetype = 'dashed', color = "forestgreen", linewidth = 1) +
  facet_grid(WYT~.) +
  scale_fill_manual("Alternatives", values = MyColors.EIS) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle(paste("Summer-Fall temperature")) + 
  # rename the y axis label
  ylab("Temperature (deg C)") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

## Plot salinity
PlotSal_SubregWYT.EIS <-HSIRplot_long.EIS %>%
  filter(env_var == "Sal.mn") %>%
  ggplot(aes(x=Sub2, y = mean, fill = f.alt)) +
  geom_boxplot() +
  # add horizontal lines for temp thresholds
  geom_hline(yintercept=6, linetype = 'dashed', color = "forestgreen", linewidth = 1) +
  facet_grid(WYT~.) +
  scale_fill_manual("Alternatives", values = MyColors.EIS) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle(paste("Summer-Fall salinity")) + 
  # rename the y axis label
  ylab("Salinity (ppt)") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

## Plot turbidity
### note: turbidity doesn't vary by alt
PlotTurb_SubregWYT.EIS <-HSIRplot_long.EIS %>%
  filter(env_var == "Pr.Turb12.mn") %>%
  ggplot(aes(x=Sub2, y = mean, fill = f.alt)) +
  geom_boxplot() +
  facet_grid(WYT~.) +
  scale_fill_manual("Alternatives", values = MyColors.EIS) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle(paste("Summer-Fall turbidity")) + 
  # rename the y axis label
  ylab("Probability turbidity > 12 NTU") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

## Plot current speed
PlotVeloc_SubregWYT.EIS <-HSIRplot_long.EIS %>%
  filter(env_var == "MaxSpd.mn") %>%
  ggplot(aes(x=Sub2, y = mean, fill = f.alt)) +
  geom_boxplot() +
  facet_grid(WYT~.) +
  scale_fill_manual("Alternatives", values = MyColors.EIS) +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle(paste("Summer-Fall current speed")) + 
  # rename the y axis label
  ylab("Current speed") +
  # choose the black-and-white theme for graphical display
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save EIS Plots: full set per temp source -----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Delta-wide, Bever
jpeg(paste("HSI_figs_tables/EIS_BeverHSIR_Delta_",TempSource,".jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_DeltaBev.EIS)
dev.off()

# Delta-wide, 22C
jpeg(paste("HSI_figs_tables/EIS_HSIR_Delta_",TempSource,".jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_DeltaHSIR22.EIS)
dev.off()

# Delta-wide, 24C
jpeg(paste("HSI_figs_tables/EIS_HSIR_24_Delta_",TempSource,".jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_DeltaHSIR24.EIS)
dev.off()

# Summer-fall habitat subregions, Bever
jpeg(paste("HSI_figs_tables/EIS_BeverHSIR_SFHAarc_",TempSource,".jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_SFHBev.EIS)
dev.off()

# Summer-fall habitat subregions, 22C
jpeg(paste("HSI_figs_tables/EIS_HSIR_SFHAarc_",TempSource,".jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_SFHHSIR22.EIS)
dev.off()

# Summer-fall habitat subregions, 24C
jpeg(paste("HSI_figs_tables/EIS_HSIR_24_SFHAarc_",TempSource,".jpg"),
     height = 4.5,
     width = 6.5,
     units = "in",
     res = 300)
plot(Plot_SFHHSIR24.EIS)
dev.off()


# subregions, faceted by WYT, Bever et al (no temp)
jpeg(paste("HSI_figs_tables/EIS_BeverHSIR_Subregion_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(Plot_SubRegBevHSIR.EIS)
dev.off()


# subregions, faceted by WYT, 22C
jpeg(paste("HSI_figs_tables/EIS_HSIR_Subregion_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(Plot_SubRegHSIR22.EIS)
dev.off()

# subregions, faceted by WYT, 24C
jpeg(paste("HSI_figs_tables/EIS_HSIR_24_Subregion_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(Plot_SubRegHSIR24.EIS)
dev.off()

# Temperature subregions, faceted by WYT, 24C
jpeg(paste("HSI_figs_tables/EIS_Temp_SubregWYT_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(PlotTemp_SubregWYT.EIS)
dev.off()

# Salinity subregions, faceted by WYT, 24C
jpeg(paste("HSI_figs_tables/EIS_Sal_SubregWYT_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(PlotSal_SubregWYT.EIS)
dev.off()

# Turbidity subregions, faceted by WYT, 24C
jpeg(paste("HSI_figs_tables/EIS_Turb_SubregWYT_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(PlotTurb_SubregWYT.EIS)
dev.off()

# Current speed subregions, faceted by WYT, 24C
jpeg(paste("HSI_figs_tables/EIS_Veloc_SubregWYT_",TempSource,".jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(PlotVeloc_SubregWYT.EIS)
dev.off()



#~~~~~~~~~~~~~~~~~
# Exploratory plot -----
#~~~~~~~~~~~~~~~~~
# Plot bi-weekly salinity for just AN and W WYTs
Sal_ANWet <- HSIRplot_long.BA %>%
  filter(WYT == "W" | WYT == "AN") %>%
  filter(env_var == "Sal.mn") %>%
  filter(Subregion == "Confluence" | Subregion == "NE Suisun" | Subregion == "NW Suisun" | Subregion == "SE Suisun" |
           Subregion == "SW Suisun" | Subregion == "Suisun Marsh") %>%
  mutate(moday = as.Date(paste0(as.character(day(date)), '-',as.character(month(date))), format='%d-%m'))

## Set colors
SalColors <- c("#C69214","#88CCEE")
# c("#9a3324", "#007396",  "#AA4499", "#007396", "#FF671F", "#DDCBA4", "#C69214")

PlotSal_NAA_WYT <- Sal_ANWet %>%
  filter(alt == "NAA") %>%
  ggplot(aes(x=moday, y = mean, color = WYT)) +
  facet_grid(Subregion ~.) +
  geom_point(size = 2) +
  ylab("Salinity (ppt)") +
  scale_color_manual("WYT", values = SalColors) +
  ggtitle(paste("Salinity: Above normal vs Wet water years, NAA")) + 
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

PlotSal_EXP1_WYT <- Sal_ANWet %>%
  filter(alt == "EXP1") %>%
  ggplot(aes(x=moday, y = mean, color = WYT)) +
  facet_grid(Subregion ~.) +
  geom_point(size = 2) +
  ylab("Salinity (ppt)") +
  scale_color_manual("WYT", values = SalColors) +
  ggtitle(paste("Salinity: Above normal vs Wet water years, EXP1")) + 
  theme_bw() +
  theme(axis.title.x=element_blank(), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=12), text = element_text(size = 12), 
        axis.text.y = element_text(size = 12))

# Save plots
jpeg(paste("HSI_figs_tables/Salinity_Wet_AN_NAA.jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(PlotSal_NAA_WYT)
dev.off()

jpeg(paste("HSI_figs_tables/Salinity_Wet_AN_EXP1.jpg"),
     height = 7,
     width = 10,
     units = "in",
     res = 300)
plot(PlotSal_EXP1_WYT)
dev.off()


