# DS_HSI
Summarizes Delta Smelt habitat suitability index values from SCHISM 2-week averages

Scripts are designed to be run in numerical order

01_AppK_SCHISM_pre.R
 - Takes the SCHISM postprocessed files: biweekly environmental conditions and HSI values
    - these data files are not housed on GitHub due to their size; can be acquired by request
 - Before running,specify:
    - Alternative 
    - Year used for water temperature data (wet and above normal years require designation; 
      other water year types leave blank)
 - Removes all rows that don't have data (i.e., areas in each region that are not submerged)
 - Binds the individual biweeky files together into one alternative * water year type file
 - Saves files to SCHISM_combo folder

02_AppK_SCHISM_summzHSI.R
 - For each alternative * water year type
 - Before running, specify:
    - Alternative
    - Year used for water temperature data (see above)
    - Temperature data year folder for saving output files
 - Calculates the depth-averaged habitat suitability indices (HSIRs) by subregion for each 
   biweekly time period for:
    - BevHSIR: Bever et al. (2016) HSI cacluations
    - HSIR: Bever et al. (2016) HSI calculation with 0,1 temperature threshold of 22C
    - HSIR_24: Bever et al. (2016) HSI calculation with 0,1 temerature threshold of 24C
 - Calculates mean environmental variables
 - Produces scatter plots for QAQC
- Saves files to the HSI_clean folder and, for wet year, temperature source subfolder
    
03_AppK_HSI_TablesFigs_v2
 - For each alternative * water year type
 - Before running, specify:
    - Alternative
    - Year used for water temperature data (see above)
    - Temperature data year folder for saving output files
 - Calculates the mean and standard deviation (st dev) HSIRs for each subregion
 - Creates a summary table for each alternative that includes mean  and st dev values for: 
   each subregion; entire Delta; Summer-Fall Habitat action subregions, combined
 - Creates figures showing medians, means, and st dev for each subregion for the following:
    - BevHSIR, HSIR, and HSIR_24
    - salinity, temperature, turbidity, and current speed
 - Saves files to the HSI_clean or HSI_figs_tables folders and, for wet year, temperature 
   source subfolder

04_AppK_CollateHSI_Tables.R
 - All alternative * water year types
 - Before running, specify:
    - Year used for water temperature data (see above)
 - Calculates percent difference between BA alternatives and the NAA
 - Creates the following tables
    - Bever HSIR that includes all alternatives and water year types
        - means + st dev
        - means and percent difference
    - HSIR for all alternatives and water year types
        - means + st dev
        - means and percent difference
 - Saves files to the HSI_figs_tables folder
 
05_AppK_CollateHSI_Figs.R
 - All alternative * water year types
 - Before running, specify:
    - Year used for water temperature data (see above)
 - Creates boxplots of Bever HSIR, HSIR , and HSIR_24 by alternative and water year type by:
    - Subregions
    - Whole Delta
    - Summer-Fall Habitat Action subregions, combined
 - Creates boxplots of environmental variables by alternative, water year type, and subregion:
    - Temperature
    - Salinity
    - Turbidity
    - Current Speed
 - Saves files to the HSI_figs_tables folder
 
06_AppK_HSI_Uncertainty.R
 - All alternatives for wet year temperature and HSI means by the different temperature sources
 - Creates figures for discussing uncertainties in air temperature source and temperature
   threshold approach
 - Saves files to the HSI_figs_tables folder
