# Wildfire and landscape controls on tundra hydrochemistry
## Introduction
Source code for the manuscript: Physiographic Controls and Wildfire Effects on Aquatic Biogeochemistry in Tundra of the Yukon-Kuskokwim Delta, Alaska. DOI: https://zenodo.org/badge/latestdoi/235470761

## Authors
- [Scott Zolkos](https://www.researchgate.net/profile/Scott-Zolkos)
- Erin MacDonald
- Jacqueline K.Y. Hung
- John D. Schade
- Sarah Ludwig
- Paul J. Mann
- Rachael Treharne
- Susan Natali

## Data
### The following data associated with this manuscript can be downloaded from the [Arctic Data Center](https://arcticdata.io/)
 
## Scripts
### Data processing
#### *Note: These scripts process the data archived via the link above, which was compiled as detailed in the Methods of this manuscript. Users of these scripts will first need to update working directory pathways within scripts as needed.*  
*1_intro.R*:  Read and compile data for analyses and figures.  
*2_smry_stats.R*:  Summary statistics of hydrochemical constituents.  
*3_permANOVA.R*: Permutational ANOVA (permANOVA) (non-parametric) to test for differences in surface water chemistry (i) within same aquatic environment having different burn history and (ii) between aquatic environments with same burn history.  
*4_boxplots.R*: Boxplots of hydrochemical consituent values across landscape types.  
*5_d13C_DIC.R*: d13C-DIC vs. pH.  
*6_CH4_iso.R*: Figures for stable DIC and CH4 isotopes.    
*7_multiyear.R*: Plots of desired hydrochemical constituents, by sampling year & burn history.  
*8_H2O_iso.R*: Plot d2H-H2O vs. d18O-H2O.  
*9_wtrshd_attrb.R*: Create plots of watershed attributes, e.g. mean elevation.  

## Packages
### *For data processing, plotting, and analyses*
- Hmisc
- gdata
- vegan
- ecodist
- pvclust
- ggfortify
- lubridate
- reshape2
- ggplot2
- RColorBrewer
- corrplot
- plotrix
- cowplot
- foreign
- tidyverse
- dplyr
- ggfortify
- lme4
- lmerTest
- lmPerm
- ggpubr
