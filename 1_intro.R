#===========================================================================================================#
# 1-intro.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2019-12-18
# Background: Read and compile data for analyses and figures
#===========================================================================================================#

  # Packages ####
    library(Hmisc)
    library(gdata) # to bind columns with different number of rows
    library(vegan) # e.g. for rda function
    library(ecodist) # for NMDS
    library(pvclust)
    library(ggfortify) # so ggplot can read PCA objects
    library(lubridate) # To convert from date to Julian day
    library(reshape2)
    library(ggplot2)
    library(RColorBrewer) # for plotting with R color brewer
    library(corrplot) # to graphically display correlation matrices
    library(plotrix)
    library(cowplot)
    library(foreign)
    library(tidyverse)
    library(dplyr)
    library(ggfortify) # so ggplot can read PCA objects
    library(lme4) # for LMER (lmer)
    library(lmerTest) # for LMER (lmer)
    library(lmPerm) # for permutational ANOVA
    library(ggpubr) # for ggarrange()
  
  # Functions #####

    NotFancy <- function(l) {
      l <- format(l, scientific = FALSE)
      parse(text=l)
    }
  
  # Set working directory #####
    
    dir <- "/Users/szolkos/Documents/Research/Projects/The Polaris Project/Analyses/Data/Data4R/"
    setwd(dir)

  
  # Data preparation ####
    
    # Import data
      ykd <- read.csv(paste0(dir, "PolarisYKD_aquatic_2020_04_01.csv"), header=T); dim(ykd)
    # Omit duplicate for SampleID Polaris_2019_27 (it has less data)
      subset(ykd, ykd$SampleID=="Polaris_2019_27"); ykd <- droplevels(subset(ykd, ykd$SampleID!="Polaris_2019_27" | ykd$Atm!=0.9950)); dim(ykd)
    # Read in and merge mean burn percent
      burn_pcnt <- read.csv(paste0(dir, "elev_pcnt2015burn_2020-04-30.csv"), header=T); burn_pcnt <- droplevels(subset(burn_pcnt, select=c("SampleID","BurnPcnt")))
      ykd <- merge(x=ykd, y=burn_pcnt, by="SampleID", all.x=T)
    # Read in and merge mean elevation from ArcticDEM
      mean_elev <- read.csv(paste0(dir, "YKD_aquatics_mean_elev_2020-05-04.csv"), header=T); mean_elev <- droplevels(subset(mean_elev, select=c("SampleID","mean")))
      names(mean_elev) <- c("SampleID","Elev")
      ykd <- merge(x=ykd, y=mean_elev, by="SampleID", all.x=T)
    # Atmospheric CO2 and CH4
      brw_atm_CO2 <- read.csv(paste0(dir, "Barrow_daily_mean_CO2.csv"), header=T)
      brw_atm_CO2 <- droplevels(subset(brw_atm_CO2, select=c("year","month","day","value"), brw_atm_CO2$value > 0))
      brw_atm_CH4 <- read.csv(paste0(dir, "Barrow_daily_mean_CH4.csv"), header=T)
      brw_atm_CH4 <- droplevels(subset(brw_atm_CH4, select=c("year","month","day","value"), brw_atm_CH4$value > 0))
      
    # Optionally export data
      #write.csv(ykd, "/Users/szolkos/Desktop/ykd_w_burn_and_elev.csv", row.names=F)
      
      sub_expo <- droplevels(subset(ykd, select=c("SampleID","Kit","LatDD","LonDD","WaterType","LandscapeCategory","BurnPcnt","Burn")))
      write.csv(sub_expo, "/Users/szolkos/Desktop/ykd_burns.csv", row.names=F)
      
    # Add sampling date
      ykd$Date <- as.character(ykd$Date)
    # Add sampling year, month, and day
      # Year
        #ykd$Year <- paste0("20", substr(ykd$Date,7,8)[ykd$Date!="<NA>"])
        #ykd$Year[ykd$Year=="NA"] <- NA
        ykd$Year <- substr(ykd$Date,1,4)[ykd$Date!="<NA>"]
        ykd$Year <- as.character(ykd$Year)
      # Month
        #ykd$Month <- substr(ykd$Date,2,2)[ykd$Date!="<NA>"]
        ykd$Month <- substr(ykd$Date,6,7)[ykd$Date!="<NA>"]
        ykd$Month <- as.numeric(ykd$Month)
      # Day
        #ykd$Day <- substr(ykd$Date,4,5)[ykd$Date!="<NA>"]
        ykd$Day <- substr(ykd$Date,9,10)[ykd$Date!="<NA>"]
        ykd$Day <- as.numeric(ykd$Day)
  
    # Convert concentrations to µM
      ykd$DOCuM <- ((ykd$DOCmgL/1000)/12.0107)*1000000
      ykd$TDNuM <- ((ykd$TDNmgL/1000)/14.0067)*1000000
      ykd$DONuM <- ((ykd$DONmgL/1000)/14.0067)*1000000
      ykd$NH4uM <- ((ykd$NH4ugL/1000000)/14.0067)*1000000
      ykd$NO3uM <- ((ykd$NO3ugL/1000000)/14.0067)*1000000
      ykd$PO4uM <- ((ykd$PO4ugL/1000000)/30.973762)*1000000
      #ykd$SiuM <- ((ykd$SimgL/1000)/28.0855)*1000000
      
    # Convert CO2 and CH4 flux from mgC/m2/d to µmol/m2/d and nmol/m2/d
      # summary(ykd$CO2flux/1000/12.0107*1000000/86400) # test
      ykd$CO2flux <- ykd$CO2flux/1000/12.0107*1000000/86400
      ykd$CH4flux <- ykd$CH4flux/1000/12.0107*1000000000/86400
  
    # Calculate d-excess, following Turner et al. (2014, DOI: 10.1002/ppp.1802): d-excess = d2H-8*d18O
      ykd$dexcess <- ykd$d2hH2O-(8*ykd$d18oH2O)
  
    # Omit 1972 burns
      ykd <- droplevels(subset(ykd, ykd$BurnYear!="1972"))
      
    # Add burn history (BurnHx)
      # Based on burn %, for surface waters
        ykd$Burn <- ykd$BurnPcnt
        ykd$Burn[ykd$Burn <= 10] <- "Unburned"
        ykd$Burn[ykd$Burn != "Unburned"] <- "Burned"
        ykd$Burn[ykd$Burn==NA] <- NA
        ykd$Burn <- factor(ykd$Burn , levels=c("Unburned","Burned"))
        summary(ykd$Burn)
      # Based on field observations, for pore waters
        ykd$BurnHx <- as.vector(ykd$BurnYear)
        ykd$BurnHx[ykd$BurnHx=="2015"] <- "Burned"
        #ykd$BurnHx[ykd$BurnHx=="1972"] <- "Burned"
        ykd$BurnHx[ykd$BurnHx=="Control"] <- "Unburned"
        ykd$BurnHx[ykd$BurnHx==NA] <- NA
        ykd$BurnHx <- factor(ykd$BurnHx , levels=c("Unburned","Burned"))
        ykd$Burn[ykd$WaterType=="pore"] <- ykd$BurnHx[ykd$WaterType=="pore"]
        summary(ykd$BurnHx)
        
    # Subset desired parameters
      ykd <- droplevels(subset(ykd, select=c(
        "SampleID","Kit","Date","Year","Month","Day","LatDD","LonDD","LandscapeCategory","WaterType","FenDetail","BurnHx","Burn","BurnYear",
        "Atm","Temp","Cond","pH","DOpcnt","DOmgL","DOCuM","DICuM","TDNuM","DONuM","NH4uM","NO3uM","PO4uM","CN","SR","SUVA",
        "CO2atm","CO2ppm","CO2uM","pCO2","CO2excess","CO2flux","CH4atm","CH4ppm","CH4uM","pCH4","CH4excess","CH4flux",
        "d13cDIC","d13cCH4","d2hH2O","d18oH2O","dexcess",
        "Area","Slope","Elev","NDVI","NDWI","BurnPcnt")))
      
    # Subset data for fens, lakes, peat plateaus
      ykd <- droplevels(subset(ykd, ykd$LandscapeCategory=="peat plateau" | ykd$LandscapeCategory=="pond" | ykd$LandscapeCategory=="fen" | ykd$LandscapeCategory=="stream" | ykd$LandscapeCategory=="lake"))
    
    # Set classes
      ykd$Year <- as.numeric(ykd$Year)
      ykd$LandscapeCategory <- factor(ykd$LandscapeCategory, levels=c("peat plateau","pond","lake","fen","stream"))
      summary(ykd); dim(ykd)
      
      
  # QA/QC ####
      
    # Check BurnPcnt, Burn, BurnHx
      par(mar=c(4.5,4.5,1,1))
      boxplot(ykd$BurnPcnt~ykd$BurnHx, lwd=1.2, col="darkgray", ylab="Burn % (2015 fires)", xlab="Burn history")
      
      droplevels(subset(ykd, select=c("SampleID","Kit","Date","LandscapeCategory","WaterType","FenDetail","BurnYear","BurnHx","BurnPcnt"), ykd$Burn=="Unburned" & ykd$BurnPcnt>0))
      droplevels(subset(ykd, select=c("SampleID","Kit","Date","LandscapeCategory","WaterType","FenDetail","BurnYear","BurnHx","BurnPcnt"), ykd$Burn=="Burned" & ykd$BurnPcnt==0))
      
    # ID where 'Burned' sites have 0% burn
      droplevels(subset(ykd, select=c("SampleID","BurnHx","BurnPcnt"), ykd$BurnHx=="Burned" & ykd$BurnPcnt==0))
    ## Optional: re-classify site BurnHx based on watershed burn history
      #ykd$BurnHx[ykd$SampleID=="Polaris_2017_47"] <- "Unburned"
    
    # ID where 'Unburned' areas have >0% burn
      droplevels(subset(ykd, select=c("SampleID","BurnHx","BurnPcnt"), ykd$BurnHx=="Unburned" & ykd$BurnPcnt>0))
    ## Optional: re-classify site BurnHx based on watershed burn history
      #ykd$BurnHx[ykd$BurnPcnt>0] <- "Burned"
    
      
  # Subset surface water hydrochemistry ####
    # Subset data
      ykd_surf <- droplevels(subset(ykd, ykd$WaterType=="surface"))
    # Store sampling Year as factor
      ykd_surf$Year <- factor(ykd_surf$Year)
    # Store BurnYear as factor
      ykd_surf$BurnYear <- factor(ykd_surf$BurnYear, levels=c("Control","2015"))
    # Store BurnHx as factor
      #ykd_surf$BurnHx <- factor(ykd_surf$BurnHx, levels=c("Burned","Unburned"))
    # Combine 'peat plateau' (i.e. peat plateau pools) and 'pond' (i.e. peat plateau ponds) into 'plateau'
      ykd_surf$LandscapeCategory <- as.character(ykd_surf$LandscapeCategory)
      ykd_surf$LandscapeCategory[ykd_surf$LandscapeCategory=="peat plateau"] <- "plateau"
      ykd_surf$LandscapeCategory[ykd_surf$LandscapeCategory=="pond"] <- "plateau"
    # Store LandscapeCategory as factor
      ykd_surf$LandscapeCategory <- factor(ykd_surf$LandscapeCategory)#, levels=c("fen","stream","pond","lake"))
      summary(ykd_surf$LandscapeCategory)
    # Store FenDetail as factor
      ykd_surf$FenDetail <- as.character(ykd_surf$FenDetail)
      ykd_surf$FenDetail[is.na(ykd_surf$FenDetail)] <- "other"
      ykd_surf$FenDetail[ykd_surf$FenDetail=="neither"] <- "other"
      ykd_surf$FenDetail <- factor(ykd_surf$FenDetail)
    # Summarize
      summary(ykd_surf); dim(ykd_surf)
    # Subset "peat plateau", "pond", "fenpond", "channel, "lake", "stream"
      #peatplats <- droplevels(subset(ykd_surf, ykd_surf$LandscapeCategory=="peat plateau")); peatplats$LandType <- "peatplateau"
      #ponds <- droplevels(subset(ykd_surf, ykd_surf$LandscapeCategory=="pond")); ponds$LandType <- "pond"
      peatplats <- droplevels(subset(ykd_surf, ykd_surf$LandscapeCategory=="plateau"))
      fenponds <- droplevels(subset(ykd_surf, ykd_surf$LandscapeCategory=="fen" & ykd_surf$FenDetail=="fenpond"))
      lakes <- droplevels(subset(ykd_surf, ykd_surf$LandscapeCategory=="lake"))
      fenchans <- droplevels(subset(ykd_surf, ykd_surf$LandscapeCategory=="fen" & ykd_surf$FenDetail=="channel"))
      streams <- droplevels(subset(ykd_surf, ykd_surf$LandscapeCategory=="stream"))
    # Add categorical landscape variable to each df
      peatplats$LandType <- "plateau"
      fenponds$LandType <- "fenpond"
      lakes$LandType <- "lake"
      fenchans$LandType <- "fenchannel"
      streams$LandType <- "stream"
    # Bind dataframes
      surf_df <- rbind(peatplats,fenponds,lakes,fenchans,streams)
      surf_df$LandType <- factor(surf_df$LandType, levels=c("plateau","fenpond","lake","fenchannel","stream"))
      surf_df$Burn <- factor(surf_df$Burn , levels=c("Unburned","Burned"))
      #surf_df$BurnHx <- factor(surf_df$BurnHx , levels=c("Unburned","Burned"))
      surf_df$Year <- factor(surf_df$Year , levels=c("2015","2016","2017","2018","2019"))
    # Omit negative pCO2 values
      surf_df$pCO2[surf_df$pCO2<=0] <- NA
    # Add negligible value to pCO2 and pCH4 0 mmnts, for log transformation
      surf_df$pCO2 <- round(surf_df$pCO2+0.001,4); surf_df$pCH4 <- round(surf_df$pCH4+0.001,4)
    # Summary of df
      summary(surf_df); dim(surf_df)
    # Explore
      boxplot(surf_df$Elev~surf_df$LandType, lwd=1.2, col=c("gray100","gray80","gray60","gray40","gray20","gray5"))
      
    # Subset lakes
      lakes_2018 <- droplevels(subset(surf_df, surf_df$Year=="2018" & surf_df$LandscapeCategory=="lake", select=c("SampleID", "Kit", "Date", "Month", "Day", "LatDD", "LonDD", "BurnHx", "BurnPcnt", "BurnYear", "DOCuM", "SUVA", "SR")))
    # Filter out samples from project 'Msw1'
      lakes_2018 <- lakes_2018 %>% filter(!str_detect(Kit, "Msw1"))
    # Set data types
      lakes_2018$Kit <- as.numeric(lakes_2018$Kit); lakes_2018$Date <- date(lakes_2018$Date)
    # Export data
      #write.csv(lakes_2018, "/Users/szolkos/Desktop/Polaris_lakes_2018.csv", row.names=F)
      
      
  # Subset pore water hydrochemistry ####
    # Subset data
      ykd_pore <- droplevels(subset(ykd, ykd$WaterType=="pore"))
    # Store burn history as factor
      ykd_pore$Burn <- factor(ykd_pore$Burn, levels=c("Unburned","Burned"))
    # Store sampling Year as factor
      ykd_pore$Year <- factor(ykd_pore$Year)
    # Store BurnYear as factor
      ykd_pore$BurnYear <- factor(ykd_pore$BurnYear, levels=c("Control","2015"))
    # Store LandscapeCategory as factor
      ykd_pore$LandscapeCategory <- factor(ykd_pore$LandscapeCategory)
    # Store FenDetail as factor
      ykd_pore$FenDetail <- as.character(ykd_pore$FenDetail)
      ykd_pore$FenDetail[is.na(ykd_pore$FenDetail)] <- "other"
      ykd_pore$FenDetail <- factor(ykd_pore$FenDetail)
    # Summarize
      summary(ykd_pore); dim(ykd_pore)
    # Subset "peat plateau", "pond", "fenpond", "channel, "lake", "stream"; add categorical landscape variable to each df
      peatplat <- droplevels(subset(ykd_pore, ykd_pore$LandscapeCategory=="peat plateau")); peatplat$LandType <- "peatplateau"
      fen <- droplevels(subset(ykd_pore, ykd_pore$LandscapeCategory=="fen")); fen$LandType <- "fen"
    # Bind dataframes
      pore_df <- rbind(peatplat,fen)
      pore_df$LandType <- factor(pore_df$LandType, levels=c("peatplateau","fen"))
      pore_df$Burn <- factor(pore_df$Burn , levels=c("Unburned","Burned"))
      pore_df$Year <- factor(pore_df$Year , levels=c("2017","2018","2019"))
    # Omit negative pCO2 values
      pore_df$pCO2[pore_df$pCO2<=0] <- NA
    # Add negligible value to pCO2 and pCH4 0 mmnts, for log transformation
      pore_df$pCO2 <- round(pore_df$pCO2+0.001,4); pore_df$pCH4 <- round(pore_df$pCH4+0.001,4)
    # Summary of df
      summary(pore_df); dim(pore_df)
      

  # OTHER ####
      
    fen_pore_ch4 <- droplevels(subset(pore_df, select=c("Year","BurnHx","DOCuM","CH4uM"), pore_df$LandscapeCategory=="fen"))
    par(mar=c(7,4.5,1,1)); boxplot(fen_pore_ch4$CH4uM~fen_pore_ch4$BurnHx*fen_pore_ch4$Year, las=2)
    par(mar=c(7,4.5,1,1)); boxplot(fen_pore_ch4$DOCuM~fen_pore_ch4$BurnHx*fen_pore_ch4$Year, las=2)
    