#===========================================================================================================#
# 2-smry stats.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2019-12-18
# Background: Summary statistics of hydrochemical constituents
#===========================================================================================================#

  # Functions ####

    # Summary statistics for desired parameter (unburned and burned watersheds TOGETHER) (median (Q1-Q3) and/or mean ± SE)
      summary_stats_all <- function(stat, param, rnd){

        # Store landscape types as variable
          landtypes <- levels(surf_df$LandType)
        # for loops
          if(stat=="median"){
            for(i in landtypes){
              vals <- as.numeric(as.matrix(na.omit(subset(surf_df, select=param, surf_df$LandType==i))))
              med <- round(median(vals),rnd)
              q1 <- round(summary(vals),rnd)[[2]] # 1st quartile
              q3 <- round(summary(vals),rnd)[[5]] # 3rd quartile
              print(paste0(i,": ",med," (",q1,"-",q3,")"))
            }
          }
          if(stat=="mean"){
            for(i in landtypes){
              vals <- as.numeric(as.matrix(na.omit(subset(surf_df, select=param, surf_df$LandType==i))))
              avg <- round(mean(vals),rnd)
              standard_error <- round(std.error(vals),rnd)
              print(paste0(i,": ",avg," ± ",standard_error))
            }
          }
          if(stat=="both"){
            for(i in landtypes){
              vals <- as.numeric(as.matrix(na.omit(subset(surf_df, select=param, surf_df$LandType==i))))
              avg <- round(mean(vals),rnd)
              standard_error <- round(std.error(vals),rnd)
              med <- round(median(vals),rnd)
              q1 <- round(summary(vals),rnd)[[2]] # 1st quartile
              q3 <- round(summary(vals),rnd)[[5]] # 3rd quartile
              print(paste0(i,": ",avg," ± ",standard_error,"; ", med," (",q1,"-",q3,")"))
            }
          }
        } # stat = "median", "mean", or "both"
      
    # Summary statistics for desired parameter (unburned and burned watersheds SEPARATE) (median (Q1-Q3) and/or mean ± SE)
      summary_stats <- function(stat, water_type, param){
        
        # Subset dataframe
          if(water_type=="surface" & stat=="mean"){
            sum_stats_df <- surf_df
            df_names <- c("Parameter","Burn","Mean ± SE",rep("",4))
          }
          if(water_type=="surface" & stat=="median"){
            sum_stats_df <- surf_df
            df_names <- c("Parameter","Burn","Median (Q1-Q3)",rep("",4))
          }
          if(water_type=="pore" & stat=="mean"){
            sum_stats_df <- pore_df
            df_names <- c("Parameter","Burn","Mean ± SE",rep("",1))
          }
          if(water_type=="pore" & stat=="median"){
            sum_stats_df <- pore_df
            df_names <- c("Parameter","Burn","Median (Q1-Q3)",rep("",1))
          }

        # Create dataframe to write to, run for loop
          sum_stats <- as.data.frame(matrix(ncol=length(df_names), nrow=3))
          names(sum_stats) <- df_names
          j=1
          for(i in levels(sum_stats_df$LandType)){
            # Subset data
              if(water_type=="surface"){
                unburned <- as.numeric(as.matrix(na.omit(subset(sum_stats_df, select=param, sum_stats_df$LandType==i & sum_stats_df$Burn=="Unburned"))))
                burned <- as.numeric(as.matrix(na.omit(subset(sum_stats_df, select=param, sum_stats_df$LandType==i & sum_stats_df$Burn=="Burned"))))
              }
            # Subset data
              if(water_type=="pore"){
                unburned <- as.numeric(as.matrix(na.omit(subset(sum_stats_df, select=param, sum_stats_df$LandType==i & sum_stats_df$BurnHx=="Unburned"))))
                burned <- as.numeric(as.matrix(na.omit(subset(sum_stats_df, select=param, sum_stats_df$LandType==i & sum_stats_df$BurnHx=="Burned"))))
              }
            # Summary stats: median
              if(stat=="median"){
                unburned_median <- round(median(unburned),2)
                burned_median <- round(median(burned),2)
                unburned_q1 <- round(summary(unburned),3)[[2]] # 1st quartile
                unburned_q3 <- round(summary(unburned),3)[[5]] # 3rd quartile
                burned_q1 <- round(summary(burned),3)[[2]] # 1st quartile
                burned_q3 <- round(summary(burned),3)[[5]] # 3rd quartile
              }
            # Summary stats: mean
              if(stat=="mean"){
                unburned_mean <- round(mean(unburned),2)
                burned_mean <- round(mean(burned),2)
                unburned_se <- round(std.error(unburned),3)
                burned_se <- round(std.error(burned),3)
              }
            # Store summary stats in df
              sum_stats[1,1] <- ""
              sum_stats[2,1] <- param
              sum_stats[3,1] <- ""
              sum_stats[1,2] <- ""
              sum_stats[2,2] <- "Unburned"
              sum_stats[3,2] <- "Burned"
              sum_stats[1,j+2] <- i
              if(stat=="median"){
                sum_stats[2,j+2] <- paste0(unburned_median, " (",unburned_q1,"-",unburned_q3,")")
                sum_stats[3,j+2] <- paste0(burned_median, " (",burned_q1,"-",burned_q3,")")
              }
              if(stat=="mean"){
                sum_stats[2,j+2] <- paste0(unburned_mean," ± ",unburned_se)
                sum_stats[3,j+2] <- paste0(burned_mean," ± ",burned_se)
              }
              j <- j+1
          }
          
        # Return dataframe
          sum_stats <- return(sum_stats)
      }
      
      
  # Statistics ####
      
    # Summary statistics- unburned and burned watersheds TOGETHER (median (Q1-Q3) and/or mean ± SE)
      summary_stats_all("mean","dexcess",2)

    # Summary statistics- unburned and burned watersheds SEPARATE (median (Q1-Q3) and/or mean ± SE)
      summary_stats("mean", "surface", "DOCuM")
      
    # Summary statistics for surface water parameters
      surf_sum_stats_df <- rbind(summary_stats("mean", "surface", "Temp"),
                            summary_stats("mean", "surface", "Cond"),
                            summary_stats("mean", "surface", "pH"),
                            summary_stats("mean", "surface", "DOpcnt"),
                            summary_stats("mean", "surface", "DOCuM"),
                            summary_stats("mean", "surface", "NH4uM"),
                            summary_stats("mean", "surface", "NO3uM"),
                            summary_stats("mean", "surface", "PO4uM"),
                            summary_stats("mean", "surface", "CO2uM"),
                            summary_stats("mean", "surface", "CH4uM"),
                            summary_stats("mean", "surface", "SUVA"),
                            summary_stats("mean", "surface", "d13cDIC"),
                            summary_stats("mean", "surface", "d13cCH4"),
                            summary_stats("mean", "surface", "dexcess"),
                            summary_stats("mean", "surface", "CO2flux"),
                            summary_stats("mean", "surface", "CH4flux"))
      #write.csv(surf_sum_stats_df, "/Users/szolkos/Desktop/surface_hydrochem_summary_stats_df.csv", row.names=F)
      
    # Summary statistics for pore water parameters
      pore_sum_stats_df <- rbind(summary_stats("mean", "pore", "Temp"),
                            summary_stats("mean", "pore", "Cond"),
                            summary_stats("mean", "pore", "pH"),
                            summary_stats("mean", "pore", "DOpcnt"),
                            summary_stats("mean", "pore", "DOCuM"),
                            summary_stats("mean", "pore", "NH4uM"),
                            summary_stats("mean", "pore", "NO3uM"),
                            summary_stats("mean", "pore", "PO4uM"),
                            summary_stats("mean", "pore", "CO2uM"),
                            summary_stats("mean", "pore", "CH4uM"),
                            summary_stats("mean", "pore", "SUVA"),
                            summary_stats("mean", "pore", "d13cDIC"),
                            summary_stats("mean", "pore", "d13cCH4"),
                            summary_stats("mean", "pore", "dexcess"))
      #write.csv(pore_sum_stats_df, "/Users/szolkos/Desktop/pore_hydrochem_summary_stats_df.csv", row.names=F)
      
      
      
  # OTHER ####
      
    # Median (IQR) or Mean ± SE by landscape type for each hydrochemical parameter ####
        
      # Run function for all parameters, except those noted, rbind into df
        SumStat <- "mean" # "mean" or "median"
        WaterType <- "pore" # "surface" or "pore"
        {hydrochem_sum_stats <- rbind(summary_stats(SumStat,WaterType,"Temp"),
                            summary_stats(SumStat,WaterType,"Cond"),
                            #summary_stats(SumStat,WaterType,"pH"), # Run manually for pore water
                            summary_stats(SumStat,WaterType,"DOpcnt"),
                            summary_stats(SumStat,WaterType,"DOCuM"),
                            summary_stats(SumStat,WaterType,"NH4uM"),
                            summary_stats(SumStat,WaterType,"NO3uM"),
                            summary_stats(SumStat,WaterType,"PO4uM"),
                            summary_stats(SumStat,WaterType,"CO2uM"),
                            summary_stats(SumStat,WaterType,"CH4uM"),
                            #summary_stats(SumStat,WaterType,"CO2flux"), # Run manually for surface water; no mmnts for pore water
                            #summary_stats(SumStat,WaterType,"CH4flux"), # Run manually for surface water; no mmnts for pore water
                            summary_stats(SumStat,WaterType,"CN"),
                            summary_stats(SumStat,WaterType,"dexcess"),
                            #summary_stats(SumStat,WaterType,"Area"), # Run manually for pore water
                            #summary_stats(SumStat,WaterType,"Slope"), # Run manually for pore water
                            summary_stats(SumStat,WaterType,"Elev"),
                            #summary_stats(SumStat,WaterType,"NDVI"), # Run manually for pore water
                            #summary_stats(SumStat,WaterType,"NDWI"), # Run manually for pore water
                            #summary_stats(SumStat,WaterType,"DICuM"), # Run manually for surface and pore water
                            summary_stats(SumStat,WaterType,"TDNuM"),
                            summary_stats(SumStat,WaterType,"SUVA"),
                            summary_stats(SumStat,WaterType,"d13cDIC") # Run manually for surface water
                            #summary_stats(SumStat,WaterType,"d13cCH4") # Run manually for pore water
                            )} # KEEP
        
      # Optional: Export df to csv
        #write.csv(hydrochem_sum_stats, paste0("/Users/szolkos/Desktop/PolarisAquatics/",WaterType,"_summary_stats_",SumStat,".csv"), row.names=F)
        
      # Run function for noted parameters (where n ≤ 1 for at least one landscape type); get values for land type where n ≤ 1
      ## NOTE: for statistical comparisons, use code below for Wilcoxon Rank Sum Test or Welch's t-test
        # Surface water
          sw_param <- "CO2flux"
          lt <- "plateau" # plateau, lake, fenpond, fenchannel, stream
          summary(subset(surf_df, select=sw_param, surf_df$LandType==lt & surf_df$Burn=="Unburned"))
          summary(subset(surf_df, select=sw_param, surf_df$LandType==lt & surf_df$Burn=="Burned"))
        # Pore water
          pw_param <- "d13cCH4"
          lt <- "fen"
          mean(as.numeric(as.matrix(na.omit(subset(pore_df, select=pw_param, pore_df$LandType==lt & pore_df$Burn=="Unburned")))))
          std.error(as.numeric(as.matrix(na.omit(subset(pore_df, select=pw_param, pore_df$LandType==lt & pore_df$Burn=="Unburned")))))
          mean(as.numeric(as.matrix(na.omit(subset(pore_df, select=pw_param, pore_df$LandType==lt & pore_df$Burn=="Burned")))))
          std.error(as.numeric(as.matrix(na.omit(subset(pore_df, select=pw_param, pore_df$LandType==lt & pore_df$Burn=="Burned")))))
      # Check values
        summary(subset(surf_df, select="Area", surf_df$LandType=="peatplateau" & surf_df$Burn=="Unburned"))
        summary(subset(surf_df, select="Slope", surf_df$LandType=="peatplateau" & surf_df$Burn=="Unburned"))
        summary(subset(surf_df, select="Elev", surf_df$LandType=="peatplateau" & surf_df$Burn=="Unburned"))
        summary(subset(surf_df, select="NDVI", surf_df$LandType=="peatplateau" & surf_df$Burn=="Unburned"))
        summary(subset(surf_df, select="NDWI", surf_df$LandType=="peatplateau" & surf_df$Burn=="Unburned"))
        summary(subset(surf_df, select="Area", surf_df$LandType=="pond" & surf_df$Burn=="Burned"))
    