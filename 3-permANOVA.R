#===========================================================================================================#
# 3-permANOVA.R ####
# Author: Scott Zolkos (sgzolkos@gmail.com) | December 18, 2019
# Background: Permutational ANOVA (permANOVA) (non-parametric) to test for differences in surface water 
#             chemistry (i) within same aquatic environment having different burn history and (ii) between aquatic 
#             environments with same burn history
#===========================================================================================================#

  # Surface water
    # Interactions of interest
      interacs <- c("plateau:Burned-plateau:Unburned",
      "fenpond:Burned-fenpond:Unburned",
      "lake:Burned-lake:Unburned",
      "fenchannel:Burned-fenchannel:Unburned",
      "stream:Burned-stream:Unburned",
      "lake:Unburned-plateau:Unburned",
      "fenpond:Unburned-plateau:Unburned",
      "lake:Unburned-fenpond:Unburned",
      "fenchannel:Unburned-plateau:Unburned",
      "fenchannel:Unburned-lake:Unburned",
      "fenchannel:Unburned-fenpond:Unburned",
      "stream:Unburned-plateau:Unburned",
      "stream:Unburned-lake:Unburned",
      "stream:Unburned-fenpond:Unburned",
      "stream:Unburned-fenchannel:Unburned",
      "lake:Burned-plateau:Burned",
      "fenpond:Burned-plateau:Burned",
      "lake:Burned-fenpond:Burned",
      "fenchannel:Burned-plateau:Burned",
      "fenchannel:Burned-lake:Burned",
      "fenchannel:Burned-fenpond:Burned",
      "stream:Burned-plateau:Burned",
      "stream:Burned-lake:Burned",
      "stream:Burned-fenpond:Burned",
      "stream:Burned-fenchannel:Burned")
    # Subset surface water data
      pn_df <- surf_df
      pn_df$CO2uM[pn_df$CO2uM == 0] <- NA # Omit 0 values where CO2uM could not be determined
      pn_df$CH4uM[pn_df$CH4uM == 0] <- NA # Omit 0 values where CH4uM could not be determined
      pn_df$CO2flux[pn_df$CO2flux <= 0] <- NA # Omit 0 values where CO2uM could not be determined
      pn_df$CH4flux[pn_df$CH4flux <= 0] <- NA # Omit 0 values where CH4uM could not be determined
    # Run permANOVA
      pn <- aovp( log(CH4flux) ~ LandType*Burn, pn_df, seqs=T) # Set hydrochemical parameter here
    # Visual inspection of residual distributions- as needed, transform y and re-run aovp
      par(mar=c(4.5,4.5,1,1), mfrow=c(2,2)); plot(pn)
        # transform (ln): Cond, DOCuM, NH4uM, NO3uM, CO2uM, CH4uM, SUVA
    # Test permANOVA
      anova(pn)
    # For significant results, perform post-hoc Tukey HSD
      TukeyHSD(pn) # All interactions
      TukeyHSD(pn)$LandType # Landscape type only
      TukeyHSD(pn)$Burn # Burn history only
      TukeyHSD(pn)[3] # Landscape type ~ Burn history
  # Output results
    # Landscape type only
      #interac_lt <- as.data.frame(TukeyHSD(pn)$LandType[SPECIFY INTERACTIONS,])
    # Burn history only
      #interac_bh <- as.data.frame(TukeyHSD(pn)$Burn[SPECIFY INTERACTIONS,])
    # Landscape type ~ Burn history interaction
      interac_ix <- as.data.frame(TukeyHSD(pn)[3])[interacs,]
    # Store as dataframe
      interac_df <- interac_ix
      names(interac_df) <- c("diff","lwr","upr","p_adj")
      interac_df$p_adj <- as.numeric(format(interac_df$p_adj, scientific=F, digits=5))
      interac_df$p_adj <- round(interac_df$p_adj,5)
      interac_df$Comparison <- c(rep("burn",5), rep("env_unburned",10), rep("env_burned",10))
      interac_df <- select(interac_df, Comparison, lwr, upr, p_adj)
      interac_df
        
  # Pore water
    # Subset surface water data
      pn_df <- pore_df
      pn_df$CO2uM[pn_df$CO2uM == 0] <- NA # Omit 0 values where CO2uM could not be determined
      pn_df$CH4uM[pn_df$CH4uM == 0] <- NA # Omit 0 values where CH4uM could not be determined
    # Run permANOVA
      pn <- aovp( (dexcess) ~ LandscapeCategory*Burn, pn_df, seqs=T) # Set hydrochemical parameter here
    # Visual inspection of residual distributions- as needed, transform y and re-run aovp
      par(mar=c(4.5,4.5,1,1), mfrow=c(2,2)); plot(pn)
      # transform (ln): Temp, Cond, DOCuM, NH4uM, NO3uM, PO4uM
    # Test permANOVA
      anova(pn)
    # For significant results, perform post-hoc Tukey HSD
      TukeyHSD(pn) # Show all interactions
    
        
        
  # Other ####
        
    # Wilcoxon Rank Sum Test (non-parametric)
        
      # Info: (https://www.youtube.com/watch?v=ZIDICaAGmxg; https://data.library.virginia.edu/the-wilcoxon-rank-sum-test/)
        
      # Check variance
        var.test((unburned), (burned))
        var(unburned); var(burned); var(unburned)/var(burned); var.test(unburned, burned)$p.value
        par(mar=c(4.5,4.5,1,1)); boxplot(unburned, burned, col=c("gray","red"))
        #hist(unburned, col=rgb(1,0,0,0.5)); hist(burned, col=rgb(0,0,1,0.5), breaks=30, add=T); box()
      # Scale data as needed to obtain equal variance
        unburned_scaled <- as.numeric(log(unburned)); burned_scaled <- as.numeric(log(burned))
        var.test(unburned_scaled, burned_scaled)
        var(unburned_scaled); var(burned_scaled); var(unburned_scaled)/var(burned_scaled); var.test(unburned_scaled, burned_scaled)$p.value
        par(mar=c(4.5,4.5,1,1)); boxplot(unburned_scaled, burned_scaled, col=c("gray","red"))
        #hist(scale(unburned), col=rgb(1,0,0,0.5)); hist(scale(burned), col=rgb(0,0,1,0.5), add=T); box()
      # Run test
        # See here for "cannot compute exact p-value with ties" (occurs when n<50): http://courses.atlas.illinois.edu/spring2016/STAT/STAT200/RProgramming/NonParametricStats.html
        w_test <- wilcox.test(x=unburned, y=burned, alternative="two.sided") # ls(w_test)
        w_test <- wilcox.test(x=unburned_scaled, y=burned_scaled, alternative="two.sided", conf.int=T); ls(w_test)
        w_test
        w_test$statistic; round(w_test$p.value,3)