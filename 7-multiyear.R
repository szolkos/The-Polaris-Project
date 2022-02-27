#===========================================================================================================#
# multiyear.R ####
# Author: Scott Zolkos (sgzolkos@gmail.com) | January 27, 2022
# Background: 
#===========================================================================================================#

  # Packages
    library(ggpubr) # for ggarrange()

  # Functions
    # y-axis
      NotFancy <- function(l) {
    l <- format(l, scientific = FALSE)
    parse(text=l)
  }
    
    # Boxplots of desired hydrochemical constituents, by sampling year & burn history
      multiyear <- function(envmt, param){
        
        # Data subset
          if(envmt=="lake" & param=="Cond"){years <- c("2016","2017","2018","2019"); ylims=c(1,300); ybreaks=c(1,10,100); yscale <- scale_y_log10(limits=ylims, breaks=ybreaks, labels=NotFancy)}
          if(envmt=="lake" & param=="DOpcnt"){years <- c("2016","2017","2018","2019"); ylims=c(0,150); ybreaks=seq(0,140,20); yscale <- scale_y_continuous(limits=ylims, breaks=ybreaks, labels=NotFancy)}
          if(envmt=="lake" & param=="NH4uM"){years <- c("2016","2017","2018","2019"); ylims=c(0.04,100); ybreaks=c(0.1,1,10,100); yscale <- scale_y_log10(limits=ylims, breaks=ybreaks, labels=NotFancy)}
          if(envmt=="lake" & param=="CO2uM"){years <- c("2017","2018","2019"); ylims=c(1,1000); ybreaks=c(1,10,100,1000); yscale <- scale_y_log10(limits=ylims, breaks=ybreaks, labels=NotFancy)}
          if(envmt=="fenpond" & param=="pH"){years <- c("2016","2017","2018"); ylims=c(4,7); ybreaks=seq(4,7,1); yscale <- scale_y_continuous(limits=ylims, breaks=ybreaks, labels=NotFancy)}
          if(envmt=="fenpond" & param=="DOCuM"){years <- c("2016","2017","2018"); ylims=c(100,10000); ybreaks=c(100,1000,10000); yscale <- scale_y_log10(limits=ylims, breaks=ybreaks, labels=NotFancy)}
          if(envmt=="fenpond" & param=="NH4uM"){years <- c("2016","2017","2018"); ylims=c(0.01,100); ybreaks=c(0.01,0.1,1,10,100); yscale <- scale_y_log10(limits=ylims, breaks=ybreaks, labels=NotFancy)}
          if(envmt=="fenpond" & param=="CO2uM"){years <- c("2017","2018"); ylims=c(1,2000); ybreaks=c(1,10,100,1000); yscale <- scale_y_log10(limits=ylims, breaks=ybreaks, labels=NotFancy)}
          #if(envmt=="fenpond" & param=="PO4uM"){years <- c("2016","2017","2018"); ylims=c(0.006,1); ybreaks=c(0.01,0.1,1); yscale <- scale_y_log10(limits=ylims, breaks=ybreaks, labels=NotFancy)}
        # Plot labels
          if(param == "Cond"){ylab <- expression(Sp.~conductance~(µS~cm^-1))}
          if(param == "DOpcnt"){ylab <- "D.O. (% saturation)"}
          if(param == "pH"){ylab <- "pH (pH units)"}
          if(param == "DOCuM"){ylab <- "DOC (µM)"}
          if(param == "NH4uM"){ylab <- expression(NH[4]^"+"~(µM))}
          if(param == "CO2uM"){ylab <- expression(CO[2]~(µM))}
          #if(param == "PO4uM"){ylab <- expression(PO[4]^"3-"~(µM))}
          
        # Subset data
        
          df <- droplevels(na.omit(subset(surf_df, select=c("LandType", "Year", param, "Burn"), surf_df$LandType==envmt & surf_df$Year %in% years)))
          names(df) <- c("LandType", "Year","param", "Burn")
          df$param[df$param==0] <- NA
          
        # Plot
          multiyear <- ggplot(df, aes(y=param, x=Year, color=Burn)) +
            # Outliers
              geom_boxplot(outlier.shape=NA) +
            # X-axis, colors
              scale_x_discrete(labels=levels(df$Year)) +
              scale_fill_manual(values="grey40") +
              scale_color_manual(name="Burn", values=c("black","red")) +
            # Themes
              theme_bw() +
              theme(plot.margin=unit(c(0.05,0.1,0.05,0.05), "in"),
                panel.grid.minor=element_blank(),
                panel.grid.major=element_blank(),
                panel.background=element_rect(fill='white'),
                panel.border=element_rect(colour="black", fill=NA, size=1),
                plot.background=element_rect(colour="white", size=1),
                text=element_text(size=13),
                axis.text.y=element_text(size=12, angle=0, hjust=0.5, colour="black"),
                axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0)),
                axis.text.x=element_text(size=12, angle=0, hjust=0.5, colour="black"),
                #axis.title.x=element_text(margin=margin(t=0, r=0, b=0, l=0)),
                axis.title.x=element_blank(),
                legend.position="none") +
            # Points
              geom_point(aes(y=param, x=Year), position=position_jitterdodge(), alpha=0.4) +
            # Labels
              labs(y=ylab, x="") +
            # Y-axis scales
              yscale
          
        plot(multiyear)     
        return(multiyear)
      }
    
    
  # Store as grob
    p1 <- multiyear("lake", "Cond")
    p2 <- multiyear("lake", "DOpcnt")
    p3 <- multiyear("lake", "NH4uM")
    p4 <- multiyear("lake", "CO2uM")
    p5 <- multiyear("fenpond", "pH")
    p6 <- multiyear("fenpond", "DOCuM")
    p7 <- multiyear("fenpond", "NH4uM")
    p8 <- multiyear("fenpond", "CO2uM")
    ggarrange(p1, p5, p2, p6, p3, p7, p4, p8, ncol=2, nrow=4)
    
  
  # Export
    #out <- "/Users/szolkos/Documents/Research/Projects/The Polaris Project/Manuscripts/Drafts/"
    ggsave(filename=paste0(out,"multiyear ",envmt," ",param," ",Sys.Date(),".pdf"), plot=multiyear(envmt, param), height=4, width=5)
    
  # Statistics
    # Before permANOVA, explore w/ Wilcox test
      year <- "2016"
      envmt <- "lake"
      param <- "Cond"
      burned <- as.numeric(as.matrix(droplevels(na.omit(subset(surf_df, select=param, surf_df$Year==year & surf_df$LandType==envmt & surf_df$Burn=="Burned")))))
      unburned <- as.numeric(as.matrix(droplevels(na.omit(subset(surf_df, select=param, surf_df$Year==year & surf_df$LandType==envmt & surf_df$Burn=="Unburned")))))
      wilcox.test(x=unburned, y=burned, alternative="two.sided")
      round(wilcox.test(x=unburned, y=burned, alternative="two.sided")$p.value,3)

  # Permutational ANOVA (permANOVA)
    # Interactions of interest
      interacs_my <- c("2016:Burned-2016:Unburned",
                    "2017:Burned-2017:Unburned",
                    "2018:Burned-2018:Unburned",
                    "2019:Burned-2019:Unburned",
                    "2017:Unburned-2016:Unburned",
                    "2018:Unburned-2016:Unburned",
                    "2018:Unburned-2017:Unburned",
                    "2019:Unburned-2016:Unburned",
                    "2019:Unburned-2017:Unburned",
                    "2019:Unburned-2018:Unburned",
                    "2017:Burned-2016:Burned",
                    "2018:Burned-2016:Burned",
                    "2018:Burned-2017:Burned",
                    "2019:Burned-2016:Burned",
                    "2019:Burned-2017:Burned",
                    "2019:Burned-2018:Burned")
    # Subset surface water data
      my_df <- surf_df
      my_df$CO2uM[my_df$CO2uM == 0] <- NA # Omit 0 values where CO2uM could not be determined
      my_df$CH4uM[my_df$CH4uM == 0] <- NA # Omit 0 values where CH4uM could not be determined
      my_df <- droplevels(subset(my_df, select=c("Year","LandType","Burn","Cond","DOpcnt","NH4uM","CO2uM","pH","DOCuM"), my_df$LandType=="lake" | my_df$LandType=="fenpond"))
      yrs <- c("2016","2017","2018","2019")
      my_df <- droplevels(subset(my_df, my_df$Year %in% yrs))
    # Run permANOVA
      envmt <- "fenpond"
      pn_df <- droplevels(subset(my_df, my_df$LandType == envmt))
      my_pn <- aovp( log(CO2uM) ~ Year*Burn, pn_df, seqs=T) # transform (ln): Cond, DOpcnt, NH4uM, CO2uM, pH, DOCuM
    # Visual inspection of residual distributions- as needed, transform y and re-run aovp
      par(mar=c(4.5,4.5,1,1), mfrow=c(2,2)); plot(my_pn)
    # Test permANOVA
      anova(my_pn)
    # Results: Year ~ Burn history interaction
      interac_yb <- as.data.frame(TukeyHSD(my_pn)[3])[interacs_my,]
    # Store as dataframe
      interac_mydf <- interac_yb
      names(interac_mydf) <- c("diff","lwr","upr","p_adj")
      interac_mydf$p_adj <- as.numeric(format(interac_mydf$p_adj, scientific=F, digits=5))
      interac_mydf$p_adj <- round(interac_mydf$p_adj,5)
      interac_mydf

            
  # Other ####
      
    # Results
      # Year only
        interac_y <- as.data.frame(TukeyHSD(my_pn)$Year[interacs_my,])
      # Burn history only
        interac_b <- as.data.frame(TukeyHSD(my_pn)$Burn[interacs_my,])
        
      # For significant results, perform post-hoc Tukey HSD
        TukeyHSD(my_pn) # Show all interactions
        TukeyHSD(my_pn)$Year # Year only
        TukeyHSD(my_pn)$Burn # Burn history only
        TukeyHSD(my_pn)[3] # Year ~ Burn history interaction
        
      #interac_mydf$Comparison <- c(rep("burn",5), rep("env_unburned",10), rep("env_burned",10))
      #interac_mydf <- select(interac_mydf, Comparison, lwr, upr, p_adj)
        