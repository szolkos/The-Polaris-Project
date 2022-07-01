#===========================================================================================================#
# boxplots.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2019-12-18
# Background: Boxplots of hydrochemical consituent values across landscape types
#===========================================================================================================#

  # Functions ####
  
    # Loop through parameters, make individual boxplots
      bp_land_type <- function(parameter, save){
        
        # Set graphical parameters
          fll <- c("gray100","gray80","gray60","gray40","gray20")
          my.labels <- paste0(c("Plateau","Fen","Lake","Fen","Stream"),"\n",c("pond","pond","","channel",""))
  
        # Set y and x-axis labels, and axis scale limits and breaks
          xlab <- "Landscape category"
          if(parameter=="Temp"){ylab <- "Temperature (˚C)"; limits <- c(4,27); breaks <- seq(5,25,5)}
          if(parameter=="Cond"){ylab <- expression(Conductivity~(µS~cm^-1)); limits <- c(8,300); breaks <- c(10,100,300)}
          if(parameter=="pH"){ylab <- "pH (pH units)"; limits <- c(3.8,8); breaks <- seq(4,8,1)}
          if(parameter=="DOpcnt"){ylab <- "D.O. (% saturation)"; limits <- c(0,150); breaks <- seq(0,150,50)}
          if(parameter=="DOCuM"){ylab <- "DOC (µM)"; limits <- c(0,5000); breaks <- seq(0,5000,1000)}
          if(parameter=="DICuM"){ylab <- "DIC (µM)"; limits <- c(1,6000); breaks <- c(1,10,100,1000,6000)} 
          if(parameter=="TDNuM"){ylab <- "TDN (µM)"; limits <- c(7,200); breaks <- c(10,100)}
          if(parameter=="DONuM"){ylab <- "DON (µM)"; limits <- c(0.5,100); breaks <- c(1,10,100)}
          if(parameter=="NH4uM"){ylab <- expression(NH[4]^"+"~(µM)); limits <- c(0.01,100); breaks <- c(0.01,0.1,1,10,100)}
          if(parameter=="NO3uM"){ylab <- expression(NO[3]^"-"~(µM)); limits <- c(0.005,40); breaks <- c(0.01,0.1,1,10)}
          if(parameter=="PO4uM"){ylab <- expression(PO[4]^"3-"~(µM)); limits <- c(0,0.71); breaks <- seq(0,0.7,0.1)}
          if(parameter=="CO2uM"){ylab <- expression(CO[2]~(µM)); limits <- c(3,2000); breaks <- c(1,10,100,1000)}
          if(parameter=="pCO2"){ylab <- expression(italic(p)*CO[2]~(µatm)); limits <- c(60,30000); breaks <- c(100,1000,10000,30000)}
          if(parameter=="CH4uM"){ylab <- expression(CH[4]~(µM)); limits <- c(0.01,280); breaks <- c(0.1,1,10,100)}
          if(parameter=="pCH4"){ylab <- expression(italic(p)*CH[4]~(µatm)); limits <- c(10,50000); breaks <- c(0.1,1,10,100,1000,10000,50000)}
          if(parameter=="SUVA"){ylab <- expression(SUVA[254]~(L~mgC^-1~m^-1)); limits <- c(0,20); breaks <- seq(0,20,5)}
          if(parameter=="CN"){ylab <- "DOC:DON (µM:µM)"; limits <- c(0,80); breaks <- seq(0,80,20)}
          if(parameter=="d13cDIC"){ylab <- expression(delta^13*"C-DIC (‰VPDB)"); limits <- c(-25,2); breaks <- seq(-25,0,5)}
          if(parameter=="d13cCH4"){ylab <- expression(delta^13*"C-CH"[4]~"(‰VPDB)"); limits <- c(-70,-35); breaks <- seq(-70,-40,10)}
          if(parameter=="dexcess"){ylab <- "d-excess (‰)"; limits <- c(-22,15); breaks <- seq(-20,10,10)}
          if(parameter=="NDVI"){ylab <- "NDVI"; limits <- c(0.25,0.55); breaks <- seq(0.3,0.5,0.05)}
          if(parameter=="NDWI"){ylab <- "NDWI"; limits <- c(-0.76,-0.55); breaks <- seq(-0.75,-0.55,0.05)}
          if(parameter=="Elev"){ylab <- "Elevation (m asl)"; limits <- c(10,50); breaks <- seq(10,50,10)}
          #if(parameter==""){ylab <- expression()}
        
        # Subset data
          df <- droplevels(subset(surf_df, select=c("LandType","Burn",parameter)))
          names(df) <- c("LandType","Burn","y")
        
        # Apply constants, as needed
          if(parameter=="CO2uM" | parameter=="pCO2" | parameter=="CH4uM" | parameter=="pCH4"){df$y <- df$y + 0.01}
        
        # Plot it
        ## Continuous scale
          if(parameter=="Temp" | parameter=="pH" | parameter=="DOpcnt" | parameter=="DOCuM" | parameter=="PO4uM" | parameter=="d13cDIC" | parameter=="d13cCH4" | parameter=="dexcess" | parameter=="SUVA" | parameter=="CN" | parameter=="NDVI" | parameter=="NDWI" | parameter=="Elev")
            {
            df_bp <- ggplot(df, aes(y=y, x=LandType, fill=LandType, color=Burn)) +
              geom_boxplot(outlier.shape=NA) +
              scale_y_continuous(limits=limits, breaks=breaks, labels=NotFancy) +
              scale_fill_manual(name="LandType", values=fll) +
              scale_color_manual(name="Burn", values=c("black","red")) +
              theme_bw() +
              theme(plot.margin=unit(c(0.05,0.1,0.05,0.05), "in"),
                    panel.grid.minor=element_blank(),
                    panel.grid.major=element_blank(),
                    plot.background=element_rect(colour="white", size=1),
                    panel.border=element_rect(colour="black", fill=NA, size=1),
                    text=element_text(size=13)) +
              theme(axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0))) +
              theme(axis.text.y=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
              theme(axis.title.x=element_blank()) +
              theme(axis.text.x=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
              theme(plot.background=element_rect(fill='white')) +
              theme(panel.background=element_rect(fill='white')) +
              theme(legend.position="none") +
              labs(y=ylab, x=xlab) +
            scale_x_discrete(labels=my.labels) +
            geom_point(aes(y=y, x=LandType), position=position_jitterdodge(), alpha=0.4)
          }
        ## Log scale
          if(parameter=="Cond" | parameter=="DICuM" | parameter=="NH4uM" | parameter=="NO3uM" | parameter=="CO2uM" | parameter=="pCO2" | parameter=="CH4uM" | parameter=="pCH4" | parameter=="TDNuM" | parameter=="DONuM")
            {
            df_bp <- ggplot(df, aes(y=y, x=LandType, fill=LandType, color=Burn)) +
              geom_boxplot(outlier.shape=NA) +
              scale_y_log10(limits=limits, breaks=breaks, labels=NotFancy) +
              scale_fill_manual(name="LandType", values=fll) +
              scale_color_manual(name="Burn", values=c("black","red")) +
              theme_bw() +
              theme(plot.margin=unit(c(0.05,0.1,0.05,0.05), "in"),
                    panel.grid.minor=element_blank(),
                    panel.grid.major=element_blank(),
                    plot.background=element_rect(colour="white", size=1),
                    panel.border=element_rect(colour="black", fill=NA, size=1),
                    text=element_text(size=13)) +
              theme(axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0))) +
              theme(axis.text.y=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
              theme(axis.title.x=element_blank()) +
              theme(axis.text.x=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
              theme(plot.background=element_rect(fill='white')) +
              theme(panel.background=element_rect(fill='white')) +
              theme(legend.position="none") +
              labs(y=ylab, x=xlab) +
              scale_x_discrete(labels=my.labels) +
              geom_point(aes(y=y, x=LandType), position=position_jitterdodge(), alpha=0.4)
            }
        ## Plot
          print(df_bp)
          return(df_bp)
        
        # Optionally save output
          if(save=="yes"){
            ggsave(paste0("/Users/szolkos/Documents/Research/Projects/The Polaris Project/Manuscripts/Drafts/2022-01/Figures/Fig2/",parameter,"_",Sys.Date(),".pdf"), df_bp, device="pdf", width=7, height=5)
        }
        
      }
     
      
  # Explore hydrochemical parameters
      
    # Individual
      bp_land_type("Temp", "no")
      
    # Multiple
      # Store parameters
        #chem_vars <- c("Temp","Cond","pH","DOpcnt","DOCuM","DICuM","NH4uM","NO3uM","PO4uM","CO2uM","CH4uM","d13cDIC","d13cCH4","dexcess") # TDN, SUVA
        chem_vars <- c("pH","DOpcnt","DOCuM","PO4uM","NH4uM","NO3uM","CO2uM","dexcess")
      # Loop through and plot
        for(i in chem_vars){bp_land_type(parameter=i, save="yes")}
        
    # Store as grob
      p1 <- bp_land_type("pH", "no")
      p2 <- bp_land_type("DOpcnt", "no")
      p3 <- bp_land_type("DOCuM", "no")
      p4 <- bp_land_type("PO4uM", "no")
      p5 <- bp_land_type("NH4uM", "no")
      p6 <- bp_land_type("NO3uM", "no")
      p7 <- bp_land_type("CO2uM", "no")
      p8 <- bp_land_type("dexcess", "no")
      ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2, nrow=4) # widths=c(2,2), heights=rep(0.05,8)
      