#===========================================================================================================#
# 6-CH4 iso.R ####
# Author: Scott Zolkos (sgzolkos@gmail.com) | December 18, 2019
# Background: Figures for stable DIC and CH4 isotopes
#===========================================================================================================#

  # Export both figures as 4" x 4.5" landscape PDF

  # 13C-DIC vs. CH4uM
    ggplot() +
      # Axis limits
        scale_y_continuous(limits=c(-25,0), breaks=seq(-25,0,5)) +
        scale_x_log10(limits=c(0.01,250), breaks=c(0.01,0.1,1,10,100), labels=NotFancy) +
      # Panel themes
        theme_bw() +
        #theme(axis.title.x=element_text(margin=margin(t=5, r=0, b=0, l=0))) +
        #theme(axis.title.y=element_text(margin=margin(t=0, r=5, b=0, l=0))) +
        theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
              panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              text=element_text(size=14)) +
        theme(axis.text.x=element_text(angle=0, hjust=0.5, colour="black")) +
        theme(axis.text.y=element_text(angle=0, hjust=0.5, colour="black")) +
        theme(plot.background=element_rect(fill='white')) +
        theme(panel.background=element_rect(fill='white')) +
        theme(legend.position="none") +
      # Data
        geom_point(data=surf_df, aes(y=d13cDIC, x=CH4uM), color=c("black","red")[surf_df$Burn], shape=c(24,23,21,22,25)[surf_df$LandType], fill=c("purple","green","darkblue","green4","blue")[surf_df$LandType], size=3, stroke=0.6) +
        geom_point(data=pore_df, aes(y=d13cDIC, x=CH4uM), color=c("black","red")[pore_df$Burn], shape=c(3,7)[pore_df$LandType], fill=c("gray20","green4")[pore_df$LandType], size=3, stroke=0.6) +
      # Axis labels
        labs(y=expression(delta^"13"*"C-DIC"*~("  "~"VPDB")), x=expression(CH[4]~(µM))) # % = ‰, but allows you to export in PDF

  # 13C-DIC vs. 13C-CH4
    ggplot() +
      # Axis limits
        scale_y_continuous(limits=c(-25,0), breaks=seq(-25,0,5)) +
        scale_x_continuous(limits=c(-100,-35), breaks=seq(-100,-35,20)) + #, labels=NotFancy
      # Panel themes
        theme_bw() +
        #theme(axis.title.x=element_text(margin=margin(t=5, r=0, b=0, l=0))) +
        #theme(axis.title.y=element_text(margin=margin(t=0, r=5, b=0, l=0))) +
        theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
              panel.grid.minor=element_blank(),
              panel.grid.major=element_blank(),
              text=element_text(size=14)) +
        theme(axis.text.x=element_text(angle=0, hjust=0.5, colour="black")) +
        theme(axis.text.y=element_text(angle=0, hjust=0.5, colour="black")) +
        theme(plot.background=element_rect(fill='white')) +
        theme(panel.background=element_rect(fill='white')) +
        theme(legend.position="none") +
      # End-members (after Campeau et al. 2018 JGR-B Fig.7)
        geom_abline(slope=1, intercept=40, lwd=0.8, lty=2, col="darkgray") + # methane oxidation: ε = 40‰
        geom_abline(slope=1, intercept=55, lwd=0.8, lty=2, col="darkgray") + # acetoclastic pathway: ε = 55‰
        geom_abline(slope=1, intercept=85, lwd=0.8, lty=2, col="darkgray") + # hydrogenotrophic pathway: ε = 85‰
      # Data
        geom_point(data=surf_df, aes(y=d13cDIC, x=d13cCH4), color=c("black","red")[surf_df$Burn], shape=c(24,23,21,22,25)[surf_df$LandType], fill=c("purple","green","darkblue","green4","blue")[surf_df$LandType], size=3, stroke=0.6) +
        geom_point(data=pore_df, aes(y=d13cDIC, x=d13cCH4), color=c("black","red")[pore_df$Burn], shape=c(3,7)[pore_df$LandType], fill=c("gray20","green4")[pore_df$LandType], size=3, stroke=0.6) +
      # Axis labels
        labs(y=expression(delta^"13"*"C-DIC"*~("  "~"VPDB")), x=expression(delta^"13"*"C-CH4"*~("  "~"VPDB"))) # % = ‰, but allows you to export in PDF
    
  # Sample sizes
    # Set variables
      envs_surf <- c("plateau", "fenpond", "lake", "fenchannel", "stream")
      envs_pore <- c("peatplateau", "fen")
      envs_burn <- c("Unburned", "Burned")
    # n: 
      for(i in envs_surf){print(dim(na.omit(subset(surf_df, select=c("d13cDIC","CH4uM"), surf_df$LandType==i))))}
      for(i in envs_pore){print(dim(na.omit(subset(pore_df, select=c("d13cDIC","CH4uM"), pore_df$LandType==i))))}
      for(i in envs_burn){print(dim(na.omit(subset(surf_df, select=c("d13cDIC","CH4uM"), surf_df$Burn==i))))} # Add n for each Burn class from this and next line
      for(i in envs_burn){print(dim(na.omit(subset(pore_df, select=c("d13cDIC","CH4uM"), pore_df$Burn==i))))}
    # n: 
      for(i in envs_surf){print(dim(na.omit(subset(surf_df, select=c("d13cDIC","d13cCH4"), surf_df$LandType==i))))}
      for(i in envs_pore){print(dim(na.omit(subset(pore_df, select=c("d13cDIC","d13cCH4"), pore_df$LandType==i))))} # Add n for each Burn class from this and next line
      for(i in envs_burn){print(dim(na.omit(subset(surf_df, select=c("d13cDIC","d13cCH4"), surf_df$Burn==i))))}
      for(i in envs_burn){print(dim(na.omit(subset(pore_df, select=c("d13cDIC","d13cCH4"), pore_df$Burn==i))))}
        