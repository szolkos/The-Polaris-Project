#===========================================================================================================#
# 9-H2O iso.R ####
# Author: Scott Zolkos (sgzolkos@gmail.com) | December 18, 2019
# Background: 
#===========================================================================================================#

  # Export as 5"x6" PDF

  # d2H-H2O vs. d18O-H2O
    ggplot() +
      # Axis limits
        scale_y_continuous(limits=c(-105,-55), breaks=seq(-100,-50,10)) +
        scale_x_continuous(limits=c(-15,-4), breaks=seq(-15,5,5)) +
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
      # End-members
        geom_abline(slope=8, intercept=10, lwd=0.9, lty=1, col="darkgray") + # GMWL
        #geom_abline(slope=5.87, intercept=-25.21, lwd=1.2, lty=1, col="black") + # LMWL
      # Data
        geom_point(data=surf_df, aes(y=d2hH2O, x=d18oH2O), color=c("black","red")[surf_df$Burn], shape=c(24,23,21,22,25)[surf_df$LandType], fill=c("purple","green","darkblue","green4","blue")[surf_df$LandType], size=3, stroke=0.6) +
      # Axis labels
        labs(y=expression(delta^"2"*H*"-"*H[2]*O~("  "~"VSMOW")), x=expression(delta^"18"*O*"-"*H[2]*O~("  "~"VSMOW"))) # % = ‰, but allows you to export in PDF
    # n
      dim(na.omit(subset(surf_df, select=c("d2hH2O","d18oH2O"), surf_df$LandType=="stream"))) # plateau, lake, fenpond, fenchannel, stream
      dim(na.omit(subset(surf_df, select=c("d2hH2O","d18oH2O"), surf_df$Burn=="Unburned"))) # Unburned, Burned
  
  # Sample sizes
    # Set variables
      envs_surf <- c("plateau", "fenpond", "lake", "fenchannel", "stream")
      envs_burn <- c("Unburned", "Burned")
    # n: 
      for(i in envs_surf){print(dim(na.omit(subset(surf_df, select=c("d2hH2O","d18oH2O"), surf_df$LandType==i))))}
      for(i in envs_burn){print(dim(na.omit(subset(surf_df, select=c("d2hH2O","d18oH2O"), surf_df$Burn==i))))} # Add n for each Burn class from this and next line
      