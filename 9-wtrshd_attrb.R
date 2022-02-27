#===========================================================================================================#
# wtrshd_attrb.R ####
# Author: Scott Zolkos (sgzolkos@gmail.com) | January 27, 2022
# Background: 
#===========================================================================================================#

  # Read data

  

  # Summary stats
    round(tapply(surf_df$Area, surf_df$LandType, FUN=mean, na.rm=T),2)
    round(tapply(surf_df$Elev, surf_df$LandType, FUN=mean, na.rm=T),2)
    round(tapply(surf_df$Slope, surf_df$LandType, FUN=mean, na.rm=T),2)
    round(tapply(surf_df$NDVI, surf_df$LandType, FUN=mean, na.rm=T),2)
    round(tapply(surf_df$BurnPcnt, surf_df$LandType, FUN=mean, na.rm=T),2)
    
    mean(na.omit(subset(surf_df, select="dexcess", surf_df$LandType=="fenpond" | surf_df$LandType=="plateau")$dexcess))

    cols <- c("purple", "green3", "darkblue","darkgreen","blue") # "peat plateau","pond","lake","fen","stream"
    
    par(mar=c(4.5,4.5,1,1))
    boxplot( (surf_df$Area) ~ surf_df$LandType, col=cols, log="y", y=expression(Watershed~area~(km^2)))
    my.labels <- paste0(c("Plateau","Fen","Lake","Fen","Stream"),"\n",c("","pond","","channel",""))
    
  # Plot
    
    # Scatterplot
      plot( log(surf_df$Area) ~ (surf_df$Elev), pch=21, cex=1.6, col="black", bg=cols[surf_df$LandType])
      
    # Boxplot for desired parameter
      ggplot(data=surf_df, aes(y=Area, x=LandType, fill=LandType)) +
          geom_boxplot(outlier.shape=21) +
        # Scales
          #scale_y_continuous(lim=c(10,50)) + # Elevation
          scale_y_log10(, labels=NotFancy) + # Watershed area
        # Themes
          theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
            panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            panel.background=element_rect(fill='white'),
            panel.border=element_rect(colour="black", fill=NA, size=1),
            plot.background=element_rect(colour="white", size=1),
            text=element_text(size=19),
            axis.text.y=element_text(angle=0, hjust=0.5, colour="black"),
            axis.title.y.left=element_text(margin=margin(t=0, r=10, b=0, l=0)),
            axis.title.y.right=element_text(margin=margin(t=0, r=0, b=0, l=10)),
            axis.text.x=element_text(angle=0, hjust=0.5, colour="black"),
            axis.title.x=element_text(margin=margin(t=0, r=0, b=0, l=0)),
            legend.position="none") +
        # Data
          scale_fill_manual(values=cols) +
          #geom_point(aes(y=Area, x=LandType), position=position_jitterdodge(), alpha=0.4) +
        # Labels
          scale_x_discrete(labels=my.labels) +
          #labs(y="Elevation (m)", x=NULL)
          labs(y=expression(Watershed~area~(km^2)), x=NULL)

    # Density ditribution for desired parameter
      ggplot(data=surf_df, aes(x=Elev, fill=LandType)) +
        # Scales
          scale_x_continuous(limits=c(5,50), breaks=seq(0,50,10)) + # Elevation
          #scale_x_continuous(limits=c(0.2,0.6), breaks=seq(0.2,0.6,0.1)) + # NDVI
          #scale_x_continuous(limits=c(-0.77,-0.54), breaks=seq(-0.75,-0.55,0.05)) + # NDWI
          #scale_x_continuous(limits=c(0,7), breaks=seq(0,7,1)) + # NDWI
        # Themes
          # theme_void() + # <<== TURN ON OR OFF FOR EMPTY PLOTTING SPACE
          theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
                #axis.title.x=element_blank() + # <<== TURN ON OR OFF FOR EMPTY PLOTTING SPACE
                #axis.ticks.x=element_blank() + # <<== TURN ON OR OFF FOR EMPTY PLOTTING SPACE
                #axis.text.x=element_blank() + # <<== TURN ON OR OFF FOR EMPTY PLOTTING SPACE
                panel.grid.minor=element_blank(),
                panel.grid.major=element_blank(),
                panel.background=element_rect(fill='white'),
                panel.border=element_rect(colour="black", fill=NA, size=1),
                plot.background=element_rect(colour="white", size=1),
                text=element_text(size=19),
                axis.text.y=element_text(angle=0, hjust=0.5, colour="black"),
                axis.title.y.left=element_text(margin=margin(t=0, r=10, b=0, l=0)),
                axis.title.y.right=element_text(margin=margin(t=0, r=0, b=0, l=10)),
                axis.text.x=element_text(angle=0, hjust=0.5, colour="black"),
                axis.title.x=element_text(margin=margin(t=0, r=0, b=0, l=0)),
                legend.position="none") +
        # Data
          scale_fill_manual(values=cols) +
          labs(y="density", x="Elevation (m)") +
          geom_density(data=surf_df, aes(x=Elev, y=), alpha=0.8)