#===========================================================================================================#
# 5-d13C-DIC.R ####
# Author: Scott Zolkos | sgzolkos@gmail.com
# Version: 2019-12-18
# Background: d13C-DIC vs. pH
#===========================================================================================================#

  # Determine n for each land type and burn Hx, for surface and pore waters
    nrow(na.omit(subset(surf_df, select=c("pH","d13cDIC","Burn"), surf_df$LandType=="stream")))
    nrow(na.omit(subset(pore_df, select=c("pH","d13cDIC","Burn"), pore_df$LandType=="peatplateau")))
    summary(na.omit(subset(surf_df, select=c("pH","d13cDIC","Burn")))$Burn)
    summary(na.omit(subset(pore_df, select=c("pH","d13cDIC","Burn")))$Burn)
    
    
  # Upper end-member
    up.em.x <- c(4,5,6,7,8)
    up.em.y <- c(-7.97,-7.70,-5.63,-0.30,1.95)
    up.em  <- as.data.frame(cbind(up.em.x, up.em.y))
    names(up.em) <- c("x","y")
  # Store splines
    up.em.spline <- as.data.frame(cbind(spline(x=up.em$x, y=up.em$y)$x, spline(x=up.em$x, y=up.em$y)$y))
    names(up.em.spline) <- c("x","y")
  
  # Lower end-member
    lw.em.x <- c(4,5,6,7,8)
    lw.em.y <- c(-26.02,-25.72,-23.64,-19.71,-18.48)
    lw.em  <- as.data.frame(cbind(lw.em.x, lw.em.y))
    names(lw.em) <- c("x","y")
  # Store splines
    lw.em.spline <- as.data.frame(cbind(spline(x=lw.em$x, y=lw.em$y)$x, spline(x=lw.em$x, y=lw.em$y)$y))
    names(lw.em.spline) <- c("x","y")

  ggplot() +
    # Set axis limits
      scale_x_continuous(limits=c(4,8)) +
      scale_y_continuous(limits=c(-26.2,5), breaks=seq(-24,4,4)) +
    # Set panel theme
      theme_bw() +
      theme(plot.margin=unit(c(0.1,0.1,0.1,0.1), "in"),
            panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            text=element_text(size=14)) +
      theme(axis.text.x=element_text(angle=0, hjust=0.5, colour="black")) +
      theme(axis.text.y=element_text(angle=0, hjust=0.5, colour="black")) +
      theme(plot.background=element_rect(fill='white')) +
      theme(panel.background=element_rect(fill='white')) +
      theme(legend.position="none") +
    # Add end-members
      geom_vline(xintercept=7.33, lwd=0.8, lty=1, col="darkgray") + # Reference line where HCO3:DIC > 0.9 (pH = 7.3 @ 26.6˚C, pH = 7.48 @ 4.4˚C)
      geom_smooth(data=up.em.spline, aes(x=x, y=y), method=loess, se=F, col="darkgray", lwd=1.2) +
      geom_smooth(data=lw.em.spline, aes(x=x, y=y), method=loess, se=F, col="darkgray", lwd=1.2) +
      geom_rect(mapping=aes(xmin=7.345, xmax=8, ymin=0, ymax=2), color="transparent", fill="white") +
      geom_rect(mapping=aes(xmin=7.345, xmax=8, ymin=-22, ymax=-17.3), color="transparent", fill="white") +
      geom_rect(mapping=aes(xmin=7.33, xmax=8, ymin=-5, ymax=5), color="transparent", fill="darkgray") + # SACW range
      geom_rect(mapping=aes(xmin=7.33, xmax=8, ymin=-15.6, ymax=-6.7), color="transparent", fill="darkgray") + # CACW range
      geom_rect(mapping=aes(xmin=7.33, xmax=8, ymin=-26.15, ymax=-18.3), color="transparent", fill="darkgray") + # CASW/unreacted H2CO3 range
    # Add data points
      annotate("text", x=4.32, y=-7, label=expression("Eq'm w/ atm CO"[2]), col="darkgray", size=3.5) +
      annotate("text", x=4.31, y=-25.2, label=expression("Eq'm w/ soil CO"[2]), col="darkgray", size=3.5) +
      geom_point(data=surf_df, aes(x=pH, y=d13cDIC), color=c("black","red")[surf_df$Burn], shape=c(24,23,21,22,25)[surf_df$LandType], fill=c("purple","green","darkblue","green4","blue")[surf_df$LandType], size=3, stroke=0.6) +
      geom_point(data=pore_df, aes(x=pH, y=d13cDIC), color=c("black","red")[pore_df$Burn], shape=c(3,7)[pore_df$LandType], fill=c("gray20","green4")[pore_df$LandType], size=2, stroke=0.6) +
      scale_color_manual(values=rep(c("black"),15)) +
      theme(axis.title.x=element_text(margin=margin(t=5, r=0, b=0, l=0))) +
      theme(axis.title.y=element_text(margin=margin(t=0, r=5, b=0, l=0))) +
      labs(y=expression(delta^"13"*"C-DIC"), x="pH") + #  = ‰, but allows you to export in PDF
      annotate("text", x=7.48, y=4.3, label="SACW", col="black", size=2.8) + # SACW label
      annotate("text", x=7.48, y=-7.4, label="CACW", col="black", size=2.8) + # CACW label
      annotate("text", x=7.48, y=-19, label="CASW", col="black", size=2.8) # CASW label
    