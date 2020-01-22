#========================================================================================================#
# Polaris_YKD_Aquatic.R
# Created: December 18, 2019
# By: Scott Zolkos
# Contact: szolkos@whrc.org
# Background: Code for analyses and figures for the Polaris Aquatic survey 2015-2019
#========================================================================================================#

  # Load packages
    library(Hmisc)
    library(gdata) # to bind columns with different number of rows
    library(vegan) # e.g. for rda function
    library(ecodist)
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
    #library(Cairo) # for exporting plots to PDF with axes labels containing with special glyphs e.g. permil
    #library(svglite) # for exporting plots to PDF with axes labels containing with special glyphs e.g. permil
    library(tidyverse)
    
  # Set working directory
    dir <- "/Users/szolkos/Documents/Research/Projects/The Polaris Project/Analyses/Data/Data4R/"
    setwd(dir)
    
########################
### DATA PREPARATION ###
########################
    
  # Import data
    ykd_aq <- read.csv(paste0(dir, "PolarisYKD_aquatic_2020_01_21.csv"), header=T)
    
  # Convert concentrations to µM
    ykd_aq$DOCuM <- ((ykd_aq$DOCmgL/1000)/12.0107)*1000000
    ykd_aq$TDNuM <- ((ykd_aq$TDNmgL/1000)/14.0067)*1000000
    ykd_aq$NO3uM <- ((ykd_aq$NO3ugL/1000000)/14.0067)*1000000
    ykd_aq$NH4uM <- ((ykd_aq$NH4ugL/1000000)/14.0067)*1000000
    ykd_aq$PO4uM <- ((ykd_aq$PO4ugL/1000000)/30.973762)*1000000
    ykd_aq$SiuM <- ((ykd_aq$SimgL/1000)/28.0855)*1000000
    
  # Add sampling year
    ykd_aq$Year <- paste0("20", substr(ykd_aq$Date,7,8)[ykd_aq$Date!="NA"])
    ykd_aq$Year[ykd_aq$Year=="20NA"] <- NA
    ykd_aq$Year <- as.numeric(ykd_aq$Year)
    
  # Set factors
    aq_envs <- c("porewater","stream","pond","lake")
    
##########################
### GRAPHICAL ANAYLSES ###
##########################
    
  # Subset
    ykd_aq_sub <- drop.levels(subset(ykd_aq, select=c("Date","Year","Aqenv","BurnType","Temp","Cond","pH","DOpcnt","DOmgL","DOCuM","TDNuM","NO3uM","NH4uM","PO4uM","SiuM","SR","SUVA254","CN","d13DIC","dexcess"), ykd_aq$Aqenv!="river" & ykd_aq$Aqenv!="fen" & ykd_aq$BurnType!="Both"))
    names(ykd_aq_sub)
    
  # Explore data
    par(mar=c(4.5,4.5,1,1))
    boxplot(ykd_aq_sub$dexcess~ykd_aq_sub$Aqenv, lwd=1.2, col="gray", las=2)
    
### HYDROCHEMISTRY BY YEAR AND BURN TYPE, FOR EACH AQUATIC ENVIRONMENT
    
  # Subset  data for boxplot
    param <- "DOCuM"
    bpdf_burn <- drop.levels(na.omit(subset(ykd_aq_sub, select=c("Date","Year","Aqenv","BurnType",param), ykd_aq_sub$BurnType=="Burned" | ykd_aq_sub$BurnType=="Control")))
    bpdf_burn <- drop.levels(na.omit(subset(bpdf_burn, bpdf_burn$Aqenv=="lake" | bpdf_burn$Aqenv=="pond" | bpdf_burn$Aqenv=="stream" | bpdf_burn$Aqenv=="porewater")))
    bpdf <- bpdf_burn
    #bpdf <- drop.levels(na.omit(subset(ykd_aq_sub, select=c("Date","Year","Aqenv","BurnType",param))))
    bpdf$Aqenv <- factor(bpdf$Aqenv, levels=aq_envs)
    bpdf$BurnType <- factor(bpdf$BurnType, levels=c("Control","Burned"))
    dim(bpdf); summary(bpdf); range(na.omit(bpdf[5]))
  
  # Explore trends by study Year and BurnType
  ## Subset  data
    bpdf_sub <- drop.levels(subset(bpdf, bpdf$Aqenv=="porewater"))
    bpdf_sub$BurnType <- factor(bpdf_sub$BurnType, levels=c("Control","Burned"))
    bpdf_sub$Year <- factor(bpdf_sub$Year, levels=c("2015","2016","2017","2018","2019"))
    
  ## Boxplot
    ggplot(bpdf_sub, aes(y=DOCuM, x=factor(Year), fill=BurnType)) +
      geom_boxplot(aes(fill=factor(BurnType))) +
      scale_fill_manual(name="Treatment", values=c("darkseagreen2","sienna3")) +
      #scale_y_log10(limits=c(0.1,1000), breaks=c(1,10,100,1000)) + # Cond
      #scale_y_log10(limits=c(7,1000), breaks=c(10,100,1000)) + # TDNuM
      #scale_y_log10(limits=c(0.01,30), breaks=c(0.01,0.1,1,10), labels=NotFancy) + # NO3uM
      #scale_y_log10(limits=c(0.01,1000), breaks=c(0.1,1,10,100,1000), labels=NotFancy) + # NH4uM
      #scale_y_log10(limits=c(1,100), breaks=c(1,10,100)) + # SiuM
      #scale_y_continuous(limits=c(0,0.8), breaks=seq(0,0.8,0.2)) + # PO4uM
      #scale_y_continuous(limits=c(0,20), breaks=c(0,2,4,6,8,10,20)) + # SUVA254
      #scale_y_log10(limits=c(1,100), breaks=c(1,5,10,20,30,40,50,100)) + # CN
      theme_bw() +
      theme(plot.margin=unit(c(0.1,0.1,0,0.1), "in"),
            panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            plot.background=element_rect(colour="white", size=1),
            panel.border=element_rect(colour="black", fill=NA, size=1),
            text=element_text(size=13)) +
      theme(axis.text.x=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
      theme(axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0))) +
      theme(axis.text.y=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
            #axis.text.y.right=element_text(color="grey40"),
            #axis.title.y.right=element_text(margin=margin(t=0, r=0, b=0, l=10), color="grey40")) +
      #theme(legend.position="none") +
      theme(legend.position=c(0.14,0.92)) + #, legend.justification=c("left","top")) +
      theme(legend.title=element_blank()) +
      theme(legend.background=element_rect(fill=FALSE, colour=FALSE)) +
      theme(plot.background=element_rect(fill='white')) +
      theme(panel.background=element_rect(fill='white')) +
      #labs(y=expression(Temp~(degree*C)), x="")
      #labs(y=expression(Conductivity~(µS~cm^-1)), x="")
      #labs(y="pH", x="")
      #labs(y="Dissolved oxygen (% saturation)", x="")
      labs(y="DOC (µM)", x="")
      #labs(y="TDN (µM)", x="")
      #labs(y=expression(NO[3]^"-"*~(µM)), x="")
      #labs(y=expression(NH[4]^"+"*~(µM)), x="")
      #labs(y=expression(PO[4]^"3-"*~(µM)), x="")
      #labs(y=expression(SiO[2]*~(µM)), x="")
      #labs(y=expression(Slope~ratio~(italic(S)[R])), x="")
      #labs(y=expression(SUVA[254]*~(L~mgC^-1*~m^-1), x=""), x="")
      #labs(y="C:N (DOC:TDN)", x="")
      #labs(y=expression(delta^"13"*"C-DIC (%VPDB)"), x="") # δ, ‰ | \u2030 # https://stackoverflow.com/questions/5293715/how-to-use-greek-symbols-in-ggplot2
      #labs(y="d-excess", x="")
    
    
### HYDROCHEMISTRY BY YEAR
    
  # Explore trends by year
    ggplot(bpdf, aes(y=pH, x=Aqenv, fill=factor(Year))) + 
      geom_boxplot(aes(fill=factor(Year))) +
      scale_fill_manual(name="Treatment", values=c("red","orange","yellow","green","blue")) +
      #scale_y_log10(limits=c(0.1,1000), breaks=c(1,10,100,1000)) + # Cond
      #scale_y_log10(limits=c(7,1000), breaks=c(10,100,1000)) + # TDNuM
      #scale_y_log10(limits=c(0.01,30), breaks=c(0.01,0.1,1,10), labels=NotFancy) + # NO3uM
      #scale_y_log10(limits=c(0.01,1000), breaks=c(0.1,1,10,100,1000), labels=NotFancy) + # NH4uM
      #scale_y_log10(limits=c(1,100), breaks=c(1,10,100)) + # SiuM
      #scale_y_continuous(limits=c(0,0.8), breaks=seq(0,0.8,0.2)) + # PO4uM
      #scale_y_continuous(limits=c(0,20), breaks=c(0,2,4,6,8,10,20)) + # SUVA254
      #scale_y_log10(limits=c(1,100), breaks=c(1,5,10,20,30,40,50,100)) + # CN
      theme_bw() +
      theme(plot.margin=unit(c(0.1,0.1,0,0.1), "in"),
            panel.grid.minor=element_blank(),
            panel.grid.major=element_blank(),
            plot.background=element_rect(colour="white", size=1),
            panel.border=element_rect(colour="black", fill=NA, size=1),
            text=element_text(size=13)) +
      theme(axis.text.x=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
      theme(axis.title.y=element_text(margin=margin(t=0, r=10, b=0, l=0))) +
      theme(axis.text.y=element_text(size=12, angle=0, hjust=0.5, colour="black")) +
      theme(legend.position=c(0.14,0.9)) + #, legend.justification=c("left","top")) +
      theme(legend.title=element_blank()) +
      theme(legend.background=element_rect(fill=FALSE, colour=FALSE)) +
      theme(plot.background=element_rect(fill='white')) +
      theme(panel.background=element_rect(fill='white')) +
      #labs(y=expression(Temp~(degree*C)), x="")
      #labs(y=expression(Conductivity~(µS~cm^-1)), x="")
      labs(y="pH", x="")
      #labs(y="Dissolved oxygen (% saturation)", x="")
      #labs(y="DOC (µM)", x="")
      #labs(y="TDN (µM)", x="")
      #labs(y=expression(NO[3]^"-"*~(µM)), x="")
      #labs(y=expression(NH[4]^"+"*~(µM)), x="")
      #labs(y=expression(PO[4]^"3-"*~(µM)), x="")
      #labs(y=expression(SiO[2]*~(µM)), x="")
      #labs(y=expression(Slope~ratio~(italic(S)[R])), x="")
      #labs(y=expression(SUVA[254]*~(L~mgC^-1*~m^-1), x=""), x="")
      #labs(y="C:N (DOC:TDN)", x="")
      #labs(y=expression(delta^"13"*"C-DIC (%VPDB)"), x="") # δ, ‰ | \u2030 # https://stackoverflow.com/questions/5293715/how-to-use-greek-symbols-in-ggplot2
      #labs(y="d-excess", x="")
    
    
    # Set PDF to default factory settings
      #pdf.options(reset=TRUE)
    # Set PDF to default encoding
      #pdf.options(width=5, height=4, onefile=T, family="Helvetica", title="R Graphics Output", fonts=NULL, version="1.4", paper="special", encoding="default", bg="transparent", fg="black", pointsize=12, pagecentre=T, colormodel="srgb", useDingbats=T, useKerning=T, fillOddEven=F, compress=T)
    # Set PDF to UTF-8 encoding, to export ‰ symbol with PDF
      #pdf.options(width=5, height=4, onefile=T, family="Helvetica", title="R Graphics Output", fonts=NULL, version="1.4", paper="special", encoding="UTF-8", bg="transparent", fg="black", pointsize=12, pagecentre=T, colormodel="srgb", useDingbats=T, useKerning=T, fillOddEven=F, compress=T)
    # Export figure
      ggsave(paste0("/Users/szolkos/Desktop/",param,".pdf"), chemplot, device="pdf", width=5, height=4)
    