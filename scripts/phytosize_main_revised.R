###############################################
### cell size of coastal phytoplankton    #####
###############################################

# based on data files by Claus Dürselen
# script by Helmut Hillebrand started 20210104
# major iteration 20210215, 20210312, 20210406


## initiate WS, libraries
rm(list=ls())
graphics.off()
setwd("data/")
library(readr)
library(plyr)
library(tidyverse)
library(vegan)
library(grid)
library(gridExtra)
library(calibrate)
library(maps)
library(mapdata)
library(psych)
library(reshape2)
library(agricolae)
library(ggExtra)
library(clusterSim)
library(cowplot)
library(ggridges)
library(multcomp)
library(RColorBrewer)
library(lubridate)
library(naniar)
library(magrittr)
library(lme4)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(glmmTMB)
library(glmmADMB)
library(DirichletReg)
library(Hmisc)
library(kableExtra)
library(effects)

##################
## primary data ##
##################

# read data
allppfinal <- read_csv("ppall_corr.csv")
summary(allppfinal)
allppfinal$stationID<-as.factor(allppfinal$stationID)
allppfinal$exclude<-as.factor(allppfinal$exclude)
allppfinal$phylum<-as.factor(allppfinal$phylum)
allppfinal$class<-as.factor(allppfinal$class)
allppfinal$order<-as.factor(allppfinal$order)
allppfinal$genus<-as.factor(allppfinal$genus)
allppfinal$species<-as.factor(allppfinal$species)
allppfinal$specname<-as.factor(allppfinal$specname)
allppfinal$specname.unique<-as.factor(allppfinal$specname.unique)

# check whether $exclude finds all species to be excluded
unique(allppfinal$species[is.na(allppfinal$exclude)])
allppfinal$exclude[allppfinal$species=="indet.ellipsoid"]<-"not.to.species"
allppfinal$exclude[allppfinal$species=="indet.rhom"]<-"not.to.species"
allppfinal$exclude<-as.factor(allppfinal$exclude)

# according to NLWKN, some stations changed names
# these have been aligned, check if needed
unique(allppfinal$stationID)

# factorize unique sample identifier
allppfinal$USI<-as.factor(allppfinal$USI)

# check a few numeric columns for NA
names(allppfinal)
summary(allppfinal[,c("cell.vol","mean.size","abundance","biovol","year","julian")])

#delete rows where no size was measured
allppfinal<-(allppfinal[!is.na(allppfinal$mean.size),])

#recalculate biovol
allppfinal$biovolume<-allppfinal$abundance*allppfinal$mean.size
plot(log(biovolume)~log(biovol), allppfinal)

### subsetting data in two branches ###########################
# subset 1: all taxa identified ==> allppfinal ==> see below
# subset 2: only taxa identified to species ==> include
###############################################################

################
### subset 2 ###
################

include<-allppfinal[is.na(allppfinal$exclude),]

# to get weighted means, sum per sample & the relative proportion of 
# species to total abundance and biomass are calculated

# create sum abundance and biovol for the subset assemblage
stationinfo.incl<- ddply(include, .(USI), summarise, 
                 abund.incl= sum(abundance, na.rm = T),
                 biovol.incl= sum(biovolume, na.rm = T))

# create a file to get species specific averages
specinfo.incl<-merge(stationinfo.incl,include, by="USI")
names(specinfo.incl)
summary(specinfo.incl)

#calculate relative abundance and biovol
specinfo.incl$relabund<-specinfo.incl$abundance/specinfo.incl$abund.incl
specinfo.incl$relbiovol<-specinfo.incl$biovolume/specinfo.incl$biovol.incl

#get total number of samples
sampN<-length(unique(specinfo.incl$USI))

# get species specific information across samples
specinfo.incl<- ddply(specinfo.incl, .(phylum, class, order, genus, species,specname), summarise, 
            occupancy= length(USI)/sampN,#ratio presence/total sample
            first= min(julian),#first occurrence in a year
            last= max(julian),#last occurrence in a year
            veg.period=last-first,#length of occurrence in a year
            mean.vol=mean(mean.size,na.rm=T),#arithmetic average cell size
            sd.vol=sd(mean.size,na.rm=T),#sd of previous
            mean.relabu=mean(relabund),#average proportion to abundance
            sd.relabu=sd(relabund,na.rm=T),#sd of previous
            mean.relbiov=mean(relbiovol,na.rm=T),# average proportion to biovolume
            sd.relbiov=sd(relbiovol,na.rm=T))#sd of previous

#transform some variables
specinfo.incl$LN.size<-log(specinfo.incl$mean.vol)
specinfo.incl$sqrt.relabu<-sqrt(specinfo.incl$mean.relabu)
specinfo.incl$sqrt.relvol<-sqrt(specinfo.incl$mean.relbiov)
specinfo.incl$sqrt.occ<-sqrt(specinfo.incl$occupancy)
summary(specinfo.incl$specname)

# quick visual check
pairs.panels(specinfo.incl[,c("veg.period","LN.size","sqrt.relabu","sqrt.relvol","sqrt.occ")])
names(include)

# get a data set with single cell measurements
vol.incl<-include[,c(3:9,123:125,135,138,140:169)]
summary(vol.incl)

# format into long format
vol.incl<- melt(vol.incl,
           id.vars=c("USI","stationID","date","year","julian",
                     "phylum","class","order","genus",
                     "species", "specname","specname.unique"),
           variable.name="no",
           value.name="cell.vol")
vol.incl<-vol.incl[!is.na(vol.incl$cell.vol),]
vol.incl<-vol.incl[(vol.incl$cell.vol>0),]

summary(vol.incl)

vol.incl$LN.cell.vol<-log(vol.incl$cell.vol)
vol.incl$no<-NULL

# a quick check
plot(LN.cell.vol~date,vol.incl)
plot(LN.cell.vol~julian,vol.incl)

## statistics for subset 2 (species level)
data <- vol.incl

#create a factorial identifier for year
data$yearID<-as.factor(data$year)

# to reflect the cyclic nature of seasonality, transfrom
# julian day into a variable of "summerness"
data$jul2<-data$julian
data$jul2[data$jul2>183]<-365-data$julian[data$jul2>183]

#check classes and create a color gradient
unique(data$class)

mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(14)
plot(1:14,col=mycolors,cex=5,pch=17)

data$color<-NA
data$color[data$class=="Bacillariophyceae"]<-mycolors[4]
data$color[data$class=="Coscinodiscophyceae"]<-mycolors[5]
data$color[data$class=="Mediophyceae"]<-mycolors[6]
data$color[data$class=="Dinophyceae"]<-mycolors[3]
data$color[data$class=="Euglenophyceae"]<-mycolors[8]
data$color[data$class=="Pyramimonadophyceae"]<-mycolors[11]
data$color[data$class=="Dictyophyceae"]<-mycolors[14]
data$color[data$class=="Cyanophyceae"]<-mycolors[1]
data$color[data$class=="Ulvophyceae"]<-mycolors[9]
data$color[data$class=="Chlorophyceae"]<-mycolors[2]
data$color[data$class=="Coccolithophyceae"]<-mycolors[12]
data$color[data$class=="Katablepharidophyceae"]<-mycolors[10]


summary(as.factor(data$color))

class.colors <- c(Bacillariophyceae=mycolors[4], 
                  Coscinodiscophyceae=mycolors[5], 
                  Mediophyceae=mycolors[6], 
                  Dinophyceae=mycolors[3], 
                  Euglenophyceae=mycolors[8], 
                  Pyramimonadophyceae=mycolors[11], 
                  Dictyophyceae=mycolors[14], 
                  Cyanophyceae=mycolors[1], 
                  Ulvophyceae=mycolors[9], 
                  Chlorophyceae=mycolors[2], 
                  Coccolithophyceae=mycolors[12], 
                  Katablepharidophyceae=mycolors[10])
summary(data)
names(data)


##################################################
# subset 2 (species level) statistical analyses ##
##################################################

## FULL MODEL
# without year ID
mod.incl.full.SID<-lmer(LN.cell.vol~year+jul2
             +(1|phylum/class/order/genus/species)
             +(1|stationID),
             data=data)
summary(mod.incl.full.SID)

# with year ID
mod.incl.full.SID.YID<-lmer(LN.cell.vol~year+jul2
            +(1|phylum/class/order/genus/species)
            +(1|yearID)
            +(1|stationID),
            data=data)
summary(mod.incl.full.SID.YID)
tab_model(mod.incl.full.SID.YID, digits=4)


## based on friendly review comment: leave out 2006
# without year ID
mod.incl.no2006.SID<-lmer(LN.cell.vol~year+jul2
                        +(1|phylum/class/order/genus/species)
                        +(1|stationID),
                        data=data[data$year>2006,])
summary(mod.incl.no2006.SID)

# with year ID
mod.incl.no2006.SID.YID<-lmer(LN.cell.vol~year+jul2
                            +(1|phylum/class/order/genus/species)
                            +(1|yearID)
                            +(1|stationID),
                            data=data[data$year>2006,])
summary(mod.incl.no2006.SID.YID)
tab_model(mod.incl.no2006.SID.YID, digits=4)



plot_model(mod.incl.full.SID.YID, type = "pred", terms = c("year","jul2"))
plot.incl.full.SID.YID<-plot_model(mod.incl.full.SID.YID, type = "pred", terms = c("year"),title="", size=2)
plot.incl.full.SID.YID
fig.incl.full.SID.YID<-plot.incl.full.SID.YID+theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("Year")+
  theme(legend.position = "none")+
  geom_jitter(data=data,aes(x=year,y=LN.cell.vol,col=class),size=.1,alpha=.1)+
  scale_color_manual(values = class.colors) +
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
fig.incl.full.SID.YID


date.raw<-ggplot(data=data,aes(x=date,y=LN.cell.vol,col=class))+
  theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("Time")+
  theme(legend.position = "none")+
  geom_jitter(size=.1,alpha=.1)+
  scale_color_manual(values = class.colors) +
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
date.raw

#add regression manually
dates <- read_delim("dates.csv", ";", escape_double = FALSE, 
                    trim_ws = TRUE)
dates$date<-dmy(dates$date)
dates$year<-year(dates$date)
dates$midyear<-dmy(dates$midyear)
dates$value<-NULL

effects_year <- effects::effect(term= "year", mod= mod.incl.full.SID.YID)
summary(effects_year)
x_year<-as.data.frame(effects_year)
summary(x_year)
x_year<-merge(x_year,dates,by="year")
names(x_year)
raw.incl.final<-date.raw+
  geom_line(data=x_year, aes(x=midyear, y=fit), color="black",size=1)+ 
  geom_line(data=x_year, aes(x=midyear, y=lower), color="darkgrey",size=1)+ 
  geom_line(data=x_year, aes(x=midyear, y=upper), color="darkgrey",size=1)



#check for temporal autocorrelation

acf(residuals(mod.incl.full.SID))
acf(residuals(mod.incl.full.SID.YID))
AIC(mod.incl.full.SID.YID,mod.incl.full.SID)

#redo analyses with mean cell volumes per sample

names(include)
include$jul2<-include$julian
include$jul2[include$jul2>183]<-365-include$julian[include$jul2>183]
include$yearID<-as.factor(include$year)
summary(include)

#without year ID
mod.incl.mean.SID<-lmer(log(mean.size)~year+jul2
                            +(1|phylum/class/order/genus/species)
                            +(1|stationID),
                            data=include)
summary(mod.incl.mean.SID)

#with year ID
mod.incl.mean.SID.YID<-lmer(log(mean.size)~year+jul2
             +(1|phylum/class/order/genus/species)
             +(1|yearID)
             +(1|stationID),
             data=include)
summary(mod.incl.mean.SID.YID)

tab_model(mod.incl.mean.SID.YID,digits = 4)

acf(residuals(mod.incl.mean.SID))
acf(residuals(mod.incl.mean.SID.YID))
AIC(mod.incl.mean.SID.YID,mod.mean.full.SID)


# leave out year 2006
# without year ID
mod.incl.mean.no2006.SID<-lmer(log(mean.size)~year+jul2
                        +(1|phylum/class/order/genus/species)
                        +(1|stationID),
                        data=include[include$year>2006,])
summary(mod.incl.mean.no2006.SID)

#with year ID
mod.incl.mean.no2006.SID.YID<-lmer(log(mean.size)~year+jul2
                            +(1|phylum/class/order/genus/species)
                            +(1|yearID)
                            +(1|stationID),
                            data=include[include$year>2006,])
summary(mod.incl.mean.no2006.SID.YID)


#FOR PUBLICATION
tab_model(mod.incl.full.SID.YID,mod.incl.mean.SID.YID,digits = 3)


###########################################
## species specific models over time   ####
###########################################

summary(data)
test<-data[data$specname=="Brockmanniella_brockmannii",]
mod.test<-lmer(LN.cell.vol~year+jul2
             +(1|yearID)
             +(1|stationID),
             data=test)
summary(mod.test)
coef(summary(mod.test))
lmer.int<-coef(summary(mod.test))[1,1]
lmer.int.se<-coef(summary(mod.test))[1,2]
lmer.slp<-coef(summary(mod.test))[2,1]
lmer.se<-coef(summary(mod.test))[2,2]

UIspec<-unique(data$specname)

slope.year<-data.frame()

for(i in 1:length(UIspec)){
  temp<-data[data$specname==UIspec[i], ]#creates a temporary data frame for each case
  if(dim(temp)[1]>100){#does next step only if at least 100 datapoints are available
    if((max(temp$year)-min(temp$year))>5){#does the next step only if at least 6 years apart are present
    lm1<-lm(LN.cell.vol~year, temp)#makes a linear regreassion
    icpt <- coef(summary(lm1))[1, 1]#selects the slope
    slp <- coef(summary(lm1))[2, 1]#selects the slope
    se.slp<- coef(summary(lm1))[2, 2]#selects its standard error
    p<-anova(lm1)$'Pr(>F)'[1]#gives the p-value
    N<-dim(temp)[1] # gives the number of data points
    median<-median(temp$cell.vol) # gives the median cell size
    mod.test<-lmer(LN.cell.vol~year+jul2
                   +(1|yearID)
                   +(1|stationID),
                   data=temp)
    lmer.int<-coef(summary(mod.test))[1,1]
    lmer.int.se<-coef(summary(mod.test))[1,2]
    lmer.slp<-coef(summary(mod.test))[2,1]
    lmer.se<-coef(summary(mod.test))[2,2]
    slope.year<-rbind(slope.year,data.frame(temp[1,"specname"],
                                            icpt,slp,se.slp,p,
                                            N,median,
                                            lmer.int, lmer.int.se,
                                            lmer.slp, lmer.se))
    rm(temp)
  }
  }
}

summary(slope.year)
slope.year<-plyr::rename(slope.year, c("temp.1...specname.."="specname"))
unique(slope.year$specname)

appendix.tab1<-slope.year[,c(1,3,4,5,6,10,11)]
names(appendix.tab1)
appendix.tab1 <- appendix.tab1[order(appendix.tab1$specname),]
appendix.tab1<-appendix.tab1 %>% mutate_if(is.numeric, round, digits = 4)

#FOR PUBLICATION
kbl(appendix.tab1, caption = "") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F) %>%
  add_header_above(c(" ", "Linear Model" = 4, "Mixed Model" = 2))

hist(slope.year$slp)

unique(specinfo.incl$specname)
fulldat.incl<-merge(specinfo.incl,slope.year, by="specname",all=F)

names(fulldat.incl)
fulldat.incl$median.size<-log(fulldat.incl$median)
fulldat.incl$col<-NA
fulldat.incl$col[fulldat.incl$p>.05]<-.2
fulldat.incl$col[fulldat.incl$p<.05]<-.6
summary(fulldat.incl)

fulldat.incl$pch1<-log(abs(fulldat.incl$slp/fulldat.incl$se.slp))+1
fulldat.incl$pch2<-log(abs(fulldat.incl$lmer.slp/fulldat.incl$lmer.se))+1

unique(fulldat.incl$class)
fulldat.incl$color<-NA
fulldat.incl$color[fulldat.incl$class=="Bacillariophyceae"]<-mycolors[4]
fulldat.incl$color[fulldat.incl$class=="Coscinodiscophyceae"]<-mycolors[5]
fulldat.incl$color[fulldat.incl$class=="Mediophyceae"]<-mycolors[6]
fulldat.incl$color[fulldat.incl$class=="Dinophyceae"]<-mycolors[3]
fulldat.incl$color[fulldat.incl$class=="Euglenophyceae"]<-mycolors[8]
fulldat.incl$color[fulldat.incl$class=="Pyramimonadophyceae"]<-mycolors[11]
fulldat.incl$color[fulldat.incl$class=="Dictyophyceae"]<-mycolors[14]
fulldat.incl$color[fulldat.incl$class=="Cyanophyceae"]<-mycolors[1]
fulldat.incl$color[fulldat.incl$class=="Ulvophyceae"]<-mycolors[9]
fulldat.incl$color[fulldat.incl$class=="Chlorophyceae"]<-mycolors[2]
fulldat.incl$color[fulldat.incl$class=="Coccolithophyceae"]<-mycolors[12]
fulldat.incl$color[fulldat.incl$class=="Katablepharidophyceae"]<-mycolors[10]

names(fulldat.incl)
pairs.panels(fulldat.incl[,c(11,19:21,18,32,23,30)],method="spearman")
plot(slp~lmer.slp,fulldat.incl)
hist(fulldat.incl$lmer.slp)
hist(fulldat.incl$slp)

summary(fulldat.incl)

spec.slope.size<-ggplot(fulldat.incl, 
                   aes(median.size,slp, 
                       col=class))+
  geom_hline(yintercept=0)+
  geom_point(alpha=fulldat.incl$col,
             size=fulldat.incl$pch1)+
  theme_bw()+
  ylab("Slope LN size~time")+
  xlab("Median cell size [log µm³]")+
  scale_color_manual(values = mycolors[c(4,5,3,8,6)])+  
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=2)))
  #+facet_wrap(~phylum, scales="free")

spec.slope.size


spec.slope.relabu<-ggplot(fulldat.incl, 
                        aes(sqrt.relabu,slp, 
                            col=class))+
  geom_hline(yintercept=0)+
  geom_point(alpha=fulldat.incl$col,
             size=fulldat.incl$pch1)+
  theme_bw()+
  ylab("Slope LN size~time")+ylim(-.25,.2)+
  xlab("Relative abundance [sqrt-transf.]")+
  scale_color_manual(values = mycolors[c(4,5,3,8,6)])+   
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=2)))


#+facet_wrap(~phylum, scales="free")

spec.slope.relabu





spec.lmer.slope.size<-ggplot(fulldat.incl, 
                               aes(median.size,lmer.slp, 
                                   col=class))+
  geom_hline(yintercept=0)+
  geom_point(size=2*fulldat.incl$pch2, alpha=.5)+
  theme_bw()+
  scale_color_manual(values = mycolors[c(5,6,3,9,7)])+  
  ylab("Fixed effect LN size~year")+
  xlab("Median cell size [log µm³]")+ylim(-.25,.2)+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3)))+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=12))

#+facet_wrap(~phylum, scales="free")

spec.lmer.slope.size



spec.lmer.slope.relabu<-ggplot(fulldat.incl, aes(sqrt.relabu,lmer.slp, 
                       col=class))+
  geom_hline(yintercept=0)+
  geom_point(size=2*fulldat.incl$pch2,alpha=.5)+
  theme_bw()+
  scale_color_manual(values = mycolors[c(5,6,3,9,7)])+  
  ylab("  ")+
  xlab("Relative abundance [sqrt-transf.]")+ylim(-.25,.2)+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3)))+
  theme(legend.title=element_text(size=12),
        legend.text=element_text(size=12))

#+facet_wrap(~phylum, scales="free")

spec.lmer.slope.relabu

names(fulldat.incl)
cor.test(fulldat.incl$sqrt.relabu,fulldat.incl$lmer.slp)
cor.test(fulldat.incl$median.size,fulldat.incl$lmer.slp)

#for identification redo graph with text labels
spec.lmer.slope.relabu.show<-ggplot(fulldat.incl, aes(sqrt.relabu,lmer.slp, 
                                                 col=class))+
  geom_hline(yintercept=0)+
  geom_point(size=2*fulldat.incl$pch2,alpha=.5)+
  theme_bw()+
  scale_color_manual(values = mycolors[c(4,5,3,8,6)])+  
  ylab("Fixed effect LN size~year")+
  xlab("Relative abundance [sqrt-transf.]")+ylim(-.25,.2)+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_label_repel(data=fulldat.incl[fulldat.incl$lmer.slp>0,],aes(sqrt.relabu,lmer.slp,label=specname),col="orange") 
#+facet_wrap(~phylum, scales="free")

spec.lmer.slope.relabu.show



## Violin & density plots

names(data)
violin.raw<-ggplot(data, 
                   aes(as.factor(year),LN.cell.vol))+
  geom_violin(col="black",fill="steelblue")+
  stat_summary(mapping = aes(x=as.factor(year),
                             y=LN.cell.vol),
               fun.y = "median",
               geom = "point",size=3,
               col="red")+
  theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("Year")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

violin.raw

violin.samplemean<-ggplot(include, 
                   aes(as.factor(year),log(mean.size)))+
  geom_violin(col="black",fill="steelblue")+
  stat_summary(mapping = aes(x=as.factor(year),
                             y=log(mean.size)),
               fun.y = "median",
               geom = "point",size=3,
               col="yellow")+
  theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("Year")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

violin.samplemean


## dominant species plot
#add dominance values to data 

data2<-merge(data,fulldat.incl[c("specname","sqrt.relabu","sqrt.relvol","sqrt.occ","icpt","slp","p","N","lmer.slp","lmer.se")], by="specname", all=FALSE)
unique(data2$specname)

# quick check
allspec<-ggplot(data2, 
                aes(year,LN.cell.vol,col=species))+
  geom_point(alpha=.1)+
  theme(legend.position="none")+
  theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("year")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  facet_wrap(~genus,scales="free_y")
allspec


#select some species
select<-data2[data2$sqrt.relabu>.05,]
select<-select[select$N>100,]
select<-select[which(abs(select$sqrt.occ) > .2 | select$phylum!="Bacillariophyta") , ]
#select<-select[which(abs(select$lmer.slp) > .05 | select$phylum!="Bacillariophyta") , ]
select<-select[select$specname!="Asteroplanus_karianus",]
select<-select[select$specname!="Bacillaria_paxillifera",]
select<-select[select$specname!="Bacteriastrum_hyalinum",]
select<-select[select$specname!="Biddulphia_alternans",]
select<-select[select$specname!="Brockmanniella_brockmannii",]
select<-select[select$specname!="Chaetoceros_curvisetus",]
select<-select[select$specname!="Chaetoceros_danicus",]
select<-select[select$specname!="Chaetoceros_densus",]
select<-select[select$specname!="Chaetoceros_didymus",]
select<-select[select$specname!="Cymatosira_belgica",]
select<-select[select$specname!="Detonula_pumila",]
select<-select[select$specname!="Eucampia_zodiacus",]
select<-select[select$specname!="Guinardia_flaccida ",]
select<-select[select$specname!="Guinardia_striata",]
select<-select[select$specname!="Gyrosigma_fasciola",]
select<-select[select$specname!="Lithodesmium_undulatum",]
select<-select[select$specname!="Lauderia_annulata",]
select<-select[select$specname!="Lennoxia_faveolata",]
select<-select[select$specname!="Mediopyxis_helysia",]
select<-select[select$specname!="Odontella_aurita var. minima",]
select<-select[select$specname!="Odontella_rhombus f. trigona",]
select<-select[select$specname!="Odontella_rhombus",]
select<-select[select$specname!="Paralia_sulcata",]
select<-select[select$specname!="Plagiogrammopsis_vanheurckii",]
select<-select[select$specname!="Pseudo-nitzschia_pungens",]
select<-select[select$specname!="Thalassiosira_punctigera",]
select<-select[select$specname!="Tripos_fusus",]
select<-select[select$specname!="Thalassiosira_punctigera",]
select<-select[select$specname!="Rhizosolenia_imbricata",]
select<-select[select$specname!="Rhizosolenia_setigera f. pungens",]
select<-select[select$specname!="Rhizosolenia_similoides",]
select<-select[select$specname!="Guinardia_flaccida",]
select<-select[select$specname!="Chaetoceros_debilis",]
select<-select[select$specname!="Heterocapsa_rotundata",]
select<-select[select$specname!="Leptocylindrus_minimus",]
select<-select[select$specname!="Trieres_mobiliensis",]
summary(select)
unique(select$specname)

selectspec<-ggplot(select, 
                aes(year,LN.cell.vol, col=class))+
  geom_point(alpha=.1)+
  scale_color_manual(values = c(mycolors[5],mycolors[6],mycolors[3],mycolors[9],mycolors[7]))+   
  geom_smooth(col="blue")+
  #geom_smooth(method="lm",col="red")+
  theme(legend.position="none")+
  theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("year")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  facet_wrap(~specname,scales="free_y")+
  guides(colour = guide_legend(override.aes = list(alpha = 1)))

selectspec

#################################
## Community weighted means  ####
#################################

names(data)
#create mean values per species and sample
means.incl<-ddply(data, .(stationID, date,USI,phylum, class, order, genus, species,specname), summarise, 
      mean.ar= mean(cell.vol, na.rm = T),
      sd.ar= sd(cell.vol, na.rm = T),
      cv.ar=sd.ar/mean.ar,
      mean.gm= mean(log(cell.vol), na.rm = T),
      sd.gm= sd(log(cell.vol), na.rm = T),
      cv.gm=sd.gm/mean.gm)
summary(means.incl)
names(means.incl)
#merge with mean values from mean dimensions
test.incl<-merge(means.incl,include[,c("USI","genus","species","mean.size","abundance","biovolume")], by=c("USI","genus","species"), all=FALSE)
#these should be pretty linear
plot(mean.ar~mean.size,test.incl)
plot(mean.gm~log(mean.size),test.incl)
hist(test.incl$mean.gm)
hist(log(test.incl$mean.size))

names(stationinfo.incl)

weights.incl<-merge(test.incl,stationinfo.incl, by="USI")
weights.incl$relabund<-weights.incl$abundance/weights.incl$abund.incl
weights.incl$relbiovol<-weights.incl$biovolume/weights.incl$biovol.incl

numspec.incl <-
  weights.incl %>%
  dplyr::group_by(USI) %>%
  dplyr::summarize(N= length(USI))
hist(numspec.incl$N)
weights.incl<-merge(weights.incl,numspec.incl, by="USI")
weights.incl<-weights.incl[weights.incl$N>10,]
weights.incl$year<-year(weights.incl$date)
weights.incl$lnsize<-log(weights.incl$mean.size)

summary(weights.incl)

pp.cwm.incl <-
  weights.incl %>%
  dplyr::group_by(stationID,date) %>%
  dplyr::summarize(N= length(USI),
            cwm_abu = weighted.mean(mean.ar, relabund,na.rm=T), 
            cwm_vol = weighted.mean(mean.ar, relbiovol,na.rm=T),
            ln_cwm_abu = weighted.mean(mean.gm, relabund,na.rm=T), 
            ln_cwm_vol = weighted.mean(mean.gm, relbiovol,na.rm=T),
            cwsd_abu = wtd.var(mean.ar, relabund,na.rm=T), 
            cwsd_vol = wtd.var(mean.ar, relbiovol,na.rm=T),
            ln_cwsd_abu = wtd.var(mean.gm, relabund,na.rm=T), 
            ln_cwsd_vol = wtd.var(mean.gm, relbiovol,na.rm=T))

names(pp.cwm.incl)
pp.cwm.incl$cwsd_abu[pp.cwm.incl$cwsd_abu=="Inf"]<-NA
pp.cwm.incl$cwsd_vol[pp.cwm.incl$cwsd_vol=="Inf"]<-NA
pp.cwm.incl$ln_cwsd_abu[pp.cwm.incl$ln_cwsd_abu=="Inf"]<-NA
pp.cwm.incl$ln_cwsd_vol[pp.cwm.incl$ln_cwsd_vol=="Inf"]<-NA

pp.cwm.incl$year<-year(pp.cwm.incl$date)
pp.cwm.incl$julian<-yday(pp.cwm.incl$date)
pp.cwm.incl$yearID<-as.factor(pp.cwm.incl$year)

pairs.panels((pp.cwm.incl[,c(3:13)]),method="spearman")

summary(pp.cwm.incl)
names(pp.cwm.incl)

Env_data <- read_csv("~/R/InterReg-project/C_Chl_ratio/MARISCO_pp_wadden_env.csv")
summary (Env_data)
#rename columns
Env_data$date<-dmy(Env_data$date)
Env_data$stationID<-(Env_data$StationID)
env.cwm.incl<-merge(pp.cwm.incl,Env_data, by=c("stationID","date"))

size.cwm<-ggplot(pp.cwm.incl, 
                    aes(date,ln_cwm_abu))+
  geom_jitter(alpha=.1)+
  #geom_violin(alpha=.21)+
  geom_smooth(method="lm")+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Community weighted mean cell size [LN µm³]")+
  xlab("year")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

size.cwm




violin.cwm<-ggplot(pp.cwm.incl, 
                 aes(as.factor(year),ln_cwm_abu))+
  geom_violin(alpha=.21)+
  #geom_smooth()+
  theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("year")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())#+
  #facet_wrap(~stat)

violin.cwm



# add the seasonal julian variable again
pp.cwm.incl$jul2<-pp.cwm.incl$julian
pp.cwm.incl$jul2[pp.cwm.incl$jul2>183]<-365-pp.cwm.incl$julian[pp.cwm.incl$jul2>183]
env.cwm.incl$jul2<-env.cwm.incl$julian
env.cwm.incl$jul2[env.cwm.incl$jul2>183]<-365-env.cwm.incl$julian[env.cwm.incl$jul2>183]


#check different options of CWM
test1<-lmer(ln_cwm_abu~year+jul2
            +(1|stationID)
            +(1|yearID),
            data=pp.cwm.incl[pp.cwm.incl$year>2006,])

test2<-lmer(ln_cwm_abu~year+jul2
            +(1|stationID),
            data=pp.cwm.incl)

test3<-lmer(ln_cwm_abu~year+jul2
            +(1|stationID)
            +(1|yearID),
            data=pp.cwm.incl)


#check different options of CWM

test.abu<-lmer(log(cwm_abu)~year+jul2
            +(1|stationID)
            +(1|yearID),
            data=pp.cwm.incl)
test.ln.abu<-lmer(ln_cwm_abu~year+jul2
            +(1|stationID)
            +(1|yearID),
            data=pp.cwm.incl)
test.vol<-lmer(log(cwm_vol)~year+jul2
               +(1|stationID)
               +(1|yearID),
               data=pp.cwm.incl)
test.ln.vol<-lmer(ln_cwm_vol~year+jul2
                  +(1|stationID)
                  +(1|yearID),
                  data=pp.cwm.incl)

tab_model(test.abu,test.ln.abu,test.vol,test.ln.vol)
summary(test3)

tab_model(test3,digits=3)

effects_cwm.year <- effects::effect(term= "year", mod= test3)
summary(effects_cwm.year)
x_cwm.year<-as.data.frame(effects_cwm.year)
summary(x_cwm.year)
x_cwm.year<-merge(x_cwm.year,dates,by="year")
names(x_cwm.year)


# final figures for CWM
size.cwm.incl<-ggplot(pp.cwm.incl, 
                      aes(date,ln_cwm_abu,col=stationID))+
  geom_point(alpha=.2)+
  #geom_violin(alpha=.21)+
  #geom_smooth()+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("CWM cell size [LN µm³]")+
  xlab("year")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

size.cwm.incl

cwm.incl.final<-size.cwm.incl+
  geom_line(data=x_cwm.year, aes(x=midyear, y=fit), color="black",size=1)+ 
  geom_line(data=x_cwm.year, aes(x=midyear, y=lower), color="darkgrey",size=1)+ 
  geom_line(data=x_cwm.year, aes(x=midyear, y=upper), color="darkgrey",size=1)

cwm.incl.final


## model with environmental variables
names(env.cwm.incl)

# without year ID
envmod1<-lmer(ln_cwm_abu~temperature+
                log(total.n)+
                log(suspended.particulates)+
                log(total.p)+
                log(silicon)+
                pH+
                salinity+
                (1|stationID),
              data=env.cwm.incl)

#with year ID
envmod2<-lmer(ln_cwm_abu~temperature+
                log(total.n)+
                log(suspended.particulates)+
                log(total.p)+
                log(silicon)+
                pH+
                salinity+
                (1|stationID)+(1|yearID),
              data=env.cwm.incl)

summary(envmod2)


#checking for interannual versus intraannual effects
#creating mean values per year 
names(env.cwm.incl)

env.cwm.incl.year <-
  env.cwm.incl %>%
  dplyr::group_by(stationID, year, yearID) %>%
  dplyr::summarize(ln_cwm_abu = mean(ln_cwm_abu),
                   temperature=mean(temperature,na.rm=TRUE),
                   TN=mean(log(total.n),na.rm=TRUE),
                   TP=mean(log(total.p),na.rm=TRUE))

summary(env.cwm.incl.year)
envmod3<-lmer(ln_cwm_abu~temperature+TN+TP+
                (1|stationID),
              data=env.cwm.incl.year)

summary(envmod3)



#FOR PUBLICATIOn
tab_model(test3,envmod2,envmod3, digits =3)



#Graphs for CWM

obj<-plot_model(test3, type = "pred", pred.type="re",terms = c("year"),title="")
obj
obj2<-plot_model(envmod2, type = "pred", pred.type="re",terms = c("temperature"),title="")
obj2
obj3<-plot_model(envmod2, type = "pred", pred.type="re",terms = c("total.p"),title="")
obj3

mod.fig3<-obj+theme_bw()+
  ylab("CWM cell size [LN µm³]")+
  xlab("Year")+
  geom_jitter(data=pp.cwm.incl,aes(x=year,y=ln_cwm_abu,alpha=jul2-.8),col="darkgreen")+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

mod.fig3


mod.fig4<-obj2+theme_bw()+
  ylab("CWM cell size [LN µm³]")+
  xlab("Temperature [°C]")+
  geom_jitter(data=env.cwm.incl,aes(x=temperature,y=ln_cwm_abu,alpha=jul2),col="blue")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(legend.position = "none")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

mod.fig4



#alternative
effects_cwm.temp <- effects::effect(term= "temperature", mod= envmod2)
summary(effects_cwm.temp)
x_cwm.temp<-as.data.frame(effects_cwm.temp)
names(x_cwm.temp)
names(env.cwm.incl)
      
temp.raw.incl<-ggplot(data=env.cwm.incl,aes(x=temperature,y=ln_cwm_abu))+
  theme_bw()+
  ylab("CWM cell size [LN µm³]")+
  xlab("Temperature [°C]")+
  geom_point(aes(alpha=jul2),col="darkred")+
  geom_line(data=x_cwm.temp, aes(x=temperature, y=fit), color="black",size=1)+ 
  geom_line(data=x_cwm.temp, aes(x=temperature, y=lower), color="darkgrey",size=1)+ 
  geom_line(data=x_cwm.temp, aes(x=temperature, y=upper), color="darkgrey",size=1)+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(legend.position = "none")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

temp.raw.incl





#alternative
effects_cwm.totaln <- effects::effect(term= "log(total.n)", mod= envmod2)
summary(effects_cwm.totaln)
x_cwm.totaln<-as.data.frame(effects_cwm.totaln)
names(x_cwm.totaln)
names(env.cwm.incl)

totaln.raw.incl<-ggplot(data=env.cwm.incl,aes(x=log(total.n),y=ln_cwm_abu))+
  theme_bw()+
  ylab("CWM cell size [LN µm³]")+
  xlab("Total N [LN µM]")+
  geom_point(aes(alpha=jul2),col="darkblue")+
  geom_line(data=x_cwm.totaln, aes(x=log(total.n), y=fit), color="black",size=1)+ 
  geom_line(data=x_cwm.totaln, aes(x=log(total.n), y=lower), color="darkgrey",size=1)+ 
  geom_line(data=x_cwm.totaln, aes(x=log(total.n), y=upper), color="darkgrey",size=1)+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(legend.position = "none")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

totaln.raw.incl


#alternative
effects_cwm.totalp <- effects::effect(term= "log(total.p)", mod= envmod2)
summary(effects_cwm.totalp)
x_cwm.totalp<-as.data.frame(effects_cwm.totalp)
names(x_cwm.totalp)
names(env.cwm.incl)

totalp.raw.incl<-ggplot(data=env.cwm.incl,aes(x=log(total.p),y=ln_cwm_abu))+
  theme_bw()+
  ylab("CWM cell size [LN µm³]")+
  xlab("Total P [LN µM]")+
  geom_point(aes(alpha=jul2),col="darkgreen")+
  geom_line(data=x_cwm.totalp, aes(x=log(total.p), y=fit), color="black",size=1)+ 
  geom_line(data=x_cwm.totalp, aes(x=log(total.p), y=lower), color="darkgrey",size=1)+ 
  geom_line(data=x_cwm.totalp, aes(x=log(total.p), y=upper), color="darkgrey",size=1)+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(legend.position = "none")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

totalp.raw.incl



#############################
###  SUBSET 1  ##############
#############################

##Analysis for all data

# create sum abundance and biovol for the subset assemblage
stationinfo.all<- ddply(allppfinal, .(USI), summarise, 
                         abund.all= sum(abundance, na.rm = T),
                         biovol.all= sum(biovolume, na.rm = T))

# create a file to get species specific averages
specinfo.all<-merge(stationinfo.all,allppfinal, by="USI")
names(specinfo.all)
summary(specinfo.all)

#calculate relative abundance and biovol
specinfo.all$relabund<-specinfo.all$abundance/specinfo.all$abund.all
specinfo.all$relbiovol<-specinfo.all$biovolume/specinfo.all$biovol.all

#get total number of samples
sampN<-length(unique(specinfo.all$USI))

# get species specific information across samples
specinfo.all<- ddply(specinfo.all, .(phylum, class, order, genus, species,specname), summarise, 
                      occupancy= length(USI)/sampN,#ratio presence/total sample
                      first= min(julian),#first occurrence in a year
                      last= max(julian),#last occurrence in a year
                      veg.period=last-first,#length of occurrence in a year
                      mean.vol=mean(mean.size,na.rm=T),#arithmetic average cell size
                      sd.vol=sd(mean.size,na.rm=T),#sd of previous
                      mean.relabu=mean(relabund),#average proportion to abundance
                      sd.relabu=sd(relabund,na.rm=T),#sd of previous
                      mean.relbiov=mean(relbiovol,na.rm=T),# average proportion to biovolume
                      sd.relbiov=sd(relbiovol,na.rm=T))#sd of previous

specinfo.all$LN.size<-log(specinfo.all$mean.vol)
specinfo.all$sqrt.relabu<-sqrt(specinfo.all$mean.relabu)
specinfo.all$sqrt.relvol<-sqrt(specinfo.all$mean.relbiov)
specinfo.all$sqrt.occ<-sqrt(specinfo.all$occupancy)
summary(specinfo.all$specname)

# quick check
pairs.panels(specinfo.all[,c("veg.period","LN.size","sqrt.relabu","sqrt.relvol","sqrt.occ")])
names(include)

# get a data set with single measurements
vol.all<-allppfinal[,c(3:9,123:125,135,138,140:169)]
summary(vol.all)

# format into long format
vol.all<- melt(vol.all,
                id.vars=c("USI","stationID","date","year","julian",
                          "phylum","class","order","genus",
                          "species", "specname","specname.unique"),
                variable.name="no",
                value.name="cell.vol")
vol.all<-vol.all[!is.na(vol.all$cell.vol),]
vol.all<-vol.all[(vol.all$cell.vol>0),]

summary(vol.all)

vol.all$LN.cell.vol<-log(vol.all$cell.vol)
vol.all$no<-NULL

# a quick check
plot(LN.cell.vol~date,vol.all)
plot(LN.cell.vol~julian,vol.all)

## statistics for subset 2 (species level)
data.all <- vol.all


#create a factorial identifier for year
data.all$yearID<-as.factor(data.all$year)

# to reflect the cyclic nature of seasonality, transfrom
# julian day into a variable of "summerness"
data.all$jul2<-data.all$julian
data.all$jul2[data.all$jul2>183]<-365-data.all$julian[data.all$jul2>183]

#check classes and create a color gradient
unique(data.all$class)
levels(data.all$class)
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(20)
plot(1:20,col=mycolors,cex=5,pch=17)


# FULL MODEL
# with year ID
names(data.all)
summary(data.all)
mod.all.full.SID.YID<-lmer(LN.cell.vol~year+jul2
                            +(1|phylum/class/order/specname.unique)
                            +(1|yearID)
                            +(1|stationID),
                            data=data.all)
summary(mod.all.full.SID.YID)
tab_model(mod.all.full.SID.YID, digits=4)

plot_model(mod.all.full.SID.YID, type = "pred", terms = c("year","jul2"))
plot.all.full.SID.YID<-plot_model(mod.all.full.SID.YID, type = "pred", terms = c("year"),title="")
plot.all.full.SID.YID
fig.all.full.SID.YID<-plot.all.full.SID.YID+theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("Year")+
  theme(legend.position = "none")+
  geom_jitter(data=data.all,aes(x=year,y=LN.cell.vol,col=class),size=.1,alpha=.1)+
  scale_color_manual(values = data.all$color) +
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
fig.all.full.SID.YID


#redo analyses with mean cell volumes per sample
allppfinal$jul2<-allppfinal$julian
allppfinal$jul2[allppfinal$jul2>183]<-365-allppfinal$julian[allppfinal$jul2>183]
allppfinal$yearID<-as.factor(allppfinal$year)
#with year ID
mod.all.mean.SID.YID<-lmer(log(mean.size)~year+jul2
                            +(1|phylum/class/order/specname.unique)
                            +(1|yearID)
                            +(1|stationID),
                            data=allppfinal)
summary(mod.all.mean.SID.YID)

acf(residuals(mod.all.mean.SID.YID))

#FOR APPENDIX
tab_model(mod.all.mean.SID.YID,digits = 3)



###################################
## Community weighted means  all ##
###################################

names(allppfinal)
#create mean values per species and sample
means.all<-ddply(allppfinal, .(stationID, date,USI,phylum, class, order, genus, species,specname,specname.unique), summarise, 
                  mean.ar= mean(mean.size, na.rm = T),
                  mean.gm= mean(log(mean.size), na.rm = T))
summary(means.all)
names(means.all)
#merge with mean values from mean dimensions
test.all<-merge(means.all,allppfinal[,c("USI","genus","species","specname.unique","mean.size","abundance","biovolume")], by=c("USI","genus","species","specname.unique"), all=FALSE)
#these should be pretty linear
plot(mean.ar~mean.size,test.all)
plot(mean.gm~log(mean.size),test.all)
hist(test.all$mean.gm)
hist(log(test.all$mean.size))

names(stationinfo.all)

weights.all<-merge(test.all,stationinfo.all, by="USI")
weights.all$relabund<-weights.all$abundance/weights.all$abund.all
weights.all$relbiovol<-weights.all$biovolume/weights.all$biovol.all

numspec.all <-
  weights.all %>%
  dplyr::group_by(USI) %>%
  dplyr::summarize(N= length(USI))
hist(numspec.all$N)
weights.all<-merge(weights.all,numspec.all, by="USI")
weights.all<-weights.all[weights.all$N>10,]
weights.all$year<-year(weights.all$date)
weights.all$lnsize<-log(weights.all$mean.size)

summary(weights.all)

pp.cwm.all <-
  weights.all %>%
  dplyr::group_by(stationID,date) %>%
  dplyr::summarize(N= length(USI),
                   cwm_abu = weighted.mean(mean.ar, relabund,na.rm=T), 
                   cwm_vol = weighted.mean(mean.ar, relbiovol,na.rm=T),
                   ln_cwm_abu = weighted.mean(mean.gm, relabund,na.rm=T), 
                   ln_cwm_vol = weighted.mean(mean.gm, relbiovol,na.rm=T),
                   cwsd_abu = wtd.var(mean.ar, relabund,na.rm=T), 
                   cwsd_vol = wtd.var(mean.ar, relbiovol,na.rm=T),
                   ln_cwsd_abu = wtd.var(mean.gm, relabund,na.rm=T), 
                   ln_cwsd_vol = wtd.var(mean.gm, relbiovol,na.rm=T))


names(pp.cwm.all)
pp.cwm.all$cwsd_abu[pp.cwm.all$cwsd_abu=="Inf"]<-NA
pp.cwm.all$cwsd_vol[pp.cwm.all$cwsd_vol=="Inf"]<-NA
pp.cwm.all$ln_cwsd_abu[pp.cwm.all$ln_cwsd_abu=="Inf"]<-NA
pp.cwm.all$ln_cwsd_vol[pp.cwm.all$ln_cwsd_vol=="Inf"]<-NA

pp.cwm.all$year<-year(pp.cwm.all$date)
pp.cwm.all$julian<-yday(pp.cwm.all$date)
pp.cwm.all$yearID<-as.factor(pp.cwm.all$year)

pairs.panels((pp.cwm.all[,c(3:13)]),method="spearman")

summary(pp.cwm.all)
names(pp.cwm.all)


env.cwm.all<-merge(pp.cwm.all,Env_data, by=c("stationID","date"))



size.cwm<-ggplot(pp.cwm.all, 
                 aes(date,ln_cwm_abu))+
  geom_jitter(alpha=.1)+
  #geom_violin(alpha=.21)+
  geom_smooth(method="lm")+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Community weighted mean cell size [LN µm³]")+
  xlab("year")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

size.cwm




violin.cwm<-ggplot(pp.cwm.all, 
                   aes(as.factor(year),ln_cwm_abu))+
  geom_violin(alpha=.21)+
  #geom_smooth()+
  theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("year")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())#+
#facet_wrap(~stat)

violin.cwm

density.incl<-ggplot(data, 
                     aes(LN.cell.vol))+
  geom_density(col="black",fill="steelblue",alpha=.3)+
  theme_bw()+
  xlab("Cell size [LN µm³]")+
  ylab("Density")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

density.incl

density.all<-ggplot(data.all, 
                     aes(x=LN.cell.vol))+
  theme_bw()+
  geom_density(col="black",fill="darkred",alpha=.5,lty=2)+
  xlab("Cell size [LN µm³]")+
  ylab("Density")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

density.all


density.both<-ggplot(data, 
                   aes(LN.cell.vol))+
  geom_density(col="black",fill="white",alpha=1,size=1)+
  geom_density(data=data.all,aes(x=LN.cell.vol),col="darkred",fill="darkred",alpha=.5)+
  theme_bw()+
  xlab("Cell size [LN µm³]")+
  ylab("Density")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

density.both


pp.cwm.all$jul2<-pp.cwm.all$julian
pp.cwm.all$jul2[pp.cwm.all$jul2>183]<-365-pp.cwm.all$julian[pp.cwm.all$jul2>183]
env.cwm.all$jul2<-env.cwm.all$julian
env.cwm.all$jul2[env.cwm.all$jul2>183]<-365-env.cwm.all$julian[env.cwm.all$jul2>183]


test3.all<-lmer(ln_cwm_abu~year+jul2
            +(1|stationID)
            +(1|yearID),
            data=pp.cwm.all)


tab_model(test3.all,digits=3)

names(env.cwm.all)


envmod2.all<-lmer(ln_cwm_abu~temperature+
                log(total.n)+
                log(suspended.particulates)+
                log(total.p)+
                log(silicon)+
                pH+
                salinity+
                (1|stationID)+(1|yearID),
              data=env.cwm.all)

summary(envmod2.all)
tab_model(test3.all,envmod2.all, digits =3)

tab_model(test3,test3.all,
          envmod2, envmod2.all, digits =3)










obj<-plot_model(test3, type = "pred", pred.type="re",terms = c("year"),title="")
obj
obj2<-plot_model(envmod2, type = "pred", pred.type="re",terms = c("temperature"),title="")
obj2
obj3<-plot_model(envmod2, type = "pred", pred.type="re",terms = c("total.p"),title="")
obj3





mod.fig3<-obj+theme_bw()+
  ylab("CWM cell size [LN µm³]")+
  xlab("Year")+
  geom_point(data=pp.cwm.all,aes(x=year,y=ln_cwm_abu,alpha=jul2-.8,col=stationID))+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

mod.fig3


mod.fig4<-obj2+theme_bw()+
  ylab("CWM cell size [LN µm³]")+
  xlab("Temperature [°C]")+
  geom_jitter(data=env.cwm.all,aes(x=temperature,y=ln_cwm_abu,alpha=jul2),col="blue")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(legend.position = "none")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

mod.fig4






#check how much biovolume is in the not.to.species
names(stationinfo.all)
names(stationinfo.incl)

prop<-merge(stationinfo.all,stationinfo.incl,by="USI")
prop$p<-prop$biovol.incl/prop$biovol.all
hist(prop$p)
summary(prop)

names(Env_data)
Env_data$Co<-NA
Env_data$Co<-"NL"
Env_data$Co[Env_data$stationID=="Bork_W_1"]<-"DE"
Env_data$Co[Env_data$stationID=="Nney_W_2"]<-"DE"
Env_data$Co[Env_data$stationID=="JaBu_W_1"]<-"DE"
Env_data$Co[Env_data$stationID=="WeMu_W_1"]<-"DE"
Env_data$Co<-as.factor(Env_data$Co)
summary(Env_data)


Env_data2<-Env_data
Env_data2$excl<-NA
Env_data2$excl[Env_data2$stationID=="BOCHTVWTM"]<-"out"
Env_data2$excl[Env_data2$stationID=="TERSLG100"]<-"out"
Env_data2$excl[Env_data2$stationID=="TERSLG135"]<-"out"
Env_data2$excl[Env_data2$stationID=="TERSLG175"]<-"out"
Env_data2$excl[Env_data2$stationID=="TERSLG235"]<-"out"
Env_data2$excl[Env_data2$stationID=="TERSLG50"]<-"out"
Env_data2$excl[Env_data2$stationID=="ZUIDOLWOT"]<-"out"
Env_data2<-Env_data2[is.na(Env_data2$excl),]



mycolors3<-colorRampPalette(brewer.pal(8, "Spectral"))(20)
plot(1:20,col=mycolors3,pch=16, cex=2)
unique(Env_data2$stationID)
site.colors <- c(Bork_W_1=mycolors3[1], 
                  Nney_W_2=mycolors3[5], 
                  JaBu_W_1=mycolors3[6], 
                  WeMu_W_1=mycolors3[3], 
                  BOOMKDP=mycolors3[8], 
                  DANTZGT=mycolors3[12], 
                  DOOVBWT=mycolors3[11], 
                  GROOTGND=mycolors3[13], 
                  HUIBGOT=mycolors3[14], 
                  MARSDND=mycolors3[15], 
                  ROTTMPT3=mycolors3[16], 
                  ROTTMPT50=mycolors3[17], 
                  ROTTMPT70=mycolors3[18],
                  TERSLG4=mycolors3[19],
                  TERSLG10=mycolors3[20])



temperature<-ggplot(Env_data2, 
                     aes(date,temperature,col=Co,shape=Co))+
  geom_point(alpha=.1)+
  #geom_smooth(se=FALSE)+
  theme_bw()+
  xlab("Time")+
  ylab("Temperature°C")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

temperature


TN<-ggplot(Env_data2, 
                    aes(date,log(total.n),col=stationID))+
  geom_point(alpha=.2)+
  geom_smooth(se=FALSE)+
  theme_bw()+
  xlab("Time")+
  ylab("LN Total N (µM)")+ylim(2,7)+
  scale_color_manual(values = site.colors) +
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(legend.position="none")

TN


TP<-ggplot(Env_data2, 
           aes(date,log(total.p),col=stationID))+
  geom_point(alpha=.2)+
  geom_smooth(se=FALSE)+
  theme_bw()+
  scale_color_manual(values = site.colors) +
  xlab("Time")+
  ylab("LN Total P (µM)")+ylim(-2,3)+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(legend.position="none")

TP

Env_data2$NP<-Env_data2$total.n/Env_data2$total.p


NP<-ggplot(Env_data2, 
           aes(date,log(NP),col=stationID))+
  geom_point(alpha=.2)+
  geom_smooth(se=FALSE)+
  theme_bw()+
  scale_color_manual(values = site.colors) +
  geom_hline(yintercept=log(16),col="black",size=1)+
  geom_hline(yintercept=log(22),col="darkgrey",size=1)+
  xlab("Time")+
  ylab("LN molar N:P")+ylim(1,5)+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(legend.position = "none")

NP



salinity<-ggplot(Env_data, 
           aes(date,salinity,col=Co,shape=Co))+
  geom_point(alpha=.1)+
  #geom_smooth(se=FALSE)+
  theme_bw()+
  xlab("Time")+
  ylab("Salinity [PSU]")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

salinity



pH<-ggplot(Env_data, 
                 aes(date,pH,col=Co,shape=Co))+
  geom_point(alpha=.1)+
  #geom_smooth(se=FALSE)+
  theme_bw()+
  xlab("Time")+
  ylab("pH")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

pH


silicon<-ggplot(Env_data, 
            aes(date,log(silicon),col=Co,shape=Co))+
  geom_point(alpha=.1)+
  #geom_smooth(se=FALSE)+
  theme_bw()+
  xlab("Time")+
  ylab("log Si [µM]")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

silicon

spm<-ggplot(Env_data, 
                 aes(date,log(suspended.particulates),col=Co,shape=Co))+
  geom_point(alpha=.1)+
  #geom_smooth(se=FALSE)+
  theme_bw()+
  xlab("Time")+
  ylab("SPM")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

spm






xyplot(temperature~date|stationID,Env_data)
Env_data$NP<-Env_data$total.n/Env_data$total.p
xyplot(log(NP)~date|stationID,Env_data)


env_de<-Env_data[Env_data$Co=="DE",]

temp.de<-ggplot(env_de, 
                    aes(date,temperature))+
  geom_point(aes(col=stationID),alpha=.1)+
  geom_smooth(se=FALSE)+
  theme(legend.position = "none")+
  theme_bw()+
  xlab("Time")+
  ylab("Temperature°C")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE)

temp.de

TN.de<-ggplot(env_de, 
           aes(date,log(total.n)))+
  geom_point(aes(col=stationID),alpha=.1)+
  geom_smooth(se=FALSE)+
  theme(legend.position = "none")+
  theme_bw()+
  xlab("Time")+
  ylab("LN Total N (µM)")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE)

TN.de


TP.de<-ggplot(env_de, 
           aes(date,log(total.p)))+
  geom_point(aes(col=stationID),alpha=.1)+
  geom_smooth(se=FALSE)+
  theme(legend.position = "none")+
  theme_bw()+
  xlab("Time")+
  ylab("LN Total P (µM)")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE)

TP.de


NP.de<-ggplot(env_de, 
           aes(date,log(NP)))+
  geom_point(aes(col=stationID),alpha=.1)+
  geom_smooth(se=FALSE)+
  theme(legend.position = "none")+
  theme_bw()+
  geom_hline(yintercept=log(16),col="black")+
  geom_hline(yintercept=log(22),col="grey")+
  xlab("Time")+
  ylab("LN molar N:P")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE)

NP.de



salinity.de<-ggplot(env_de, 
                 aes(date,salinity))+
  geom_point(aes(col=stationID),alpha=.1)+
  geom_smooth(se=FALSE)+
  theme(legend.position = "none")+
  theme_bw()+
  xlab("Time")+
  ylab("Salinity [PSU]")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE)

salinity.de



pH.de<-ggplot(env_de, 
           aes(date,pH))+
  geom_point(aes(col=stationID),alpha=.1)+
  geom_smooth(se=FALSE)+
  theme_bw()+
  xlab("Time")+
  ylab("pH")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(legend.position = c(.25,.25))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

pH.de


silicon.de<-ggplot(env_de, 
                aes(date,log(silicon)))+
  geom_point(aes(col=stationID),alpha=.1)+
  geom_smooth(se=FALSE)+
  theme(legend.position = "none")+
  theme_bw()+
  xlab("Time")+
  ylab("log Si [µM]")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE)

silicon.de

spm.de<-ggplot(env_de, 
            aes(date,log(suspended.particulates)))+
  geom_point(aes(col=stationID),alpha=.1)+
  geom_smooth(se=FALSE)+
  theme(legend.position = "none")+
  theme_bw()+
  xlab("Time")+
  ylab("SPM")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE)

spm.de



#Figures 

tiff(file = "~/P2021_5_pp_size/fig1.tiff", width = 2800, height = 3600, units = "px", res = 400)
cowplot::plot_grid(density.both,raw.incl.final,
                   nrow=2,
                   align="v",
                   labels = c('A', 'B'))

dev.off()

tiff(file = "~/P2021_5_pp_size/fig2.tiff", width = 4800, height = 2000, units = "px", res = 400)
cowplot::plot_grid(spec.lmer.slope.size,spec.lmer.slope.relabu,
                   ncol=2, nrow=1,
                   align="h",
                   labels = c('A', 'B'))
dev.off()

tiff(file = "~/P2021_5_pp_size/fig3_new.tiff", width = 4800, height = 2400, units = "px", res = 400)
selectspec
dev.off()


tiff(file = "~/P2021_5_pp_size/fig4.tiff", width = 3600, height = 3200, units = "px", res = 400)
cowplot::plot_grid(cwm.incl.final,temp.raw.incl,
                   totalp.raw.incl,totaln.raw.incl,
                   ncol=2, nrow=2,
                   align="hv",
                   labels = c('A', 'B','C', 'D'))

dev.off()



tiff(file = "~/P2021_5_pp_size/app_env.tiff", width = 6400, height = 3200, units = "px", res = 400)
cowplot::plot_grid(temp.de,salinity.de,pH.de,spm.de,
                   TN.de,TP.de,NP.de,silicon.de,
                   ncol=4, nrow=2,
                   align="hv",
                   labels = c('A', 'B','C', 'D','E', 'F','G', 'H'))

dev.off()


tiff(file = "NPNLWKN.tiff", width = 3600, height = 3200, units = "px", res = 400)
cowplot::plot_grid(TN,TP,NP,
                   ncol=2, nrow=2,
                   align="hv",
                   labels = c('A', 'B','C'))

dev.off()


### check some sensitivity test

# full model species level with and without year ID
tab_model(mod.incl.full.SID.YID, mod.incl.full.SID, digits=4)
# full model species level with and without 2006, no year ID
tab_model(mod.incl.full.SID, mod.incl.no2006.SID, digits=4)
# full model species level with and without 2006, with year ID
tab_model(mod.incl.full.SID.YID, mod.incl.no2006.SID.YID, digits=4)
tab_model(mod.incl.full.SID.YID, mod.incl.full.SID, 
          mod.incl.no2006.SID.YID,mod.all.mean.SID.YID,
          mod.all.full.SID.YID,digits=3)

# full model species level and all
tab_model(mod.incl.full.SID.YID, mod.all.mean.SID.YID, digits=4)



tab_model(mod.incl.no2006.SID.YID, digits=4)



names(include)
stationinfo.map<- ddply(include, .(stationID), summarise, 
                         n.samp= length(unique(USI)),
                         min.y= min(year, na.rm = T),
                         max.y= max(year, na.rm = T))
summary(stationinfo.map)
latlong <- read_delim("~/Large Data Sets/monitoring_data_raw/stations_nlwkn_lat_long.csv", 
                                      ";", escape_double = FALSE, trim_ws = TRUE)
summary(latlong)
unique(latlong$stationID)
unique(stationinfo.map$stationID)

latlong<-merge(latlong,stationinfo.map,by="stationID",all=TRUE)
latlong$pch<-as.factor(latlong$pch)
min(latlong$Latitude)
max(latlong$Latitude)
max(latlong$Longitude)
min(latlong$Longitude)

german <- map_data("worldHires")
map<-ggplot() + geom_polygon(data = german, aes(x=long, y = lat, group = group)) + 
  coord_fixed(xlim = c(6.5, 8.5),  ylim = c(53, 54), ratio = 1.3)
map

library(sp)
library(ggrepel)
gadm <- readRDS("~/Large Data Sets/monitoring_data_raw/gadm36_DEU_0_sp.rds")
map<-ggplot() + geom_polygon(data = gadm, aes(x=long, y = lat, group = group)) + 
  coord_fixed(xlim = c(6.5, 9),  ylim = c(53.25, 54), ratio = 1.3)+
  geom_point(data=latlong[latlong$pch=="env",], aes(Longitude, Latitude), shape=16, col="red",size=5)+
  geom_point(data=latlong[latlong$pch!="env",], aes(Longitude, Latitude), shape=17, col="orange",size=3)
#geom_text_repel(data=latlong,aes(Longitude, Latitude,label=nr),col="orange") 
  
map


#meta-amalysis
names(fulldat.incl)
library(metafor)

rma(yi=lmer.slp,sei=lmer.se,
    data=fulldat.incl)
rma(yi=slp,sei=se.slp,data=fulldat.incl)

#slope for temperature increase

env_de$year<-year(env_de$date)
summary(lm(temperature~year, env_de))

summary(lm(log(total.p)~year, env_de))  
sd(env_de$NP,na.rm=T)
