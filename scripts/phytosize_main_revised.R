### cell size of coastal phytoplankton

# based on data files by Claus Dürselen
# script by Helmut Hillebrand started 20210104
# major iteration 20210215, 20210312


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

## primary data

# read data
allppfinal <- read_delim("allppfinal.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)

allppfinal$stationID<-as.factor(allppfinal$stationID)
allppfinal$include<-as.factor(allppfinal$include)
allppfinal$phylum<-as.factor(allppfinal$phylum)
allppfinal$class<-as.factor(allppfinal$class)
allppfinal$order<-as.factor(allppfinal$order)
allppfinal$genus<-as.factor(allppfinal$genus)

# according to NLWKN, some stations changed names

unique(allppfinal$stationID)
allppfinal$stationID[allppfinal$stationID=="Au_We_85"]<-"WeMu_W_1" 
allppfinal$stationID[allppfinal$stationID=="AuWe_W_1"]<-"WeMu_W_1" 
allppfinal$stationID[allppfinal$stationID=="EmDo_MZB_2"]<-"Ems_MZB_2" 
allppfinal$stationID[allppfinal$stationID=="Ems"]<-"Ems_MZB_2" 
allppfinal$stationID<-as.factor(allppfinal$stationID)
unique(allppfinal$stationID)


# a few species were identified which are zooplankton

allppfinal$exclude<-NA

allppfinal$exclude[allppfinal$genus=="Noctiluca"]<-"het"
allppfinal$exclude[allppfinal$genus=="Mesodinium"]<-"het"
allppfinal$exclude[allppfinal$genus=="Ebria"]<-"het"
summary(as.factor(allppfinal$exclude))

allppfinal<-allppfinal[is.na(allppfinal$exclude),]

#remove the rows with sums (as we will calculate them ourselves)
allppfinal$exclude[allppfinal$include=="sum"]<-"sum"
allppfinal<-allppfinal[is.na(allppfinal$exclude),]

# create a unique sample identifier
allppfinal$USI<-do.call(paste, c(allppfinal[c("stationID", "date")], sep = "_"))
allppfinal$USI<-as.factor(allppfinal$USI)

# convert the date to an R-readable date, add Julian Day
allppfinal$date<-dmy(allppfinal$date)
allppfinal$julian <- yday(allppfinal$date)

names(allppfinal)

# subsetting data in two branches
# subset 1: all taxa identified ==> ppsum
# subset 2: only taxa identified to species ==> include

# subset 1
# here only use the mean measured volumes 
# (cannot be sure about species identity)
ppsum<-allppfinal[,c(1:19)]
ppsum$julian<-allppfinal$julian

# remove taxa without biovolume information
summary(ppsum)
ppsum<-ppsum[!is.na(ppsum$cell.vol),]

# to get weighted means, sum per sample & the relative proportion of 
# species to total abundance and biomass are calculated

ppsum.total<- ddply(ppsum, .(USI), summarise, 
                    abund.incl= sum(abundance, na.rm = T),
                    biovol.incl= sum(biovol, na.rm = T))

ppsum<-merge(ppsum,ppsum.total, by="USI")

ppsum$relabund<-ppsum$abundance/ppsum$abund.incl
ppsum$relbiovol<-ppsum$biovol/ppsum$biovol.incl


# subset 2
include<-allppfinal[allppfinal$include=="yes",]

names(include)

include<-include[include$N.vols!=0,]


# to get weighted means, sum per sample & the relative proportion of 
# species to total abundance and biomass are calculated


# create a somewhat smaller data set for station and species info
mean.USI<-include[,c(3:21,112:113,145)]

# create sum abundance and biovol for the subset assemblage
stationinfo<- ddply(mean.USI, .(USI), summarise, 
                 abund.incl= sum(abundance, na.rm = T),
                 biovol.incl= sum(biovol, na.rm = T))


# create a file to get species specific averages
specinfo<-merge(stationinfo,mean.USI, by="USI")
names(specinfo)
summary(specinfo)

#calculate relative abundance and biovol
specinfo$relabund<-specinfo$abundance/specinfo$abund.incl
specinfo$relbiovol<-specinfo$biovol/specinfo$biovol.incl

#get total number of samples
sampN<-length(unique(specinfo$USI))

# get species specific information across samples
specinfo<- ddply(specinfo, .(phylum, class, order, genus, species), summarise, 
            occupancy= length(USI)/sampN,#ratio presence/total sample
            first= min(julian),#first occurrence in a year
            last= max(julian),#last occurrence in a year
            veg.period=last-first,#length of occurrence in a year
            mean.vol=mean(cell.vol,na.rm=T),#arithmetic average cell size
            sd.vol=sd(cell.vol,na.rm=T),#sd of previous
            mean.relabu=mean(relabund),#average proportion to abundance
            sd.relabu=sd(relabund,na.rm=T),#sd of previous
            mean.relbiov=mean(relbiovol,na.rm=T),# average proportion to biovolume
            sd.relbiov=sd(relbiovol,na.rm=T),#sd of previous
            mean.dim.meas=mean(N.dim.meas,na.rm=T),#average number of dimensions measured per sample
            sum.dim.meas=sum(N.dim.meas,na.rm=T),#total number of dimensions measured across sample
            mean.cells.meas=mean(N.vols,na.rm=T),#average number of cells measured per sample
            sum.cells.meas=sum(N.vols,na.rm=T))#total number of cells measured across sample


specinfo$LN.size<-log(specinfo$mean.vol)
specinfo$sqrt.relabu<-sqrt(specinfo$mean.relabu)
specinfo$sqrt.relvol<-sqrt(specinfo$mean.relbiov)
specinfo$sqrt.occ<-sqrt(specinfo$occupancy)

# quick check
pairs.panels(specinfo[,c(9,20:23,19)])

# add species name
specinfo$specname<-do.call(paste, c(specinfo[c("genus", "species")], sep = "_"))
specinfo$specname<-as.factor(specinfo$specname)
summary (specinfo)

# add species name also to include data set
include$specname<-do.call(paste, c(include[c("genus", "species")], sep = "_"))
include$specname<-as.factor(include$specname)

# get a data set with single measurements
vol<-include[,c(3:6,9:13,145:146,114:142)]
summary(vol)

# format into long format
vol<- melt(vol,
           id.vars=c("USI","program","stationID","date",
                     "phylum","class","order","genus",
                     "species", "specname"),
           variable.name="no",
           value.name="cell.vol")
vol$cell.vol<-(as.numeric(vol$cell.vol))
vol<-vol[!is.na(vol$cell.vol),]
summary(vol)

vol$LN.cell.vol<-log(vol$cell.vol)
vol$program<-as.factor(vol$program)
vol$stationID<-as.factor(vol$stationID)
vol$phylum<-as.factor(vol$phylum)
vol$class<-as.factor(vol$class)
vol$order<-as.factor(vol$order)
vol$genus<-as.factor(vol$genus)
vol$species<-as.factor(vol$species)
vol$no<-NULL
vol$year<-year(vol$date)
vol$julian <- yday(vol$date)
summary(vol)

names(vol)

# a quick check
plot(LN.cell.vol~date,vol)
plot(cell.vol~julian,vol)

#check for some really low values

## statistics for subset 2 (species level)

 

data <- vol

#correct a NA in phylum
unique(data$phylum) 
summary(data[is.na(data$phylum),])
data$phylum[is.na(data$phylum)]<-"Dinophyta"
unique(data$phylum) 
names(data)
data$yearID<-as.factor(data$year)

# to reflect the cyclic nature of seasonality, transfrom
# julian day into a variable of "summerness"

data$jul2<-data$julian
data$jul2[data$jul2>183]<-365-data$julian[data$jul2>183]

#check classes and create a color gradient
unique(data$class)
data$class[data$class=="Bacillariophyceae incertae sedis"]<-"Bacillariophyceae"
data$class<-as.factor(data$class)

mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(12)
plot(1:12,col=mycolors,cex=5,pch=17)

data$color<-NA
data$color[data$class=="Bacillariophyceae"]<-mycolors[4]
data$color[data$class=="Coscinodiscophyceae"]<-mycolors[5]
data$color[data$class=="Fragilariophyceae"]<-mycolors[6]
data$color[data$class=="Dinophyceae"]<-mycolors[2]
data$color[data$class=="Euglenophyceae"]<-mycolors[7]
data$color[data$class=="Pyramimonadophyceae"]<-mycolors[11]
data$color[data$class=="Dictyochophyceae"]<-mycolors[3]
data$color[data$class=="Cyanophyceae"]<-mycolors[1]
data$color[data$class=="Trebouxiophyceae"]<-mycolors[9]
data$color[data$class=="Chlorophyceae"]<-mycolors[8]
data$color[data$class=="Coccolithophyceae"]<-mycolors[12]


summary(as.factor(data$color))

mycolors2<-as.character("#7E6EA2","#7E6EA2", "#7D8F31","#E0A604",
                        "#B3499C","#1B9E77","#A0A811","#666666",
                        "#93752C","#CF3F76","#B78415", "#8E7037")

#check order
(data[is.na(data$order),])

data$order[is.na(data$order)]<-"Chaetocerotales"   
data$order<-as.factor(data$order)
summary(data)

names(data)
pairs.panels(data[,c(12:13,16)])
# FULL MODEL
# without year ID
test1a<-lmer(LN.cell.vol~year+jul2
             +(1|phylum/class/order/genus/species)
             +(1|stationID),
             data=data)

summary(test1a)

test1b<-lmer(LN.cell.vol~year+jul2
            +(1|phylum/class/order/genus/species)
            +(1|yearID)
            +(1|stationID),
            data=data)

summary(test1b)

tab_model(test1b, digits=3)

  
plot_model(test1b, type = "pred", terms = c("year","jul2"))
plottest1b<-plot_model(test1b, type = "pred", terms = c("year"),title="")
plottest1b
mod.fig2<-plottest1b+theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("Year")+
  theme(legend.position = "none")+
  geom_jitter(data=data,aes(x=year,y=LN.cell.vol,col=class),size=.1,alpha=.1)+
  scale_color_manual(values = mycolors,
                     labels= c("Cyanophyceae","Dinophyceae",
                               "Dictyochophyceae",
                               "Coccolithophyceae",
                               "Bacillariophyceae",
                               "Coscinodiscophyceae",
                               "Fragilariophyceae","Chlorophyceae",
                               "Euglenophyceae","Pyramimonadophyceae",
                               "Trebouxiophyceae",
                               "Bacillariophyceae incertae sedis")) +
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

mod.fig2

acf(residuals(test1b))



#redo analyses with mean cell volumes per sample

summary(mean.USI)
data3<-(mean.USI[!is.na(mean.USI$cell.vol),])
data3$jul2<-data3$julian
data3$jul2[data3$jul2>183]<-365-data3$julian[data3$jul2>183]
data3$year<-year(data3$date)
data3$yearID<-as.factor(data3$year)

test1c<-lmer(log(cell.vol)~year+jul2
             +(1|phylum/class/order/genus/species)
             +(1|yearID)
             +(1|stationID),
             data=data3)

summary(test1c)
tab_model(test1c,digits = 4)
AIC(test1,test2,test1b)
tab_model(test1b,test1c,digits = 3)


## species specific models over time


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
  if(dim(temp)[1]>2){
    if((max(temp$year)-min(temp$year))>2){#does the next step only if at least 3 data points are present
    lm1<-lm(LN.cell.vol~year, temp)#makes a linear regreassion
    icpt <- coef(summary(lm1))[1, 1]#selects the slope
    slp <- coef(summary(lm1))[2, 1]#selects the slope
    se.slp<- coef(summary(lm1))[2, 2]#selects its standard error
    p<-anova(lm1)$'Pr(>F)'[1]#gives the p-value
    N<-dim(temp)[1]
    median<-median(temp$cell.vol)
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

appendix.tab1<-slope.year[,c(1,3,4,5,6,10,11)]
names(appendix.tab1)
appendix.tab1 <- appendix.tab1[order(appendix.tab1$specname),]
appendix.tab1<-appendix.tab1 %>% mutate_if(is.numeric, round, digits = 4)
kbl(appendix.tab1, caption = "") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F) %>%
  add_header_above(c(" ", "Linear Model" = 4, "Mixed Model" = 2))

hist(slope.year$slp)



fulldat<-merge(specinfo,slope.year, by="specname")

names(fulldat)
fulldat$median.size<-log(fulldat$median)
fulldat$col<-NA
fulldat$col[fulldat$p>.05]<-.2
fulldat$col[fulldat$p<.05]<-.6
summary(fulldat)

fulldat$pch1<-abs(fulldat$slp/fulldat$se.slp)
fulldat$pch2<-abs(fulldat$lmer.slp/fulldat$lmer.se)

names(fulldat)
pairs.panels(fulldat[,c(24:26,23,37,27,28,33,35)],method="spearman")
plot(slp~lmer.slp,fulldat)
hist(fulldat$lmer.slp)
hist(fulldat$slp)
fulldat$class[fulldat$class=="Bacillariophyceae incertae sedis"]<-"Bacillariophyceae"
unique(fulldat$class)




spec.slope<-ggplot(fulldat, 
                   aes(median.size,slp, 
                       col=class))+
  geom_hline(yintercept=0)+
  geom_point(alpha=fulldat$col,
             size=10*fulldat$sqrt.relabu+.5)+
  theme_bw()+
  ylab("Slope LN size~time")+
  xlab("Median cell size [log µm³]")+
  scale_color_manual(values = c(mycolors[5],mycolors[6],mycolors[2],mycolors[9],mycolors[7]))+   
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
  #+facet_wrap(~phylum, scales="free")

spec.slope


spec.lmer.slope<-ggplot(fulldat, 
                   aes(sqrt.relabu,lmer.slp, 
                       col=class))+
  geom_hline(yintercept=0)+
  geom_point(alpha=fulldat$col,size=fulldat$pch2)+
  theme_bw()+
  scale_color_manual(values = c(mycolors[5],mycolors[6],mycolors[2],mycolors[9],mycolors[7]))+   
  ylab("Fixed effect LN size~year")+
  xlab("Relative abundance [sqrt-transf.]")+ylim(-.25,.2)+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
#+facet_wrap(~phylum, scales="free")

spec.lmer.slope




## Violin plot
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


names(data3)
violin.samplemean<-ggplot(data3, 
                   aes(as.factor(year),log(cell.vol)))+
  geom_violin(col="black",fill="steelblue")+
  stat_summary(mapping = aes(x=as.factor(year),
                             y=log(cell.vol)),
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


## Overall model with environment
allenv <- read_csv("allenv.csv")
names(allenv)
unique(data$stationID)
unique(allenv$StationID)
allenv$stationID<-allenv$StationID

year.data<- ddply(data, .(phylum, class, order, genus, species,year,stationID), summarise, 
                    mean= mean(cell.vol, na.rm = T),
                    sd= sd(cell.vol, na.rm = T),
                    cv=sd/mean,
                    lnmean= mean(log(cell.vol), na.rm = T),
                    lnsd= sd(log(cell.vol), na.rm = T),
                    lnvc=lnsd/lnmean)

summary(year.data)

pairs.panels(year.data[,8:13])
env<-merge(allenv,year.data, by=c("stationID","year"),all=F)

summary(env)
unique(env$stationID)

names(env)

env$yearID<-as.factor(env$year)
pairs.panels(env[,c(4:12,23,25,26,28)],method="spearman")
env<-env[env$temperature>8,]
envmod1<-lmer(lnmean~log(total.p)+log(total.n)+
                log(silicon)+log(suspended.particulates)+
                pH+salinity+temperature
             +(1|phylum/class/order/genus/species)
             +(1|stationID)
             +(1|yearID),
             data=env)

summary(envmod1)
tab_model(envmod1)







## dominant species plot
#add dominance values to data 

data2<-merge(data,fulldat[c("specname","sqrt.relabu","sqrt.relvol","sqrt.occ","icpt","slp","p","N","lmer.slp","lmer.se")], by="specname", all=FALSE)
unique(data2$specname)
#data2<-data2[!is.na(data2$sqrt.relabu),]
#data2<-data2[!is.na(data2$sqrt.relvol),]
summary(data2)
names(data2)
summary(data2[data2$specname=="Cylindrotheca_closterium",])
data2$dom.abu<-0
data2$dom.vol<-0
data2$dom.occ<-0
data2$dom.abu[data2$sqrt.relabu>quantile(data2$sqrt.relabu,.75)]<-1
data2$dom.vol[data2$sqrt.relvol>quantile(data2$sqrt.relvol,.75)]<-1
data2$dom.occ[data2$sqrt.occ>quantile(data2$sqrt.occ,.75)]<-1

data2$incl<-data2$dom.abu+data2$dom.vol+data2$dom.occ
dominance<-data2[data2$incl>=1,]
summary(dominance)
names(dominance)
dominance<-dominance[dominance$N>3500,]
unique(dominance$specname)
domspec<-ggplot(dominance, 
                   aes(year,LN.cell.vol, col=class))+
  geom_point(alpha=.1)+
  #scale_color_manual(values = c(mycolors[5],mycolors[6],mycolors[2],mycolors[9],mycolors[7]))+   
  geom_smooth(method="loess",col="blue")+
  #geom_smooth(method="lm",col="red")+
  #theme(legend.position="none")+
  theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("year")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  facet_wrap(~specname,scales="free_y")

domspec

summary(data2)
select<-data2[data2$sqrt.relabu>.05,]
select<-select[select$N>500,]
select<-select[which(abs(select$lmer.slp) > .05 | select$phylum!="Bacillariophyta") , ]
select<-select[select$specname!="Chaetoceros_curvisetus",]
select<-select[select$specname!="Cymatosira_belgica",]
select<-select[select$specname!="Rhizosolenia_similoides",]
select<-select[select$specname!="Thalassiosira_punctigera",]
select<-select[select$specname!="Prorocentrum_minimum",]


unique(select$specname)
summary(select)
names(select)
dominance<-dominance[dominance$N>3500,]
unique(dominance$specname)
selectspec<-ggplot(select, 
                aes(year,LN.cell.vol, col=class))+
  geom_point(alpha=.1)+
  scale_color_manual(values = c(mycolors[5],mycolors[6],mycolors[2],mycolors[9],mycolors[7]))+   
  geom_smooth(method="loess",col="blue")+
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
  facet_wrap(~specname,scales="free_y")

selectspec

steep<-data2[data2$p<0.0001,]
steep<-steep[steep$N>4000,]
unique(steep$specname)
summary(steep$specname)

steepspec<-ggplot(steep, 
                aes(year,LN.cell.vol))+
  geom_point(alpha=.1,col="darkgrey")+
  #geom_smooth(method="loess",col="blue")+
  geom_smooth(method="lm",col="red")+
  theme(legend.position="none")+
  theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("year")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  facet_wrap(~specname,scales="free_y")

steepspec


## Community weighted means
names(mean.USI)
means<-ddply(data, .(stationID, date,USI,phylum, class, order, genus, species,specname), summarise, 
      mean.ar= mean(cell.vol, na.rm = T),
      sd.ar= sd(cell.vol, na.rm = T),
      cv.ar=sd.ar/mean.ar,
      mean.gm= mean(log(cell.vol), na.rm = T),
      sd.gm= sd(log(cell.vol), na.rm = T),
      cv.gm=sd.gm/mean.gm)
summary(means)
names(means)
names(mean.USI)
test<-merge(means,mean.USI[,c("USI","genus","species","cell.vol","abundance","biovol")], by=c("USI","genus","species"), all=FALSE)
plot(mean.ar~cell.vol,test)
plot(mean.gm~log(cell.vol),test)
hist(test$mean.gm)
hist(log(test$cell.vol))

names(stationinfo)

weights<-merge(test,stationinfo, by="USI")
weights$relabund<-weights$abundance/weights$abund.incl
weights$relbiovol<-weights$biovol/weights$biovol.incl

numspec <-
  weights %>%
  dplyr::group_by(USI) %>%
  dplyr::summarize(N= length(USI))
hist(numspec$N)
weights<-merge(weights,numspec, by="USI")
weights<-weights[weights$N>10,]
weights$year<-year(weights$date)
weights$lnsize<-log(weights$cell.vol)
weights$stationID<-as.factor(weights$stationID)
summary(weights)

pp.cwm <-
  weights %>%
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


names(pp.cwm)
pp.cwm$cwsd_abu[pp.cwm$cwsd_abu=="Inf"]<-NA
pp.cwm$cwsd_vol[pp.cwm$cwsd_vol=="Inf"]<-NA
pp.cwm$ln_cwsd_abu[pp.cwm$ln_cwsd_abu=="Inf"]<-NA
pp.cwm$ln_cwsd_vol[pp.cwm$ln_cwsd_vol=="Inf"]<-NA

pp.cwm$year<-year(pp.cwm$date)
pp.cwm$julian<-yday(pp.cwm$date)
pp.cwm$yearID<-as.factor(pp.cwm$year)

pairs.panels((pp.cwm[,c(3:13)]),method="spearman")

summary(pp.cwm)
names(pp.cwm)

Env_data <- read_csv("~/R/InterReg-project/C_Chl_ratio/MARISCO_pp_wadden_env.csv")
summary (Env_data)
#rename columns
Env_data$date<-dmy(Env_data$date)
Env_data$stationID<-(Env_data$StationID)
env.cwm<-merge(pp.cwm,Env_data, by=c("stationID","date"))



size.cwm<-ggplot(pp.cwm, 
                    aes(date,ln_cwm_abu))+
  geom_jitter(alpha=.1)+
  #geom_violin(alpha=.21)+
  geom_smooth(method="lm")+
  theme_bw()+
  theme(legend.position = "none")+
  ylab("Cell size [LN µm³]")+
  xlab("year")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

size.cwm




violin.cwm<-ggplot(pp.cwm, 
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




pp.cwm$jul2<-pp.cwm$julian
pp.cwm$jul2[pp.cwm$jul2>183]<-365-pp.cwm$julian[pp.cwm$jul2>183]

test3<-lmer(ln_cwm_abu~year+jul2
            +(1|stationID)
            +(1|yearID),
            data=pp.cwm)

test.abu<-lmer(log(cwm_abu)~year+jul2
            +(1|stationID)
            +(1|yearID),
            data=pp.cwm)
test.ln.abu<-lmer(ln_cwm_abu~year+jul2
            +(1|stationID)
            +(1|yearID),
            data=pp.cwm)
test.vol<-lmer(log(cwm_vol)~year+jul2
               +(1|stationID)
               +(1|yearID),
               data=pp.cwm)
test.ln.vol<-lmer(ln_cwm_vol~year+jul2
                  +(1|stationID)
                  +(1|yearID),
                  data=pp.cwm)

tab_model(test.abu,test.ln.abu,test.vol,test.ln.vol)
summary(test3)
tab_model(test3,digits=3)

names(env.cwm)

env.cwm$jul2<-env.cwm$julian
env.cwm$jul2[env.cwm$jul2>183]<-365-env.cwm$julian[env.cwm$jul2>183]
env.cwm$month<-month(env.cwm$date)
summary(env.cwm)
pairs.panels(env.cwm[,16:23])
envmod2<-lmer(ln_cwm_abu~temperature+
                log(total.n)+
                log(suspended.particulates)+
                log(total.p)+
                log(silicon)+
                pH+
                salinity+
                (1|stationID)+(1|yearID/month),
              data=env.cwm)

summary(envmod2)
tab_model(test3,envmod2, digits =3)


obj<-plot_model(test3, type = "pred", pred.type="re",terms = c("year"),title="")
obj
obj2<-plot_model(envmod2, type = "pred", pred.type="re",terms = c("temperature"),title="")
obj2
obj3<-plot_model(envmod2, type = "pred", pred.type="re",terms = c("total.p"),title="")
obj3

mod.fig3<-obj+theme_bw()+
  ylab("CWM cell size [LN µm³]")+
  xlab("Year")+
  geom_jitter(data=pp.cwm,aes(x=year,y=ln_cwm_abu,alpha=jul2-.8),col="darkgreen")+
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
  geom_jitter(data=env.cwm,aes(x=temperature,y=ln_cwm_abu,alpha=jul2),col="blue")+
  theme(plot.margin = unit(c(0, 2, 0, 0), "cm"))+ #top, right, bottom, left
  theme(legend.position = "none")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

mod.fig4




names(mean.USI)
mean.USI$specname<-do.call(paste, c(mean.USI[c("genus", "species")], sep = "_"))



#Figures 

tiff(file = "~/P2021_5_pp_size/fig1.tiff", width = 2800, height = 3600, units = "px", res = 400)
cowplot::plot_grid(violin.raw,mod.fig2,
                   nrow=2,
                   align="v",
                   labels = c('A', 'B'))

dev.off()

tiff(file = "~/P2021_5_pp_size/fig2.tiff", width = 7200, height = 2400, units = "px", res = 400)
cowplot::plot_grid(spec.lmer.slope,selectspec,
                   ncol=2, nrow=1,
                   align="h",
                   rel_widths = c(4,5),
                   labels = c('A', 'B'))

dev.off()


tiff(file = "~/P2021_5_pp_size/fig3.tiff", width = 2000, height = 3200, units = "px", res = 400)
cowplot::plot_grid(mod.fig3,mod.fig4,
                   ncol=1, nrow=2,
                   align="v",
                   labels = c('A', 'B'))

dev.off()





