### cell size of coastal phytoplankton

# based on data files by Claus Dürselen
# model by Helmut Hillebrand started 20210104


#initiate WS, libraries
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

allppfinal <- read_delim("allppfinal.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)

names(allppfinal)
allppfinal$USI<-do.call(paste, c(allppfinal[c("stationID", "date")], sep = "_"))
allppfinal$USI<-as.factor(allppfinal$USI)

allppfinal$date<-dmy(allppfinal$date)
allppfinal$julian <- yday(allppfinal$date)
summary(allppfinal)

#subset 1: sum abundance, biovol, bioC
ppsum<-allppfinal[allppfinal$include=="sum",]
ppsum<-(ppsum[,c("USI","abundance","biovol","bioC")])
summary(ppsum)

#subset 2:
include<-allppfinal[allppfinal$include=="yes",]

names(include)

include<-include[include$N.vols!=0,]

mean.USI<-include[,c(3:21,112:113,144)]
summary(mean.USI)


stationinfo<- ddply(mean.USI, .(USI), summarise, 
                 abund.incl= sum(abundance, na.rm = T),
                 biovol.incl= sum(biovol, na.rm = T),
                 bioC.incl= sum(bioC, na.rm = T))

summary(stationinfo)
specinfo<-merge(stationinfo,mean.USI, by="USI")
names(specinfo)
summary(specinfo)

specinfo$relabund<-specinfo$abundance/specinfo$abund.incl
specinfo$relbiovol<-specinfo$biovol/specinfo$biovol.incl

specinfo<- ddply(specinfo, .(phylum, class, order, genus, species), summarise, 
            occupancy= length(USI)/1725,
            first= min(julian),
            last= max(julian),
            veg.period=last-first,
            mean.vol=mean(cell.vol,na.rm=T),
            sd.vol=sd(cell.vol,na.rm=T),
            mean.C=mean(cell.C,na.rm=T),
            sd.C=sd(cell.C,na.rm=T),
            mean.relabu=mean(relabund),
            sd.relabu=sd(relabund,na.rm=T),
            mean.relbiov=mean(relbiovol,na.rm=T),
            sd.relbiov=sd(relbiovol,na.rm=T),
            mean.dim.meas=mean(N.dim.meas,na.rm=T),
            sum.dim.meas=sum(N.dim.meas,na.rm=T),
            mean.cells.meas=mean(N.vols,na.rm=T),
            sum.cells.meas=sum(N.vols,na.rm=T))

summary(specinfo)
names(specinfo)
specinfo$LN.size<-log(specinfo$mean.vol)
specinfo$sqrt.relabu<-sqrt(specinfo$mean.relabu)
specinfo$sqrt.relvol<-sqrt(specinfo$mean.relbiov)
specinfo$sqrt.occ<-sqrt(specinfo$occupancy)

pairs.panels(specinfo[,c(9,20,23:25,22)])

specinfo$specname<-do.call(paste, c(specinfo[c("genus", "species")], sep = "_"))
specinfo$specname<-as.factor(specinfo$specname)
summary (specinfo)

include$specname<-do.call(paste, c(include[c("genus", "species")], sep = "_"))
include$specname<-as.factor(include$specname)
vol<-include[,c(3:6,9:13,145,114:143)]
summary(vol)
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

plot(LN.cell.vol~date,vol)
plot(LN.cell.vol~julian,vol)

#### OVERALL MODEL #### 
data <- vol

#get rid of two species not phytoplankton
unique(data$phylum) 
summary(data[is.na(data$phylum),])
data$phylum[is.na(data$phylum)]<-"Dinophyta"
unique(data$phylum) 
data<-data[data$genus!="Noctiluca",]
data<-data[data$genus!="Mesodinium",]
data<-data[data$genus!="Ebria",]

test1<-lmer(LN.cell.vol~year+julian
            +(1|phylum/class/order/genus/species)
            +(1|stationID),
            data=data)

summary(test1)

tab_model(test1)
plot_model(test1, type = "pred", terms = c("year"))

mod.fig<-plot_model(test1, type = "pred", terms = c("year"))
mod.fig1<-mod.fig+theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("year")+
  geom_jitter(data=data,aes(x=year,y=LN.cell.vol),size=.1,alpha=.01,col="blue")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

mod.fig1


test2<-lmer(LN.cell.vol~year+
            (1|phylum/class/order/genus/species),
            data=data)

summary(test2)

tab_model(test2)
plot_model(test2, type = "pred", terms = c("year"))

data$jul2<-data$julian
data$jul2[data$jul2>183]<-365-data$julian[data$jul2>183]


test1b<-lmer(LN.cell.vol~year+jul2
            +(1|phylum/class/order/genus/species)
            +(1|stationID),
            data=data)

summary(test1b)

data$jul2<-data$julian
data$jul2[data$jul2>183]<-365-data$julian[data$jul2>183]




tab_model(test1b,digits = 4)
plot_model(test1b, type = "pred", terms = c("year","jul2"))
plottest1b<-plot_model(test1b, type = "pred", terms = c("year"))
plottest1b
mod.fig2<-plottest1b+theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("year")+
  geom_jitter(data=data,aes(x=year,y=LN.cell.vol),size=.1,alpha=.01,col="blue")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

mod.fig2



#redo analyses with mean cell volumes per sample

summary(mean.USI)
data3<-(mean.USI[!is.na(mean.USI$cell.vol),])
data3$jul2<-data3$julian
data3$jul2[data3$jul2>183]<-365-data3$julian[data3$jul2>183]
data3$year<-year(data3$date)

test1c<-lmer(log(cell.vol)~year+jul2
             +(1|phylum/class/order/genus/species)
             +(1|stationID),
             data=data3)

summary(test1c)
tab_model(test1c,digits = 4)
AIC(test1,test2,test1b)
tab_model(test1b,test1c,digits = 4)


## species specific models over time
summary(data)
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
    slope.year<-rbind(slope.year,data.frame(temp[1,"specname"],
                                            icpt,slp,se.slp,p,
                                            N,median))
    rm(temp)
  }
  }
}

summary(slope.year)

hist(slope.year$slp)

slope.year<-plyr::rename(slope.year, c("temp.1...specname.."="specname"))
fulldat<-merge(specinfo,slope.year, by="specname")

names(fulldat)
fulldat$median.size<-log(fulldat$median)
fulldat$col<-NA
fulldat$col[fulldat$p>.05]<-.2
fulldat$col[fulldat$p<.05]<-.4
fulldat$col[fulldat$p<.01]<-.5
fulldat$col[fulldat$p<.001]<-.6
summary(fulldat)

names(fulldat)
pairs.panels(fulldat[,c(24:26,22,23,33,28)],method="spearman")

spec.slope<-ggplot(fulldat, 
                   aes(median.size,slp, 
                       col=class))+
  geom_hline(yintercept=0)+
  geom_point(alpha=fulldat$col,
             size=10*fulldat$sqrt.relabu+.5)+
  theme_bw()+
  ylab("Slope size~time")+
  xlab("Median cell size [log µm³]")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
  #+facet_wrap(~phylum, scales="free")

spec.slope



## Overall model with environment
allenv <- read_csv("allenv.csv")
names(allenv)
unique(data$stationID)
unique(allenv$StationID)
allenv$stationID<-allenv$StationID

year.data<- ddply(data3, .(phylum, class, order, genus, species,year,stationID), summarise, 
                    mean= mean(cell.vol, na.rm = T),
                    sd= sd(cell.vol, na.rm = T),
                    cv=sd/mean,
                    lnmean= mean(log(cell.vol), na.rm = T),
                    lnsd= sd(log(cell.vol), na.rm = T),
                    lnvc=lnsd/lnmean)

summary(year.data)

env<-merge(allenv,year.data, by=c("stationID","year"),all=F)

summary(env)
unique(env$stationID)

names(env)

pairs.panels(env[,c(4:12,23,25,26,28)],method="spearman")
env<-env[env$temperature>8,]
envmod1<-lmer(lnmean~log(total.p)+log(total.n)+
                log(silicon)+log(suspended.particulates)+
                pH+salinity+temperature
             +(1|phylum/class/order/genus/species),
             data=env)

summary(envmod1)
tab_model(envmod1)


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
  xlab("year")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

violin.raw




## dominant species plot
#add dominance values to data 
data2<-merge(data,fulldat[c("specname","sqrt.relabu","sqrt.relvol","sqrt.occ","icpt","slp","p","N")], by="specname", all=FALSE)
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
dominance<-dominance[dominance$N>3500,]
unique(dominance$specname)
domspec<-ggplot(dominance, 
                   aes(year,LN.cell.vol))+
  geom_point(alpha=.1,col="darkgrey")+
  geom_smooth(method="loess",col="blue")+
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
  facet_wrap(~specname)

domspec

steep<-data2[data2$p<0.0001,]
steep<-steep[steep$N>4000,]
unique(steep$specname)
summary(steep$specname)

steepspec<-ggplot(steep, 
                aes(year,LN.cell.vol))+
  geom_point(alpha=.1,col="darkgrey")+
  #geom_smooth(method="loess",col="blue")+
  geom_smooth(method="lm",col="blue")+
  theme(legend.position="none")+
  theme_bw()+
  ylab("Cell size [LN µm³]")+
  xlab("year")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  facet_wrap(~specname)

steepspec


## Community weighted means

weights<-merge(stationinfo,mean.USI, by="USI")
weights<-weights[!is.na(weights$biovol),]
weights$relabund<-weights$abundance/weights$abund.incl
weights$relbiovol<-weights$biovol/weights$biovol.incl
weights$year<-year(weights$date)
weights$lnsize<-log(weights$cell.vol)
weights$stationID<-as.factor(weights$stationID)
summary(weights)

pp.cwm <-
  weights %>%
  dplyr::group_by(stationID,date) %>%
  dplyr::summarize(size_cwm_abu = weighted.mean(cell.vol, abundance,na.rm=T), 
            size_cwm_vol = weighted.mean(cell.vol, biovol,na.rm=T),
            lnsize_cwm_abu = weighted.mean(lnsize, abundance,na.rm=T), 
            lnsize_cwm_vol = weighted.mean(lnsize, biovol,na.rm=T),
            size_cwsd_abu = wtd.var(cell.vol, abundance,na.rm=T), 
            size_cwsd_vol = wtd.var(cell.vol, biovol,na.rm=T),
            lnsize_cwsd_abu = wtd.var(lnsize, abundance,na.rm=T), 
            lnsize_cwsd_vol = wtd.var(lnsize, biovol,na.rm=T))


names(pp.cwm)
pp.cwm$year<-year(pp.cwm$date)
pp.cwm$julian<-yday(pp.cwm$date)


pairs.panels((pp.cwm[,c(3:12)]),method="spearman")

summary(pp.cwm)
names(pp.cwm)

size.cwm<-ggplot(pp.cwm, 
                    aes(year,log(size_cwsd_abu)))+
  geom_jitter(alpha=.1)+
  #geom_violin(alpha=.21)+
  geom_smooth()+
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
                 aes(as.factor(year),lnsize_cwm_abu))+
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

test3<-lmer(lnsize_cwm_abu~year+jul2
             +(1|stationID),
            data=pp.cwm)

summary(test3)
tab_model(test3,digits=4)

test4<-lmer(lnsize_cwsd_abu~year+jul2
            +(1|stationID),
            data=pp.cwm)

summary(test4)
tab_model(test4,digits=4)


pp.cwm$cv<-pp.cwm$size_cwsd_abu/pp.cwm$size_cwm_abu
pp.cwm$lncv<-pp.cwm$lnsize_cwsd_abu/pp.cwm$lnsize_cwm_abu


test6a<-lmer(lncv~year+jul2
            +(1|stationID),
            data=pp.cwm)

test6b<-lmer(log(cv+1)~year+jul2
            +(1|stationID),
            data=pp.cwm)
test6c<-lmer(log(size_cwm_abu+1)~year+jul2
             +(1|stationID),
             data=pp.cwm)
test6d<-lmer(log(size_cwsd_abu+1)~year+jul2
            +(1|stationID),
            data=pp.cwm)

tab_model(test6a,test6b,test6c,test6d,digits=4)

tab_model(test1b,test3,test4,test6a,digits=4)
tab1<-tab_model(test1b,digits = 4)

obj<-plot_model(test3, type = "pred", pred.type="re",terms = c("year"))
obj
obj2<-plot_model(test4, type = "pred", pred.type="re",terms = c("year"))
obj2
obj3<-plot_model(test6a, type = "pred", pred.type="re",terms = c("year"))
obj3
mod.fig3<-obj+theme_bw()+
  ylab("CWD log cell size [LN µm³]")+
  xlab("year")+
  geom_jitter(data=pp.cwm,aes(x=year,y=lnsize_cwm_abu),alpha=.1,col="blue")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

mod.fig3


mod.fig4<-obj2+theme_bw()+
  ylab("weighted variance log cell size [LN µm³]")+
  xlab("year")+
  geom_jitter(data=pp.cwm,aes(x=year,y=lnsize_cwsd_abu),alpha=.1,col="darkgreen")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

mod.fig4

mod.fig5<-obj3+theme_bw()+
  ylab("weighted CV log cell size")+
  xlab("year")+
  geom_jitter(data=pp.cwm,aes(x=year,y=lnsize_cwsd_abu),alpha=.1,col="darkgreen")+
  theme(axis.title.y=element_text(size=16, face="plain", colour="black",vjust=0.3),axis.text.y=element_text(size=12,face="bold",colour="black"))+
  theme(axis.title.x=element_text(size=16,face="plain",colour="black"),axis.text.x=element_text(size=12,face="bold",colour="black"))+
  theme(axis.ticks=element_line(colour="black",size=1),axis.ticks.length=unit(0.3,"cm"))+
  theme(panel.border=element_rect(colour="black",size=1.3))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

mod.fig5

