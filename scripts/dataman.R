### cell size of coastal phytoplankton


rm(list=ls())
graphics.off()

# based on data files by Claus Dürselen
# script by Helmut Hillebrand started 20210312
library(plyr)
library(tidyverse)
library(lubridate)
library(lattice)

#the data used is the synopsis of all single files with pp data
#only pre-upload change was that I aligned all stations with 
#their most recent names according to information from NLWKN
#Further I deleted "(Group)" from genus as it is inconsistently used

#& I replaced n.g. for not measured dimensions by NA

data<- read.csv("summary.csv", sep=";",dec=".")
names(data)

#get rid of some variables not consistently reported
data$time<-NULL
data$tide<-NULL
data$program<-NULL


#create a variable to identify rows to be excluded
data$exclude<-NA

#exclude the rows with the sums (as we will calculate these ourselves)
data$exclude[data$phylum=="Summe"]<-"sum"
data$exclude[data$class=="Summe"]<-"sum"

#exclude a few heterotrophic species
data$exclude[data$genus=="Noctiluca"]<-"het"
data$exclude[data$genus=="Pronoctiluca"]<-"het"
data$exclude[data$genus=="Ebria"]<-"het"
data$exclude[data$genus=="Telonema"]<-"het"
data$exclude[data$genus=="Dinobryon"]<-"het"
data$exclude[data$genus=="Polykrikos"]<-"het"
data$exclude[data$species=="indet. (Parasit)"]<-"het"
data$exclude[data$species=="indet. (parasitisch)"]<-"het"


#exclude ciliate Mesodinium rubrum even though it is photosynthetic
#by using its food's plastids
data$exclude[data$genus=="Mesodinium"]<-"het"

#exclude rows with volume meaurements
data$exclude[data$DimA=="n.g."]<-"not measured"

# reduce data set to all those with no reason to exclude
data<-data[is.na(data$exclude),]


#arrange stations
unique(data$stationID)

#create a date, year and julian day as well as a unique sample identifier
data$date<-dmy(data$date)
data$year<-year(data$date)
data$julian<-yday(data$date)
data$USI<-do.call(paste, c(data[c("stationID", "date")], sep = "_"))

#create a few diagnostics tools to see whether calculations
#shifted with years

data$DimA.needed<-(ifelse(!is.na(data$DimA), 1, 0))
data$DimB.needed<-(ifelse(!is.na(data$DimB), 1, 0))
data$DimC.needed<-(ifelse(!is.na(data$DimC), 1, 0))
data$DimD.needed<-(ifelse(!is.na(data$DimD), 1, 0))
data$DimE.needed<-(ifelse(!is.na(data$DimE), 1, 0))

data$corr.fac.needed<-(ifelse(!is.na(data$corr.fac), 1, 0))

#count the number of unique measurements per cell
data$A.count <- rowSums( !is.na(data[,29:58]))
data$B.count <- rowSums( !is.na(data[,59:88]))
data$C.count <- rowSums( !is.na(data[,89:118]))


#align taxonomy

#correct misspellings
levels(data$genus)
levels(data$order)
#in order to allow for new names the variables need to be characters
data$genus<-as.character((data$genus))
data$species<-as.character((data$species))
data$order<-as.character((data$order))
data$class<-as.character((data$class))
data$phylum<-as.character((data$phylum))

data$genus[data$genus=="Prrocentrum"]<-"Prorocentrum"

#check for changes over time

summary(as.factor(data$Name.AlgaeBase.20200609))

#Bacillaria
unique(data$genus[data$Name.AlgaeBase.20200609=="Bacillaria paxillifera"])
unique(data$species[data$Name.AlgaeBase.20200609=="Bacillaria paxillifera"])
unique(data$species[data$genus=="Bacillaria"])
# according to algaebase all of these are synonyms, only species in genus
data$species[data$genus=="Bacillaria"]<-"paxillifera"
data$phylum[data$genus=="Bacillaria"]<-"Bacillariophyta"
data$class[data$genus=="Bacillaria"]<-"Bacillariophyceae"
data$order[data$genus=="Bacillaria"]<-"Bacillariales"

#Binuclearia lauterbornii
unique(data$genus[data$Name.AlgaeBase.20200609=="Binuclearia lauterbornii"])
unique(data$species[data$Name.AlgaeBase.20200609=="Binuclearia lauterbornii"])
unique(data$species[data$genus=="Planctonema"])
data$genus[data$genus=="Planctonema"]<-"Binuclearia"
data$phylum[data$genus=="Binuclearia"]<-"Chlorophyta"
data$class[data$genus=="Binuclearia"]<-"Ulvophyceae"
data$order[data$genus=="Binuclearia"]<-"Ulotrichales"

#Lebouridinium glaucum 

unique(data$genus[data$Name.AlgaeBase.20200609=="Lebouridinium glaucum"])
unique(data$species[data$Name.AlgaeBase.20200609=="Lebouridinium glaucum"])
unique(data$species[data$genus=="Katodinium"])
data$genus[data$genus=="Katodinium" & data$species=="glaucum"]<-"Lebouridinium"
data$phylum[data$genus=="Katodinium"]<-"Dinophyta"
data$class[data$genus=="Katodinium"]<-"Dinophyceae"
data$order[data$genus=="Katodinium"]<-"Gymnodiniales"
data$phylum[data$genus=="Lebouridinium"]<-"Dinophyta"
data$class[data$genus=="Lebouridinium"]<-"Dinophyceae"
data$order[data$genus=="Lebouridinium"]<-"Gymnodiniales"

#Pseudopediastrum boryanum 

unique(data$genus[data$Name.AlgaeBase.20200609=="Pseudopediastrum boryanum"])
unique(data$species[data$Name.AlgaeBase.20200609=="Pseudopediastrum boryanum"])
unique(data$species[data$genus=="Pediastrum"])
data$genus[data$genus=="Pediastrum" & data$species=="boryanum"]<-"Pseudopediastrum"
data$phylum[data$genus=="Pediastrum"]<-"Chlorophyta"
data$class[data$genus=="Pediastrum"]<-"Chlorophyceae"
data$order[data$genus=="Pediastrum"]<-"Sphaeropleales"
data$phylum[data$genus=="Pseudopediastrum"]<-"Chlorophyta"
data$class[data$genus=="Pseudopediastrum"]<-"Chlorophyceae"
data$order[data$genus=="Pseudopediastrum"]<-"Sphaeropleales"

# Thalassiosira gravida 
unique(data$genus[data$Name.AlgaeBase.20200609=="Thalassiosira gravida"])
unique(data$species[data$Name.AlgaeBase.20200609=="Thalassiosira gravida"])
unique(data$species[data$genus=="Thalassiosira"])
data$species[data$genus=="Thalassiosira" & data$species=="rotula"]<-"gravida"
data$phylum[data$genus=="Thalassiosira"]<-"Bacillariophyta"
data$class[data$genus=="Thalassiosira"]<-"Mediophyceae"
data$order[data$genus=="Thalassiosira"]<-"Thalassiosirales"


# Trieres chinensis, mobiliensis, regia
unique(data$genus[data$Name.AlgaeBase.20200609=="Trieres chinensis"])
unique(data$species[data$Name.AlgaeBase.20200609=="Trieres chinensis"])
unique(data$species[data$genus=="Odontella"])
data$genus[data$genus=="Odontella" & data$species=="sinensis"]<-"Trieres"
data$species[data$genus=="Odontella" & data$species=="sinensis"]<-"chinensis"
unique(data$genus[data$Name.AlgaeBase.20200609=="Trieres mobiliensis"])
unique(data$species[data$Name.AlgaeBase.20200609=="Trieres mobiliensis"])
data$genus[data$genus=="Odontella" & data$species=="mobiliensis"]<-"Trieres"
data$species[data$genus=="Odontella" & data$species=="mobiliensis"]<-"mobiliensis"
unique(data$genus[data$Name.AlgaeBase.20200609=="Trieres regia"])
unique(data$species[data$Name.AlgaeBase.20200609=="Trieres regia"])
data$genus[data$genus=="Odontella" & data$species=="regia"]<-"Trieres"
data$species[data$genus=="Odontella" & data$species=="regia"]<-"regia"
data$phylum[data$genus=="Trieres"]<-"Bacillariophyta"
data$class[data$genus=="Trieres"]<-"Mediophyceae"
data$order[data$genus=="Trieres"]<-"Eupodiscales"
data$phylum[data$genus=="Odontella"]<-"Bacillariophyta"
data$class[data$genus=="Odontella"]<-"Mediophyceae"
data$order[data$genus=="Odontella"]<-"Eupodiscales"

# Tripos furca, fusus, horridus, lineatus
unique(data$genus[data$Name.AlgaeBase.20200609=="Tripos furca"])
unique(data$species[data$Name.AlgaeBase.20200609=="Tripos furca"])
unique(data$species[data$genus=="Ceratium"])
unique(data$species[data$genus=="Tripos"])
data$genus[data$genus=="Ceratium" & data$species=="furca"]<-"Tripos"
unique(data$genus[data$Name.AlgaeBase.20200609=="Tripos fusus"])
unique(data$species[data$Name.AlgaeBase.20200609=="Tripos fusus"])
data$genus[data$genus=="Ceratium" & data$species=="fusus"]<-"Tripos"
unique(data$genus[data$Name.AlgaeBase.20200609=="Tripos horridus"])
unique(data$species[data$Name.AlgaeBase.20200609=="Tripos horridus"])
data$genus[data$genus=="Ceratium" & data$species=="horridum"]<-"Tripos"
data$species[data$genus=="Tripos" & data$species=="horridum"]<-"horridus"
unique(data$genus[data$Name.AlgaeBase.20200609=="Tripos lineatus"])
unique(data$species[data$Name.AlgaeBase.20200609=="Tripos lineatus"])
data$genus[data$genus=="Ceratium" & data$species=="lineatum"]<-"Tripos"
data$species[data$genus=="Tripos" & data$species=="lineatum"]<-"lineatus"
data$genus[data$genus=="Ceratium" & data$species=="tripos"]<-"Tripos"
data$species[data$genus=="Tripos" & data$species=="tripos"]<-"muelleri"
data$species[data$genus=="Odontella" & data$species=="regia"]<-"regia"
data$phylum[data$genus=="Tripos"]<-"Dinophyta"
data$class[data$genus=="Tripos"]<-"Dinophyceae"
data$order[data$genus=="Tripos"]<-"Gonyaulacales"



#create a species name column

data$specname<-do.call(paste, c(data[c("genus", "species")], sep = "_"))
data$specname<-as.factor(data$specname)
summary(data$specname)

levels(data$specname)

#checking for potential problems

#according to algaebase Chaetoceros radians is a synonym of C. socialis
data$species[data$specname=="Chaetoceros_radians"]<-"socialis"
data$species[data$specname=="Chaetoceros_radians / socialis"]<-"socialis"

#misspelling
data$genus[data$specname=="Meuniera _membranacea"]<-"Meuniera"
data$genus[data$specname=="Peridiniella _danica"]<-"Peridiniella"
data$species[data$specname=="Plagiogrammopsis_vanheurcki"]<-"vanheurckii"
data$species[data$specname=="Podosira_stelliger"]<-"stelligera"
data$species[data$specname=="Pseudo-nitzschia_delecatissima"]<-"delicatissima"
data$species[data$specname=="Thalassiosira_minimum"]<-"minima"
data$species[data$specname=="Guinardia_sriata"]<-"striata"

#synonyms
data$species[data$specname=="Rhizosolenia_pungens"]<-"setigera f. pungens"
data$species[data$specname=="Cerataulus_turgidus"]<-"turgida"
data$genus[data$specname=="Cerataulus_turgidus"]<-"Odontella"
data$genus[data$specname=="Rhizosolenia_robusta"]<-"Neocalyptrella"
data$genus[data$specname=="Dinophysis_rotundata"]<-"Phalacroma"
data$species[data$specname=="Dinophysis_rotundata"]<-"rotundatum"
data$species[data$specname=="Scrippsiella_"]<-"sp."

#as cf was inconsistently used, I decided to fold these into the nominal species
data$species[data$specname=="Nitzschia_cf. reversa"]<-"reversa"
data$species[data$specname=="Pseudo-nitzschia_cf. pungens"]<-"pungens"
data$species[data$specname=="Pseudo-nitzschia_delicatissima (Complex)"]<-"delicatissima"
data$species[data$specname=="Pseudo-nitzschia_delicatissima (Komplex)"]<-"delicatissima"
data$species[data$specname=="Pseudo-nitzschia_pungens (Komplex)"]<-"pungens"
data$species[data$specname=="Pseudo-nitzschia_seriata (Complex)"]<-"seriata"
data$species[data$specname=="Pseudo-nitzschia_seriata (Komplex)"]<-"seriata"
data$genus[data$specname=="Scenedesmus_Desmodesmus_sp."]<-"Scenedesmus"
data$species[data$specname=="Skeletonema_costatum (Komplex)"]<-"costatum"
data$species[data$specname=="Odontella_rhombus var. trigona"]<-"rhombus f. trigona"


#inconsistent use of sp.

#Eutreptiella sp has only one record
data$species[data$specname=="Eutreptiella_sp."]<-"marina"

#Phaeocystis globosa is only used once instead of sp.
data$species[data$specname=="Phaeocystis_globosa"]<-"sp."

#create the species name column anew (as corrections were made)

data$specname<-do.call(paste, c(data[c("genus", "species")], sep = "_"))
data$specname<-as.factor(data$specname)
summary(data$specname)
levels(data$specname)

#getting the phylogeny right

levels(as.factor(data$genus))

data$phylum[data$genus=="Achnanthes"]<-"Bacillariophyta"
data$class[data$genus=="Achnanthes"]<-"Bacillariophyceae"
data$order[data$genus=="Achnanthes"]<-"Mastogloiales"

data$phylum[data$genus=="Dactyliosolen"|
              data$genus=="Rhizosolenia"|
              data$genus=="Guinardia"|
              data$genus=="Neocalyptrella"|
              data$genus=="Proboscia"]<-"Bacillariophyta"
data$class[data$genus=="Dactyliosolen"|
             data$genus=="Rhizosolenia"|
             data$genus=="Guinardia"|
             data$genus=="Neocalyptrella"|
             data$genus=="Proboscia"]<-"Coscinodiscophyceae"
data$order[data$genus=="Dactyliosolen"|
             data$genus=="Rhizosolenia"|
             data$genus=="Guinardia"|
             data$genus=="Neocalyptrella"|
             data$genus=="Proboscia"]<-"Rhizosoleniales"

data$phylum[data$genus=="Actinocyclus"|
              data$genus=="Actinoptychus"|
              data$genus=="Aulacodiscus"|
              data$genus=="Coscinodiscus"]<-"Bacillariophyta"
data$class[data$genus=="Actinocyclus"|
             data$genus=="Actinoptychus"|
             data$genus=="Aulacodiscus"|
             data$genus=="Coscinodiscus"]<-"Coscinodiscophyceae"
data$order[data$genus=="Actinocyclus"|
             data$genus=="Actinoptychus"|
             data$genus=="Aulacodiscus"|
             data$genus=="Coscinodiscus"]<-"Coscinodiscales"

data$phylum[data$genus=="Corethron"]<-"Bacillariophyta"
data$class[data$genus=="Corethron"]<-"Coscinodiscophyceae"
data$order[data$genus=="Corethron"]<-"Corethrales"

data$phylum[data$genus=="Melosira"|
              data$genus=="Podosira"]<-"Bacillariophyta"
data$class[data$genus=="Melosira"|
             data$genus=="Podosira"]<-"Coscinodiscophyceae"
data$order[data$genus=="Melosira"|
             data$genus=="Podosira"]<-"Melosirales"

data$phylum[data$genus=="Cryptomonas"|
              data$genus=="Hemiselmis"]<-"Cryptophyta"
data$class[data$genus=="Cryptomonas"|
             data$genus=="Hemiselmis"]<-"Cryptophyceae"
data$order[data$genus=="Cryptomonas"|
             data$genus=="Hemiselmis"]<-"Cryptomonadales"

data$phylum[data$genus=="Plagioselmis"|
              data$genus=="Teleaulax"]<-"Cryptophyta"
data$class[data$genus=="Plagioselmis"|
             data$genus=="Teleaulax"]<-"Cryptophyceae"
data$order[data$genus=="Plagioselmis"|
             data$genus=="Teleaulax"]<-"Pyrenomonadales"

data$phylum[data$genus=="Akashiwo"|
              data$genus=="Alexandrium"|
              data$genus=="Pyrocystis"|
              data$genus=="Pyrophacus"]<-"Dinophyta"
data$class[data$genus=="Akashiwo"|
             data$genus=="Alexandrium"|
             data$genus=="Pyrocystis"|
             data$genus=="Pyrophacus"]<-"Dinophyceae"
data$order[data$genus=="Akashiwo"|
             data$genus=="Alexandrium"|
             data$genus=="Pyrocystis"|
             data$genus=="Pyrophacus"]<-"Gonyaulacales"

data$phylum[data$genus=="Dinophysis"|
              data$genus=="Phalacroma"]<-"Dinophyta"
data$class[data$genus=="Dinophysis"|
             data$genus=="Phalacroma"]<-"Dinophyceae"
data$order[data$genus=="Dinophysis"|
             data$genus=="Phalacroma"]<-"Dinophysales"

data$phylum[data$genus=="Diplopsalis"|
              data$genus=="Heterocapsa"|
              data$genus=="Peridiniella"|
              data$genus=="Protoperidinium"]<-"Dinophyta"
data$class[data$genus=="Diplopsalis"|
             data$genus=="Heterocapsa"|
             data$genus=="Peridiniella"|
             data$genus=="Protoperidinium"]<-"Dinophyceae"
data$order[data$genus=="Diplopsalis"|
             data$genus=="Heterocapsa"|
             data$genus=="Peridiniella"|
             data$genus=="Protoperidinium"]<-"Peridiniales"

data$phylum[data$genus=="Mesoporos"|
              data$genus=="Prorocentrum"]<-"Dinophyta"
data$class[data$genus=="Mesoporos"|
             data$genus=="Prorocentrum"]<-"Dinophyceae"
data$order[data$genus=="Mesoporos"|
             data$genus=="Prorocentrum"]<-"Prorocentrales"

data$phylum[data$genus=="Dissodinium"|
              data$genus=="Gymnodinium"|
              data$genus=="Gyrodinium"|
              data$genus=="Karenia"|
              data$genus=="Polykrikos"]<-"Dinophyta"
data$class[data$genus=="Dissodinium"|
             data$genus=="Gymnodinium"|
             data$genus=="Gyrodinium"|
             data$genus=="Karenia"|
             data$genus=="Polykrikos"]<-"Dinophyceae"
data$order[data$genus=="Dissodinium"|
             data$genus=="Gymnodinium"|
             data$genus=="Gyrodinium"|
             data$genus=="Karenia"|
             data$genus=="Polykrikos"]<-"Gymnodiniales"

data$phylum[data$genus=="Amphidinium"]<-"Dinophyta"
data$class[data$genus=="Amphidinium"]<-"Dinophyceae"
data$order[data$genus=="Amphidinium"]<-"Amphidiniales"

data$phylum[data$genus=="Ankistrodesmus"|
              data$genus=="Coelastrum"|
              data$genus=="Microspora"|
              data$genus=="Monoraphidium"|
              data$genus=="Scenedesmus"|
              data$genus=="Tetraedron"|
              data$genus=="Tetrastrum"]<-"Chlorophyta"
data$class[data$genus=="Ankistrodesmus"|
             data$genus=="Coelastrum"|
             data$genus=="Microspora"|
             data$genus=="Monoraphidium"|
             data$genus=="Scenedesmus"|
             data$genus=="Tetraedron"|
             data$genus=="Tetrastrum"]<-"Chlorophyceae"
data$order[data$genus=="Ankistrodesmus"|
             data$genus=="Coelastrum"|
             data$genus=="Microspora"|
             data$genus=="Monoraphidium"|
             data$genus=="Scenedesmus"|
             data$genus=="Tetraedron"|
             data$genus=="Tetrastrum"]<-"Sphaeropleales"

data$phylum[data$genus=="Cymbella"]<-"Bacillariophyta"
data$class[data$genus=="Cymbella"]<-"Bacillariophyceae"
data$order[data$genus=="Cymbella"]<-"Cymbellales"

data$phylum[data$genus=="Subsilicea"|
              data$genus=="Biddulphia"|
              data$genus=="Bellerochea"]<-"Bacillariophyta"
data$class[data$genus=="Subsilicea"|
             data$genus=="Biddulphia"|
             data$genus=="Bellerochea"]<-"Mediophyceae"
data$order[data$genus=="Subsilicea"|
             data$genus=="Biddulphia"|
             data$genus=="Bellerochea"]<-"Biddulphiales"

data$phylum[data$genus=="Rhaphoneis"]<-"Bacillariophyta"
data$class[data$genus=="Rhaphoneis"]<-"Bacillariophyceae"
data$order[data$genus=="Rhaphoneis"]<-"Rhaphoneidales"

data$phylum[data$genus=="Cylindrotheca"|
              data$genus=="Nitzschia"|
              data$genus=="Pseudo-nitzschia"]<-"Bacillariophyta"
data$class[data$genus=="Cylindrotheca"|
             data$genus=="Nitzschia"|
             data$genus=="Pseudo-nitzschia"]<-"Bacillariophyceae"
data$order[data$genus=="Cylindrotheca"|
             data$genus=="Nitzschia"|
             data$genus=="Pseudo-nitzschia"]<-"Bacillariales"

data$phylum[data$genus=="Asterionellopsis"|
              data$genus=="Delphineis"]<-"Bacillariophyta"
data$class[data$genus=="Asterionellopsis"|
             data$genus=="Delphineis"]<-"Bacillariophyceae"
data$order[data$genus=="Asterionellopsis"|
             data$genus=="Delphineis"]<-"Rhaphoneidales"

data$phylum[data$genus=="Asteroplanus"|
              data$genus=="Fragilaria"|
              data$genus=="Synedra"]<-"Bacillariophyta"
data$class[data$genus=="Asteroplanus"|
             data$genus=="Fragilaria"|
             data$genus=="Synedra"]<-"Bacillariophyceae"
data$order[data$genus=="Asteroplanus"|
             data$genus=="Synedra"|
             data$genus=="Fragilaria"]<-"Fragilariales"

data$phylum[data$genus=="Diploneis"|
              data$genus=="Gyrosigma"|
              data$genus=="Pleurosigma"|
              data$genus=="Meuniera"|
              data$genus=="Navicula"|
              data$genus=="Plagiolemma"]<-"Bacillariophyta"
data$class[data$genus=="Diploneis"|
             data$genus=="Gyrosigma"|
             data$genus=="Pleurosigma"|
             data$genus=="Meuniera"|
             data$genus=="Navicula"|
             data$genus=="Plagiolemma"]<-"Bacillariophyceae"
data$order[data$genus=="Diploneis"|
             data$genus=="Gyrosigma"|
             data$genus=="Pleurosigma"|
             data$genus=="Meuniera"|
             data$genus=="Navicula"|
             data$genus=="Plagiolemma"]<-"Naviculales"

data$phylum[data$genus=="Diatoma"|
              data$genus=="Asterionella"]<-"Bacillariophyta"
data$class[data$genus=="Diatoma"|
             data$genus=="Asterionella"]<-"Bacillariophyceae"
data$order[data$genus=="Diatoma"|
             data$genus=="Asterionella"]<-"Tabellariales"

data$phylum[data$genus=="Entomoneis"|
              data$genus=="Surirella"]<-"Bacillariophyta"
data$class[data$genus=="Entomoneis"|
             data$genus=="Surirella"]<-"Bacillariophyceae"
data$order[data$genus=="Entomoneis"|
             data$genus=="Surirella"]<-"Surirellales"

data$phylum[data$genus=="Licmophora"]<-"Bacillariophyta"
data$class[data$genus=="Licmophora"]<-"Bacillariophyceae"
data$order[data$genus=="Licmophora"]<-"Licmophorales"

data$phylum[data$genus=="Thalassionema"]<-"Bacillariophyta"
data$class[data$genus=="Thalassionema"]<-"Bacillariophyceae"
data$order[data$genus=="Thalassionema"]<-"Thalassionematales"

data$phylum[data$genus=="Dinobryon"|
              data$genus=="Ochromonas"]<-"Ochrophyta"
data$class[data$genus=="Dinobryon"|
             data$genus=="Ochromonas"]<-"Chrysophyceae"
data$order[data$genus=="Dinobryon"|
             data$genus=="Ochromonas"]<-"Chromulinales"

data$phylum[data$genus=="Dictyocha"|
              data$genus=="Pseudopedinella"]<-"Ochrophyta"
data$class[data$genus=="Dictyocha"|
             data$genus=="Pseudopedinella"]<-"Dictyophyceae"
data$order[data$genus=="Dictyocha"]<-"Dictyochales"
data$order[data$genus=="Pseudopedinella"]<-"Pedinellales"


data$phylum[data$genus=="Synura"]<-"Ochrophyta"
data$class[data$genus=="Synura"]<-"Synurophyceae"
data$order[data$genus=="Synura"]<-"Synurales"

data$phylum[data$genus=="Brockmanniella"|
              data$genus=="Lennoxia"|
              data$genus=="Plagiogrammopsis"]<-"Bacillariophyta"
data$class[data$genus=="Brockmanniella"|
             data$genus=="Lennoxia"|
             data$genus=="Plagiogrammopsis"]<-"Mediophyceae"
data$order[data$genus=="Brockmanniella"|
             data$genus=="Lennoxia"|
             data$genus=="Plagiogrammopsis"]<-"Cymatosirales"

data$phylum[data$genus=="Ditylum"|
              data$genus=="Lithodesmium"]<-"Bacillariophyta"
data$class[data$genus=="Ditylum"|
             data$genus=="Lithodesmium"]<-"Mediophyceae"
data$order[data$genus=="Ditylum"|
             data$genus=="Lithodesmium"]<-"Lithodesmiales"

data$phylum[data$genus=="Eunotogramma"]<-"Bacillariophyta"
data$class[data$genus=="Eunotogramma"]<-"Mediophyceae"
data$order[data$genus=="Eunotogramma"]<-"Anaulales"

data$phylum[data$genus=="Cerataulina"|
              data$genus=="Eucampia"]<-"Bacillariophyta"
data$class[data$genus=="Cerataulina"|
             data$genus=="Eucampia"]<-"Mediophyceae"
data$order[data$genus=="Cerataulina"|
             data$genus=="Eucampia"]<-"Hemiaulales"

data$phylum[data$genus=="Cyclotella"|
              data$genus=="Stephanodiscus"]<-"Bacillariophyta"
data$class[data$genus=="Cyclotella"|
             data$genus=="Stephanodiscus"]<-"Mediophyceae"
data$order[data$genus=="Cyclotella"|
             data$genus=="Stephanodiscus"]<-"Stephanodiscales"

data$phylum[data$genus=="Odontella"]<-"Bacillariophyta"
data$class[data$genus=="Odontella"]<-"Mediophyceae"
data$order[data$genus=="Odontella"]<-"Eupodiscales"

data$phylum[data$genus=="Stephanopyxis"]<-"Bacillariophyta"
data$class[data$genus=="Stephanopyxis"]<-"Coscinodiscophyceae"
data$order[data$genus=="Stephanopyxis"]<-"Stephanopyxales"

data$phylum[data$genus=="Paralia"]<-"Bacillariophyta"
data$class[data$genus=="Paralia"]<-"Coscinodiscophyceae"
data$order[data$genus=="Paralia"]<-"Paraliales"

data$phylum[data$genus=="Oocystis"]<-"Chlorophyta"
data$class[data$genus=="Oocystis"]<-"Trebouxiophyceae"
data$order[data$genus=="Oocystis"]<-"Chlorellales"

data$phylum[data$genus=="Pachysphaera"|
              data$genus=="Pyramimonas"|
              data$genus=="Pterosperma"]<-"Chlorophyta"
data$class[data$genus=="Pachysphaera"|
             data$genus=="Pyramimonas"|
             data$genus=="Pterosperma"]<-"Pyramimonadophyceae"
data$order[data$genus=="Pachysphaera"|
             data$genus=="Pyramimonas"|
             data$genus=="Pterosperma"]<-"Pyramimonadales"

data$phylum[data$genus=="Chroococcus"|
              data$genus=="Gloeocapsa"]<-"Cyanobacteria"
data$class[data$genus=="Chroococcus"|
             data$genus=="Gloeocapsa"]<-"Cyanophyceae"
data$order[data$genus=="Chroococcus"|
             data$genus=="Gloeocapsa"]<-"Chroococcales"

data$phylum[data$genus=="Limnothrix"|
              data$genus=="Merismopedia"|
              data$genus=="Planktolyngbya"]<-"Cyanobacteria"
data$class[data$genus=="Limnothrix"|
             data$genus=="Merismopedia"|
             data$genus=="Planktolyngbya"]<-"Cyanophyceae"
data$order[data$genus=="Limnothrix"|
             data$genus=="Merismopedia"|
             data$genus=="Planktolyngbya"]<-"Synechococcales"

data$phylum[data$genus=="Oscillatoria"|
              data$genus=="Planktothrix"]<-"Cyanobacteria"
data$class[data$genus=="Oscillatoria"|
             data$genus=="Planktothrix"]<-"Cyanophyceae"
data$order[data$genus=="Oscillatoria"|
             data$genus=="Planktothrix"]<-"Oscillatoriales"

data$phylum[data$genus=="Chrysochromulina"|
              data$genus=="Corymbellus"]<-"Haptophyta"
data$class[data$genus=="Chrysochromulina"|
             data$genus=="Corymbellus"]<-"Coccolithophyceae"
data$order[data$genus=="Chrysochromulina"|
             data$genus=="Corymbellus"]<-"Prymnesiales"

data$phylum[data$genus=="Phaeocystis"]<-"Haptophyta"
data$class[data$genus=="Phaeocystis"]<-"Coccolithophyceae"
data$order[data$genus=="Phaeocystis"]<-"Phaeocystales"

data$phylum[data$genus=="Triceratium"]<-"Bacillariophyta"
data$class[data$genus=="Triceratium"]<-"Coscinodiscophyceae"
data$order[data$genus=="Triceratium"]<-"Triceratiales"

data$phylum[data$genus=="Euglena"|
              data$genus=="Trachelomonas"]<-"Euglenozoa"
data$class[data$genus=="Euglena"|
             data$genus=="Trachelomonas"]<-"Euglenophyceae"
data$order[data$genus=="Euglena"|
             data$genus=="Trachelomonas"]<-"Euglenales"

data$phylum[data$genus=="Eutreptiella"]<-"Euglenozoa"
data$class[data$genus=="Eutreptiella"]<-"Euglenophyceae"
data$order[data$genus=="Eutreptiella"]<-"Eutreptiiida"

data$phylum[data$genus=="Bacteriastrum"|
              data$genus=="Chaetoceros"|
              data$genus=="Leptocylindrus"]<-"Bacillariophyta"
data$class[data$genus=="Bacteriastrum"|
             data$genus=="Chaetoceros"|
             data$genus=="Leptocylindrus"]<-"Mediophyceae"
data$order[data$genus=="Bacteriastrum"|
             data$genus=="Chaetoceros"|
             data$genus=="Leptocylindrus"]<-"Chaetocerotales"


data$phylum[data$genus=="Detonula"|
              data$genus=="Thalassiosira"|
              data$genus=="Lauderia"|
              data$genus=="Porosira"|
              data$genus=="Skeletonema"]<-"Bacillariophyta"
data$class[data$genus=="Detonula"|
             data$genus=="Thalassiosira"|
             data$genus=="Lauderia"|
             data$genus=="Porosira"|
             data$genus=="Skeletonema"]<-"Mediophyceae"
data$order[data$genus=="Detonula"|
             data$genus=="Thalassiosira"|
             data$genus=="Lauderia"|
             data$genus=="Porosira"|
             data$genus=="Skeletonema"]<-"Thalassiosirales"

data$phylum[data$genus=="Helicotheca"]<-"Bacillariophyta"
data$class[data$genus=="Helicotheca"]<-"Mediophyceae"
data$order[data$genus=="Helicotheca"]<-"Briggerales"

data$phylum[data$genus=="Leucocryptos"]<-"Katablepharidophyta"
data$class[data$genus=="Leucocryptos"]<-"Katablepharidophyceae"
data$order[data$genus=="Leucocryptos"]<-"Katablephariales"

data$phylum[data$genus=="Staurastrum"]<-"Charophyta"
data$class[data$genus=="Staurastrum"]<-"Zygnematophyceae"
data$order[data$genus=="Staurastrum"]<-"Desmidiales"

data$phylum[data$genus=="Pronoctiluca"]<-"Dinophyta"
data$class[data$genus=="Pronoctiluca"]<-"Dinophyceae"
data$order[data$genus=="Pronoctiluca"]<-"Noctilucales"

data$phylum[data$genus=="Torodinium"]<-"Dinophyta"
data$class[data$genus=="Torodinium"]<-"Dinophyceae"
data$order[data$genus=="Torodinium"]<-"Torodiniales"

data$phylum[data$genus=="Scrippsiella"]<-"Dinophyta"
data$class[data$genus=="Scrippsiella"]<-"Dinophyceae"
data$order[data$genus=="Scrippsiella"]<-"Thoracosphaerales"

#species where I differ from AlgaeBASE
#Mediopyxis has been associated to Mediophyceae by Kühn et al.
data$phylum[data$genus=="Mediopyxis"]<-"Bacillariophyta"
data$class[data$genus=="Mediopyxis"]<-"Mediophyceae"
data$order[data$genus=="Mediopyxis"]<-"incerte"


#filling up the information of those which were not identified to 
#species according to the lowest level identified

#order level
unique(data$order[data$genus==""])

#unique species within Bacillariales
unique(data$species[data$genus==""&data$order=="Bacillariales"])
data$phylum[data$genus==""&data$order=="Bacillariales"]<-"Bacillariophyta"
data$class[data$genus==""&data$order=="Bacillariales"]<-"Bacillariophyceae"
data$genus[data$genus==""&data$order=="Bacillariales"]<-"indet"
data$species[data$genus=="indet"&data$order=="Bacillariales"&data$species=="indet. (elliptic cylinder)"]<-"indet.ellipsoid"
data$species[data$genus=="indet"&data$order=="Bacillariales"&data$species=="indet. (rhombic prism)"]<-"indet.rhom"
data$species[data$genus=="indet"&data$order=="Bacillariales"&data$species=="indet."]<-"indet"


unique(data$species[data$genus==""&data$order=="Biddulphiales"])
data$phylum[data$genus==""&data$order=="Biddulphiales"]<-"Bacillariophyta"
data$class[data$genus==""&data$order=="Biddulphiales"]<-"Mediophyceae"
data$genus[data$genus==""&data$order=="Biddulphiales"]<-"indet"
data$species[data$genus=="indet"&data$order=="Biddulphiales"]<-"indet"

unique(data$species[data$genus==""&data$order=="Chlorococcales"])
data$phylum[data$genus==""&data$order=="Chlorococcales"]<-"Chlorophyta"
data$class[data$genus==""&data$order=="Chlorococcales"]<-"Chlorophyceae"
data$genus[data$genus==""&data$order=="Chlorococcales"]<-"indet"
data$species[data$genus=="indet"&data$order=="Chlorococcales"]<-"indet"
data$order[data$genus=="indet"&data$order=="Chlorococcales"]<-"Sphaeropleales"

unique(data$species[data$genus==""&data$order=="Coccolithales"])
data$phylum[data$genus==""&data$order=="Coccolithales"]<-"Haptophyta"
data$class[data$genus==""&data$order=="Coccolithales"]<-"Coccolithophyceae"
data$genus[data$genus==""&data$order=="Coccolithales"]<-"indet"
data$species[data$genus=="indet"&data$order=="Coccolithales"]<-"indet"

#class level
unique(data$class[data$genus==""])

unique(data$species[data$genus==""&data$class=="Coscinodiscophyceae"])
data$order[data$genus==""&data$class=="Coscinodiscophyceae"]<-"indet"
data$genus[data$genus==""&data$class=="Coscinodiscophyceae"]<-"indet"


unique(data$species[data$genus==""&data$class=="Cryptophyceae"])
data$phylum[data$genus==""&data$class=="Cryptophyceae"]<-"Cryptophyta"
data$order[data$genus==""&data$class=="Cryptophyceae"]<-"indet"
data$genus[data$genus==""&data$class=="Cryptophyceae"]<-"indet"


unique(data$phylum[data$genus==""&data$class=="Pyramimonadophyceae (Prasinophyceae)"])
unique(data$phylum[data$genus==""&data$class=="Prasinophyceae"])
data$class[data$genus==""&data$class=="Pyramimonadophyceae (Prasinophyceae)"]<-"Pyramimonadophyceae"
data$class[data$genus==""&data$class=="Prasinophyceae"]<-"Pyramimonadophyceae"
data$phylum[data$genus==""&data$class=="Pyramimonadophyceae"]<-"Haptophyta"
data$order[data$genus==""&data$class=="Pyramimonadophyceae"]<-"indet"
data$genus[data$genus==""&data$class=="Pyramimonadophyceae"]<-"indet"

unique(data$phylum[data$genus==""&data$class=="Dinophyceae"])
data$phylum[data$genus==""&data$class=="Dinophyceae"]<-"Dinophyta"
data$order[data$genus==""&data$class=="Dinophyceae"]<-"indet"
data$genus[data$genus==""&data$class=="Dinophyceae"]<-"indet"

unique(data$phylum[data$genus==""&data$class=="Coccolithophyceae (Prymnesiophyceae)"])
unique(data$phylum[data$genus==""&data$class=="Prymnesiophyceae"])
data$class[data$genus==""&data$class=="Prymnesiophyceae"]<-"Coccolithophyceae"
data$class[data$genus==""&data$class=="Coccolithophyceae (Prymnesiophyceae)"]<-"Coccolithophyceae"
data$phylum[data$genus==""&data$class=="Coccolithophyceae"]<-"Haptophyta"
data$order[data$genus==""&data$class=="Coccolithophyceae"]<-"indet"
data$genus[data$genus==""&data$class=="Coccolithophyceae"]<-"indet"

unique(data$phylum[data$genus==""&data$class=="Xanthophyceae"])
data$order[data$genus==""&data$class=="Xanthophyceae"]<-"indet"
data$genus[data$genus==""&data$class=="Xanthophyceae"]<-"indet"

unique(data$phylum[data$genus==""&data$class=="Cyanophyceae"])
unique(data$phylum[data$genus==""&data$class=="Cyanobacteria"])
data$phylum[data$genus==""&data$class=="Cyanobacteria"]<-"Cyanobacteria"
data$class[data$genus==""&data$phylum=="Cyanobacteria"]<-"Cyanophyceae"
data$order[data$genus==""&data$class=="Cyanophyceae"]<-"indet"
data$genus[data$genus==""&data$class=="Cyanophyceae"]<-"indet"

unique(data$phylum[data$genus==""&data$class=="Chlorophyceae"])
data$order[data$genus==""&data$class=="Chlorophyceae"]<-"indet"
data$genus[data$genus==""&data$class=="Chlorophyceae"]<-"indet"

unique(data$phylum[data$genus==""&data$class=="Euglenophyceae"])
data$phylum[data$genus==""&data$class=="Euglenophyceae"]<-"Euglenozoa"
data$order[data$genus==""&data$class=="Euglenophyceae"]<-"indet"
data$genus[data$genus==""&data$class=="Euglenophyceae"]<-"indet"

unique(data$phylum[data$genus==""&data$class=="Raphidophyceae"])
data$phylum[data$genus==""&data$class=="Raphidophyceae"]<-"Ochrophyta"
data$order[data$genus==""&data$class=="Raphidophyceae"]<-"indet"
data$genus[data$genus==""&data$class=="Raphidophyceae"]<-"indet"

unique(data$phylum[data$genus==""&data$class=="Chrysophyceae"])
data$phylum[data$genus==""&data$class=="Chrysophyceae"]<-"Ochrophyta"
data$order[data$genus==""&data$class=="Chrysophyceae"]<-"indet"
data$genus[data$genus==""&data$class=="Chrysophyceae"]<-"indet"

data$class[data$genus==""&data$class=="Flagellates"]<-"indet"
data$class[data$genus==""&data$class=="unbekannt"]<-"indet"
data$class[data$genus==""&data$class=="unknown"]<-"indet"
data$class[data$genus==""&data$class==""]<-"indet"
data$class[data$genus==""&data$class==""]<-"indet"

unique(data$phylum[data$genus==""])




data$phylum[data$genus==""&data$phylum=="Flagellates"]<-"indet"
data$phylum[data$genus==""&data$phylum=="Unknown"]<-"indet"
data$phylum[data$genus==""&data$phylum=="unknown"]<-"indet"
data$phylum[data$genus==""&data$phylum==""]<-"indet"
data$phylum[data$genus==""&data$phylum==""]<-"indet"


unique(data$phylum)
data$phylum[data$phylum=="chlorophyta"]<-"Chlorophyta"

unique(data$class)

unique(data$order)
data$order[data$phylum=="indet"&data$class=="indet"&data$order==""]<-"indet"
data$genus[data$phylum=="indet"&data$class=="indet"&data$genus==""]<-"indet"
unique(data$order)
unique(data$genus)

#for species not identified to species level we do not have consistent naming
#in order to use similar taxonomic units I separate only those which frequently occur

unique(data$species)
#to check whether there is more than 1 entry per species per sample 
#I mark these in the exclude column as not to species

data$exclude[data$species=="indet. kegelf."|                       
              data$species=="indet. herzf." |                       
              data$species=="indet" |                       
              data$species=="indet. (kegelförmig)" |                       
              data$species=="indet. oval" |                       
              data$species=="indet. rund"  |                       
              data$species== "indet. birnenf."   |                       
              data$species=="indet. (2,5 µm)" |                       
              data$species=="indet. (dunkelbraune Kugel)"|                       
              data$species=="indet. (fusiform)"   |                       
              data$species=="indet. (rund)"|                       
              data$species=="indet. (heart-shaped)" |                       
              data$species== "indet. (fädig)" |                       
              data$species=="indet. (athecat)" |                       
              data$species=="indet. (3-5 µm)"|                       
              data$species=="indet. (carrot-shaped)"|                       
              data$species=="indet. (athecate, ellipsoid)" |                       
              data$species=="indet. (sphere mit spiraliger Geißel)"|                       
              data$species=="indet. (sphere mit langer Geißel)" |                       
              data$species=="indet. (spheroid)" |                       
              data$species==""|                       
              data$species=="indet." |                       
              data$species== "indet. (ellipsoid)"  |                       
              data$species=="indet. (sphere)"  |                       
              data$species=="sp."    |    
              data$species=="indet. (oval)"    |                       
              data$species=="indet. (cylinder)"]<-"not.to.species"

unique(data$species[is.na(data$exclude)])

#create a unique identifier per taxonomy

data$UTI<-do.call(paste, c(data[c("phylum", "class","order","genus","species")], sep = "_"))


#check length of unique occurrences per UTI and USI
numspec <-
  data[!is.na(data$exclude)&data$species!="sp.",] %>%
  dplyr::group_by(USI,phylum,class,order, genus,species) %>%
  dplyr::summarize(N= length(species))

doubleton<-numspec[numspec$N>1,]
#using "sp." for all taxa identified to genus or order, "indet" for all higher
unique(data$species[!is.na(data$exclude)])

unique(data$UTI[(data$species=="")])
data$species[data$UTI=="Dinophyta_Dinophyceae_Peridiniales_Diplopsalis_"]<-"sp."
data$species[data$UTI=="indet_indet_indet_indet_"]<-"indet"
unique(data$order[(data$species=="sp.")])


unique(data$UTI[(data$species=="indet. (cylinder)")])
data$species[data$UTI=="Bacillariophyta_Coscinodiscophyceae_indet_indet_indet. (cylinder)"]<-"indet"
data$species[data$species=="indet."]<-"indet"

data$species[data$species=="indet. (ellipsoid)"]<-"indet.ellipsoid"

data$species[data$species=="indet. herzf."]<-"indet.heart"
data$species[data$species=="indet. (heart-shaped)"]<-"indet.heart"

data$species[data$species=="indet. kegelf."]<-"indet.cone"
data$species[data$species=="indet. (kegelförmig)"]<-"indet.cone"
data$species[data$species=="indet. (carrot-shaped)"]<-"indet.cone"

data$species[data$species=="indet. birnenf."]<-"indet.pear"

data$species[data$species=="indet. rund"]<-"indet.sphere"
data$species[data$species=="indet. (rund)"]<-"indet.sphere"
data$species[data$species=="indet. (spheroid)"]<-"indet.sphere"
data$species[data$species=="indet. (sphere)"]<-"indet.sphere"

data$species[data$species=="indet. oval"]<-"indet.oval"
data$species[data$species=="indet. (oval)"]<-"indet.oval"

data$species[data$species=="indet. (athecat)"]<-"indet.athec"
data$species[data$species=="indet. (athecate, ellipsoid)"]<-"indet.athec"

data$species[data$species=="indet. (fädig)"]<-"indet.filiform"
data$species[data$species=="indet. (fusiform)"]<-"indet.fusiform"

data$species[data$species=="indet. (2,5 µm)"]<-"indet.small"
data$species[data$species=="indet. (3-5 µm)"]<-"indet.mid"

data$species[data$species=="indet. (sphere mit langer Geißel)"]<-"indet.longflag"
data$species[data$species=="indet. (sphere mit spiraliger Geißel)"]<-"indet.spirflag"

data$species[data$species=="indet. (dunkelbraune Kugel)"]<-"indet.sphere"



#create a unique identifier for each occurrence
data$UTI<-do.call(paste, c(data[c("phylum", "class","order","genus","species")], sep = "_"))

data$UTSI<-do.call(paste, c(data[c("UTI", "USI")], sep = "_"))

data$num <- as.numeric(ave(data$species, data$UTSI, FUN = seq_along))
data$num[data$num==1]<-NA
summary(data)
data$specname<-do.call(paste, c(data[c("genus", "species")], sep = "_"))

data$specname.unique<-do.call(paste, c(data[c("genus", "species")], sep = "_"))
data$specname.unique[!is.na(data$num)]<-do.call(paste, c(data[!is.na(data$num),c("genus", "species","num")], sep = "_"))
unique(data$specname.unique)

#check whether same species occurs twice or more in one sample
numspec <-
  data %>%
  dplyr::group_by(USI,phylum, class, order, specname.unique) %>%
  dplyr::summarize(N= length(UTSI))

doubletons<-numspec[numspec$N>1,]

summary(data)

#finalize the data set vor volume calculations
data$phylum<-as.factor(data$phylum)
data$class<-as.factor(data$class)
data$order<-as.factor(data$order)
data$genus<-as.factor(data$genus)
data$species<-as.factor(data$species)
data$geom_ID<-as.factor(data$geom_ID)
data$gezählt.am<-NULL
data$Mikroskop<-NULL
data$Zähler<-NULL
data$Name.AlgaeBase.20200609<-NULL
data$specname<-as.factor(data$specname)
data$num<-NULL
data$specname.unique<-as.factor(data$specname.unique)

#create empty columns for new cell volume estimate
data$mean.size<-NA
data$cell1<-NA
data$cell2<-NA
data$cell3<-NA
data$cell4<-NA
data$cell5<-NA
data$cell6<-NA
data$cell7<-NA
data$cell8<-NA
data$cell9<-NA
data$cell10<-NA
data$cell11<-NA
data$cell12<-NA
data$cell13<-NA
data$cell14<-NA
data$cell15<-NA
data$cell16<-NA
data$cell17<-NA
data$cell18<-NA
data$cell19<-NA
data$cell20<-NA
data$cell21<-NA
data$cell22<-NA
data$cell23<-NA
data$cell24<-NA
data$cell25<-NA
data$cell26<-NA
data$cell27<-NA
data$cell28<-NA
data$cell29<-NA
data$cell30<-NA

data$equation<-NA
data$hidden<-NA


### Consistency check for all genera

# in the following for each genus I test whether multiple 
# volume analyses have been used

unique(data$genus)[1]
sel1 <- grep("Cylindrotheca", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
#diagnosis: Until 2014 DimB is the smaller dimension, from 2015 DimA
#Until 2014 DImC is needed, after 2015 not
data$equation[sel1]<- ifelse(
  data$geom_ID[sel1] == "15", "spindle15",
  ifelse(data$geom_ID[sel1] == "11","spindle11", NA))
summary(data[sel1,c(29:58)])
summary(data[sel1,c(59:88)])
summary(data[sel1,c(89:118)])


unique(data$genus)[2]
sel2 <- grep("Pseudo-nitzschia", data$genus)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac")])
xyplot(DimA~year|specname+geom_ID, data[sel2,])
xyplot(DimB~year|specname+geom_ID, data[sel2,])
xyplot(DimC~year|specname+geom_ID, data[sel2,])
#all species with same equation, 10 until 2010, 12 afterwards
#corr.fac is true
data$corr.fac[sel2]<-0.7
#diagnosis: DImensions consistent, but one mistake in 2010
summary(data[sel2,c("DimA","DimB","DimC")])
data$transient<-NA
data$transient[sel2][which(data$DimA[sel2] == 7.89)] <- 7.89
data$DimA[sel2][which(data$DimA[sel2] == 7.89)] <- 84.29
data$DimB[sel2][which(data$transient[sel2] == 7.89)] <- 7.89
data$DimC[sel2][which(data$transient[sel2] == 7.89)] <- 7.89
#one more DImA, DImC
data$DimA[sel2][which(data$DimA[sel2] == 9.00)] <- 20.00
data$DimB[sel2][which(data$DimB[sel2] == 20.00)] <- 9
data$DimC[sel2][which(data$DimC[sel2] == 15.00)] <- 7

data$equation[sel2]<- "rhomb"

# single cells
# same mistake as above. Single values of 100 in B1...B8
# and 80 in B9...B26, 70 in B27, and 50 in B28
# conversely, 8 in A 1to A27, 5 in A 28
# one more mistake in A1, B1
data$B1[sel2][which(data$B1[sel2] == 100)] <- 8
data$B1[sel2][which(data$B1[sel2] == 20)] <- 9
data$B2[sel2][which(data$B2[sel2] == 100)] <- 8
data$B3[sel2][which(data$B3[sel2] == 100)] <- 8
data$B4[sel2][which(data$B4[sel2] == 100)] <- 8
data$B5[sel2][which(data$B5[sel2] == 100)] <- 8
data$B6[sel2][which(data$B6[sel2] == 100)] <- 8
data$B7[sel2][which(data$B7[sel2] == 100)] <- 8
data$B8[sel2][which(data$B8[sel2] == 100)] <- 8
data$B9[sel2][which(data$B9[sel2] == 80)] <- 8
data$B10[sel2][which(data$B10[sel2] == 80)] <- 8
data$B11[sel2][which(data$B11[sel2] == 80)] <- 8
data$B12[sel2][which(data$B12[sel2] == 80)] <- 8
data$B13[sel2][which(data$B13[sel2] == 80)] <- 8
data$B14[sel2][which(data$B14[sel2] == 80)] <- 8
data$B15[sel2][which(data$B15[sel2] == 80)] <- 8
data$B16[sel2][which(data$B16[sel2] == 80)] <- 8
data$B17[sel2][which(data$B17[sel2] == 80)] <- 8
data$B18[sel2][which(data$B18[sel2] == 80)] <- 8
data$B19[sel2][which(data$B19[sel2] == 80)] <- 8
data$B20[sel2][which(data$B20[sel2] == 80)] <- 8
data$B21[sel2][which(data$B21[sel2] == 80)] <- 8
data$B22[sel2][which(data$B22[sel2] == 80)] <- 8
data$B23[sel2][which(data$B23[sel2] == 80)] <- 8
data$B24[sel2][which(data$B24[sel2] == 80)] <- 8
data$B25[sel2][which(data$B25[sel2] == 80)] <- 8
data$B26[sel2][which(data$B26[sel2] == 80)] <- 8
data$B27[sel2][which(data$B27[sel2] == 70)] <- 8
data$B28[sel2][which(data$B28[sel2] == 50)] <- 5
data$A1[sel2][which(data$A1[sel2] == 8)] <- 100
data$A1[sel2][which(data$A1[sel2] == 9)] <- 20
data$A2[sel2][which(data$A2[sel2] == 8)] <- 100
data$A3[sel2][which(data$A3[sel2] == 8)] <- 100
data$A4[sel2][which(data$A4[sel2] == 8)] <- 100
data$A5[sel2][which(data$A5[sel2] == 8)] <- 100
data$A6[sel2][which(data$A6[sel2] == 8)] <- 100
data$A7[sel2][which(data$A7[sel2] == 8)] <- 100
data$A8[sel2][which(data$A8[sel2] == 8)] <- 100
data$A9[sel2][which(data$A9[sel2] == 8)] <- 80
data$A10[sel2][which(data$A10[sel2] == 8)] <- 80
data$A11[sel2][which(data$A11[sel2] == 8)] <- 80
data$A12[sel2][which(data$A12[sel2] == 8)] <- 80
data$A13[sel2][which(data$A13[sel2] == 8)] <- 80
data$A14[sel2][which(data$A14[sel2] == 8)] <- 80
data$A15[sel2][which(data$A15[sel2] == 8)] <- 80
data$A16[sel2][which(data$A16[sel2] == 8)] <- 80
data$A17[sel2][which(data$A17[sel2] == 8)] <- 80
data$A18[sel2][which(data$A18[sel2] == 8)] <- 80
data$A19[sel2][which(data$A19[sel2] == 8)] <- 80
data$A20[sel2][which(data$A20[sel2] == 8)] <- 80
data$A21[sel2][which(data$A21[sel2] == 8)] <- 80
data$A22[sel2][which(data$A22[sel2] == 8)] <- 80
data$A23[sel2][which(data$A23[sel2] == 8)] <- 80
data$A24[sel2][which(data$A24[sel2] == 8)] <- 80
data$A25[sel2][which(data$A25[sel2] == 8)] <- 80
data$A26[sel2][which(data$A26[sel2] == 8)] <- 80
data$A27[sel2][which(data$A27[sel2] == 8)] <- 70
data$A28[sel2][which(data$A28[sel2] == 5)] <- 50

summary(data[sel2,c(29:58)])
summary(data[sel2,c(59:88)])
summary(data[sel2,c(89:118)])

unique(data$genus)[3]
sel1 <- grep("Actinoptychus", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
data$equation[sel1]<- "cylinder"
summary(as.factor(data$equation))
summary(data[sel1,c(29:58)])
summary(data[sel1,c(59:88)])


unique(data$genus)[4]
sel1 <- grep("Brockmanniella", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
data$equation[sel1]<- "elliptic.cyl"
summary(as.factor(data$equation))
summary(data[sel1,c(29:58)])
summary(data[sel1,c(59:88)])
summary(data[sel1,c(89:118)])

unique(data$genus)[5]
sel1 <- grep("Chaetoceros", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC","HDB","HDC")])
data$equation[sel1][which(data$geom_ID[sel1]=="4")]<- "cylinder"
data$equation[sel1][which(data$geom_ID[sel1]=="5")]<- "elliptic.cyl"
data$equation[sel1][which(data$geom_ID[sel1]=="11")]<- "elliptic.cyl"
summary(as.factor(data$equation))

unique(data$genus)[6]
sel1 <- grep("Leptocylindrus", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
#diagnosis: DImensions consistent, but one mistake in 2008 (danicus)
#and one in 2007 (minimus)
summary(data[sel1,c("DimA","DimB","DimC")])
summary(data[data$specname=="Leptocylindrus_danicus",c("DimA","DimB","DimC")])
data$transient[data$specname=="Leptocylindrus_danicus"][which(data$DimB[data$specname=="Leptocylindrus_danicus"] == 8.00)] <- 8.00
data$DimB[data$specname=="Leptocylindrus_danicus"][which(data$DimB[data$specname=="Leptocylindrus_danicus"] == 8.00)] <- 25.00
data$DimA[data$specname=="Leptocylindrus_danicus"][which(data$transient[data$specname=="Leptocylindrus_danicus"] == 8.00)] <- 8.00
summary(data[data$specname=="Leptocylindrus_minimus",c("DimA","DimB","DimC")])
data$DimB[data$specname=="Leptocylindrus_minimus"][which(data$DimB[data$specname=="Leptocylindrus_minimus"] == 3.00)] <- 30.00
summary(data[sel1,c("DimA","DimB","DimC","HDB","HDC")])
data$equation[sel1]<- "cylinder"

#check for similar measurement conversions for both species
sel2 <- grep("Leptocylindrus_danicus", data$specname)
summary(data[sel2,c(29:58)])# A1 to A4 has 25, A6 to A8 has 60
summary(data[sel2,c(59:88)])# B1 to B4 has 8, B6 to B8 has 15
data$B1[sel2][which(data$B1[sel2] == 8)] <- 25
data$B2[sel2][which(data$B2[sel2] == 8)] <- 25
data$B3[sel2][which(data$B3[sel2] == 8)] <- 25
data$B4[sel2][which(data$B4[sel2] == 8)] <- 25
data$B6[sel2][which(data$A6[sel2] == 60)] <- 60
data$B7[sel2][which(data$A7[sel2] == 60)] <- 60
data$B8[sel2][which(data$A8[sel2] == 60)] <- 60
data$A1[sel2][which(data$A1[sel2] == 25)] <- 8
data$A2[sel2][which(data$A2[sel2] == 25)] <- 8
data$A3[sel2][which(data$A3[sel2] == 25)] <- 8
data$A4[sel2][which(data$A4[sel2] == 25)] <- 8
data$A6[sel2][which(data$A6[sel2] == 60)] <- 15
data$A7[sel2][which(data$A7[sel2] == 60)] <- 15
data$A8[sel2][which(data$A8[sel2] == 60)] <- 15


sel2 <- grep("Leptocylindrus_minimus", data$specname)
summary(data[sel2,c(29:58)])# A29,A30 has 30
summary(data[sel2,c(59:88)])# B29,B30 has 15
data$B29[sel2][which(data$A29[sel2] == 30)] <- 30
data$B30[sel2][which(data$A30[sel2] == 30)] <- 30
data$A29[sel2][which(data$A29[sel2] == 30)] <- 15
data$A30[sel2][which(data$A30[sel2] == 30)] <- 15



unique(data$genus)[7]
sel1 <- grep("Odontella", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
unique(data$specname[sel1])
data$equation[sel1][which(data$specname[sel1]=="Odontella_rhombus")]<- "rhomb"
data$equation[sel1][which(data$geom_ID[sel1]=="5")]<- "elliptic.cyl"
data$equation[sel1][which(data$geom_ID[sel1]=="11")]<- "elliptic.cyl"
data$equation[sel1][which(data$specname[sel1]=="Odontella_rhombus f. trigona")]<- "triang.prism"



unique(data$genus)[8]
sel1 <- grep("Trieres", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
unique(data$specname[sel1])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[9]
sel1 <- grep("Plagiogrammopsis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","HDB","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-0.9
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
xyplot(DimA~DimB|as.factor(year), data[sel1,])
xyplot(DimA~DimC|as.factor(year), data[sel1,])
xyplot(DimB~DimC|as.factor(year), data[sel1,])
xyplot(A1~C1|as.factor(year), data[sel1,])
plot(DimA~DimC, data[sel1,])
plot(DimC~DimB, data[sel1,])
hist(data$C1[sel1])
# it seems from 2009 to 2014, DimB is just equal DimC
# more differentiated and smaller after and before
# B rarely measured, used as 0.2 A
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "rhomb2"

unique(data$genus)[10]
sel1 <- grep("Rhizosolenia", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-0.9
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

unique(data$genus)[11]
sel1 <- grep("Skeletonema", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-0.9
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


unique(data$genus)[12]
sel1 <- grep("Thalassiosira", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

unique(data$genus)[13]
sel1 <- grep("indet", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|geom_ID, data[sel1,])
xyplot(DimB~year|geom_ID, data[sel1,])
xyplot(DimC~year|geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

data$equation[sel1][which(data$geom_ID[sel1]=="1")]<- "sphere"
data$equation[sel1][which(data$geom_ID[sel1]=="11")]<- "elliptic.cyl"
data$equation[sel1][which(data$geom_ID[sel1]=="5")]<- "elliptic.cyl"
data$equation[sel1][which(data$geom_ID[sel1]=="2")]<- "spheroid"
data$equation[sel1][which(data$geom_ID[sel1]=="3")]<- "ellipsoid"
data$equation[sel1][which(data$geom_ID[sel1]=="4")]<- "cylinder"
data$equation[sel1][which(data$geom_ID[sel1]=="12")]<- "rhomb"
data$equation[sel1][which(data$geom_ID[sel1]=="10")]<- "rhomb"
data$equation[sel1][which(data$geom_ID[sel1]=="7")]<- "cone"
data$equation[sel1][which(data$geom_ID[sel1]=="8")]<- "double.cone"
data$equation[sel1][which(data$geom_ID[sel1]=="9")]<- "ellipsoid"
data$equation[sel1][which(data$geom_ID[sel1]=="16")]<- "cone.half.sphere"

unique(data$genus)[14]
sel1 <- grep("Asterionellopsis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-.9
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
#the dimensions have different names but this does not matter
#as simple product is taken
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "pyramid"

unique(data$genus)[15]
sel1 <- grep("Thalassionema", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-.89
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
#the dimensions have different names but this does not matter
#as simple product is taken
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cuboid"

unique(data$genus)[16]
sel1 <- grep("Plagioselmis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cone.half.sphere"

unique(data$genus)[17]
sel1 <- grep("Teleaulax", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cone.half.sphere"


unique(data$genus)[18]
sel1 <- grep("Cymatosira", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[19]
sel1 <- grep("Eunotogramma", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"


unique(data$genus)[20]
sel1 <- grep("Paralia", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

unique(data$genus)[21]
sel1 <- grep("Hemiselmis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cone.half.sphere"


unique(data$genus)[22]
sel1 <- grep("Gyrodinium", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
#it seems the two speies have been evaluated diferently across time
summary(data[data$specname=="Gyrodinium_sp.",c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
#ellipsoid, with dimension B being small >2014, replaced by C
data$transient[data$specname=="Gyrodinium_sp."][which(data$year[data$specname=="Gyrodinium_sp."]>2014)] <- data$DimB[data$specname=="Gyrodinium_sp."][which(data$year[data$specname=="Gyrodinium_sp."]>2014)]
data$DimB[data$specname=="Gyrodinium_sp."][which(data$year[data$specname=="Gyrodinium_sp."]>2014)] <- data$DimC[data$specname=="Gyrodinium_sp."][which(data$year[data$specname=="Gyrodinium_sp."]>2014)]
data$DimC[data$specname=="Gyrodinium_sp."][which(data$year[data$specname=="Gyrodinium_sp."]>2014)] <- data$transient[data$specname=="Gyrodinium_sp."][which(data$year[data$specname=="Gyrodinium_sp."]>2014)]
summary(data[data$specname=="Gyrodinium_spirale",c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
#double cone, consistent
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1][which(data$specname[sel1]=="Gyrodinium_spirale")]<- "double.cone"
data$equation[sel1][which(data$specname[sel1]=="Gyrodinium_sp.")]<- "ellipsoid"
#no change in species needed as dimensions are somply multiplied in ellipsoid


unique(data$genus)[23]
sel1 <- grep("Phaeocystis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "sphere"


unique(data$genus)[24]
sel1 <- grep("Guinardia", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


unique(data$genus)[25]
sel1 <- grep("Lithodesmium", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "triang.prism"


unique(data$genus)[26]
sel1 <- grep("Asteroplanus", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
#DIm C only measured partially, additionally, Dim C, B and A seem to have switched 
#for last two years. Now we make Dim A the largest dimension 
data$transient[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2017)] <- data$DimA[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2017)]
data$DimA[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2017)] <- data$DimC[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2017)]
data$DimC[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2017)] <- data$transient[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2017)]
data$transient[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2006&data$year[data$specname=="Asteroplanus_karianus"]<2015)] <- data$DimA[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2006&data$year[data$specname=="Asteroplanus_karianus"]<2015)]
data$DimA[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2006&data$year[data$specname=="Asteroplanus_karianus"]<2015)] <- data$DimB[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2006&data$year[data$specname=="Asteroplanus_karianus"]<2015)]
data$DimB[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2006&data$year[data$specname=="Asteroplanus_karianus"]<2015)] <- data$transient[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2006&data$year[data$specname=="Asteroplanus_karianus"]<2015)]
#add this info also to Dim C
data$DimC[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2006&data$year[data$specname=="Asteroplanus_karianus"]<2015)] <- data$transient[data$specname=="Asteroplanus_karianus"][which(data$year[data$specname=="Asteroplanus_karianus"]>2006&data$year[data$specname=="Asteroplanus_karianus"]<2015)]
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"


unique(data$genus)[27]
sel1 <- grep("Delphineis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[28]
sel1 <- grep("Rhaphoneis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[29]
sel1 <- grep("Diplopsalis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
#changed between sphere and spheroid, but mostly sphere
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "sphere"

unique(data$genus)[30]
sel1 <- grep("Gymnodinium", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "ellipsoid"


unique(data$genus)[30]
sel1 <- grep("Gymnodinium", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
#minor portion of data < 2010 with only 2 dimensions
#no direct way to fill up
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "ellipsoid"

unique(data$genus)[31]
sel1 <- grep("Prorocentrum", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "ellipsoid"


unique(data$genus)[32]
sel1 <- grep("Protoperidinium", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
#different species with different forms and correction factors
unique(data$specname[sel1])
sel2 <- grep("Protoperidinium_conicum", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-1
data$equation[sel2]<- "double.cone"
sel2 <- grep("Protoperidinium_bipes", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-.5
data$equation[sel2]<- "cone.half.sphere"
sel2 <- grep("Protoperidinium_claudicans", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-1
data$equation[sel2]<- "ellipsoid"
sel2 <- grep("Protoperidinium_divergens", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-.8
data$equation[sel2]<- "cone.half.sphere"
sel2 <- grep("Protoperidinium_depressum", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-.8
data$equation[sel2]<- "cone.half.sphere"
sel2 <- grep("Protoperidinium_curtipes", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-.7
data$equation[sel2]<- "double.cone"
sel2 <- grep("Protoperidinium_pentagonum", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-1
data$equation[sel2]<- "ellipsoid"
sel2 <- grep("Protoperidinium_subinerme", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-1
data$equation[sel2]<- "ellipsoid"
sel2 <- grep("Protoperidinium_pyriforme", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-1
data$equation[sel2]<- "ellipsoid"
sel2 <- grep("Protoperidinium_sp.", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-1
data$equation[sel2]<- "cone.half.sphere"


unique(data$genus)[33]
sel1 <- grep("Mediopyxis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[34]
sel1 <- grep("Helicotheca", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
#dimensions differ but no issue as simple product
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cuboid"


unique(data$genus)[35]
sel1 <- grep("Leucocryptos", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cone.half.sphere"

unique(data$genus)[36]
sel1 <- grep("Karenia", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "ellipsoid"

unique(data$genus)[37]
sel1 <- grep("Scrippsiella", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spheroid"


unique(data$genus)[38]
sel1 <- grep("Plagiolemma", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "lanceol.cyl"


unique(data$genus)[39]
sel1 <- grep("Eutreptiella", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spheroid"

unique(data$genus)[40]
sel1 <- grep("Pyramimonas", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
#different species
summary(data[data$specname=="Pyramimonas_longicauda",c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$equation[data$specname=="Pyramimonas_longicauda"]<- "cone"
summary(data[data$specname=="Pyramimonas_sp.",c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$equation[data$specname=="Pyramimonas_sp."]<- "spheroid"


unique(data$genus)[41]
sel1 <- grep("Pleurosigma", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "rhomb"

unique(data$genus)[42]
sel1 <- grep("Cerataulina", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


unique(data$genus)[43]
sel1 <- grep("Eucampia", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[44]
sel1 <- grep("Lennoxia", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
data$equation[sel1]<- ifelse(
  data$geom_ID[sel1] == "15", "spindle15",
  ifelse(data$geom_ID[sel1] == "11","spindle11", NA))

unique(data$genus)[45]
sel1 <- grep("Heterocapsa", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- ifelse(
  data$geom_ID[sel1] == "16", "cone.half.sphere",
  ifelse(data$geom_ID[sel1] == "11","double.cone", "spheroid"))


unique(data$genus)[46]
sel1 <- grep("Coscinodiscus", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


unique(data$genus)[47]
sel1 <- grep("Detonula", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


unique(data$genus)[48]
sel1 <- grep("Ditylum", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "triang.prism"

unique(data$genus)[49]
sel1 <- grep("Microspora", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


unique(data$genus)[50]
sel1 <- grep("Akashiwo", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "ellipsoid"

unique(data$genus)[51]
sel1 <- grep("Gyrosigma", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[52]
sel1 <- grep("Achnanthes", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[53]
sel1 <- grep("Pseudopediastrum", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-0.67
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
#form is cuboid but instead of DimC, a correction factor is given
#call this cuboid1 unsing DimB² and CF
data$equation[sel1]<- "cuboid1"


unique(data$genus)[54]
sel1 <- grep("Pterosperma", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "sphere"


unique(data$genus)[55]
sel1 <- grep("Bacillaria", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-.95
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cuboid"

unique(data$genus)[56]
sel1 <- grep("Scenedesmus", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spheroid"

unique(data$genus)[57]
sel1 <- grep("Entomoneis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"


unique(data$genus)[58]
sel1 <- grep("Actinocyclus", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

unique(data$genus)[59]
sel1 <- grep("Licmophora", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-.9
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "pyramid"

unique(data$genus)[60]
sel1 <- grep("Amphidinium", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- ifelse(
  data$geom_ID[sel1] == "3", "ellipsoid",
  ifelse(data$geom_ID[sel1] == "2","spheroid", NA))


unique(data$genus)[61]
sel1 <- grep("Tripos", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed","HDB","HDC")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_corp, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])

# a special focus on Ceratium volume with a new way to assess it was
# used from 2015 onwards (id=17), previously fusus was id=8, others id=7
# this produces exorbitant cell volumes
# rearrange per species
sel2 <- grep("Tripos_furca", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
plot(DimA~DimB,data[sel2,])
xyplot(DimA~year|geom_ID, data[sel2,])
xyplot(DimB~year|geom_ID, data[sel2,])
#dimensions are used in a very similar way
data$equation[sel2]<- ifelse(
  data$geom_ID[sel2] == "17", "sphere",
  ifelse(data$geom_ID[sel2] == "7","spheroid", NA))

sel2 <- grep("Tripos_horridus", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
plot(DimA~DimB,data[sel2,])
xyplot(DimA~year|geom_ID, data[sel2,])
xyplot(DimB~year|geom_ID, data[sel2,])
#dimensions are used in a very similar way
data$equation[sel2]<- ifelse(
  data$geom_ID[sel2] == "17", "sphere",
  ifelse(data$geom_ID[sel2] == "7","spheroid", NA))

sel2 <- grep("Tripos_lineatus", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
plot(DimA~DimB,data[sel2,])
xyplot(DimA~year|geom_ID, data[sel2,])
xyplot(DimB~year|geom_ID, data[sel2,])
#dimensions are used in a very similar way
data$equation[sel2]<- ifelse(
  data$geom_ID[sel2] == "17", "sphere",
  ifelse(data$geom_ID[sel2] == "7","spheroid", NA))

sel2 <- grep("Tripos_muelleri", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
plot(DimA~DimB,data[sel2,])
xyplot(DimA~year|geom_ID, data[sel2,])
xyplot(DimB~year|geom_ID, data[sel2,])
#dimensions are used in a very similar way
data$equation[sel2]<- ifelse(
  data$geom_ID[sel2] == "17", "sphere",
  ifelse(data$geom_ID[sel2] == "7","spheroid", NA))

sel2 <- grep("Tripos_fusus", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
plot(DimA~DimB,data[sel2,])
xyplot(DimA~year|geom_ID, data[sel2,])
xyplot(DimB~year|geom_ID, data[sel2,])
#dimensions are used in a very similar way
data$equation[sel2]<- ifelse(
  data$geom_ID[sel2] == "17", "sphere",
  ifelse(data$geom_ID[sel2] == "7","spheroid", 
         ifelse(data$geom_ID[sel2] == "8","double.cone", NA)))



unique(data$genus)[62]
sel1 <- grep("Biddulphia", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "triang.prism"

unique(data$genus)[63]
sel1 <- grep("Bellerochea", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "triang.prism"

unique(data$genus)[64]
sel1 <- grep("Melosira", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

unique(data$genus)[65]
sel1 <- grep("Coelastrum", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "sphere"

unique(data$genus)[66]
sel1 <- grep("Proboscia", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
#one exchange of dimensions obvious for P. indica
summary(data[data$specname=="Proboscia_indica",c("DimA","DimB","DimC")])
plot(DimA~DimB,data[data$specname=="Proboscia_indica",c("DimA","DimB","DimC")])
data$DimB[data$specname=="Proboscia_indica"][which(data$DimA[data$specname=="Proboscia_indica"] == 230.00)] <- 230.00
data$DimA[data$specname=="Proboscia_indica"][which(data$DimA[data$specname=="Proboscia_indica"] == 230.00)] <- 30.00
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


unique(data$genus)[67]
sel1 <- grep("Lebouridinium", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-.5
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spindle15"


unique(data$genus)[68]
sel1 <- grep("Bacteriastrum", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


unique(data$genus)[69]
sel1 <- grep("Porosira", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


unique(data$genus)[70]
sel1 <- grep("Podosira", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-.83
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


unique(data$genus)[71]
sel1 <- grep("Dactyliosolen", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


unique(data$genus)[72]
sel1 <- grep("Stephanopyxis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-.87
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


unique(data$genus)[73]
sel1 <- grep("Meuniera", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[74]
sel1 <- grep("Navicula", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[75]
sel1 <- grep("Dictyocha", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-.5
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "sphere"

unique(data$genus)[76]
sel1 <- grep("Diploneis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-.5
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"


unique(data$genus)[77]
sel1 <- grep("Planktothrix", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
#for one sample, P aghardii obvioulsy not height of single cells
summary(data[sel1,c("DimA","DimB","DimC")])
data$DimB[data$specname=="Planktothrix_agardhii"][which(data$DimB[data$specname=="Planktothrix_agardhii"] == 100)] <- NA
data$equation[sel1]<- "cylinder"

unique(data$genus)[78]
sel1 <- grep("Subsilicea", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"


unique(data$genus)[79]
sel1 <- grep("Lauderia", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

unique(data$genus)[80]
sel1 <- grep("Binuclearia", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-.9
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


unique(data$genus)[81]
sel1 <- grep("Nitzschia", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
#differences between species
unique(data$specname[sel1])
sel2 <- grep("Nitzschia_reversa", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-1
data$equation[sel2]<- "elliptic.cyl"
sel2 <- grep("Nitzschia_acicularis", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-1
data$equation[sel2]<- "spindle15"
sel2 <- grep("Nitzschia_lorenziana var. incerta", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-1
data$equation[sel2]<- "elliptic.cyl"
sel2 <- grep("Nitzschia_sp.", data$specname)
summary(data[sel2,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel2]<-1.15
data$equation[sel2]<- "rhomb"

unique(data$genus)[82]
sel1 <- grep("Pseudopedinella", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- ifelse(
  data$geom_ID[sel1] == "1", "sphere",
  ifelse(data$geom_ID[sel1] == "2","spheroid", NA))

unique(data$genus)[83]
sel1 <- grep("Phalacroma", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "ellipsoid"


unique(data$genus)[84]
sel1 <- grep("Monoraphidium", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- ifelse(
  data$geom_ID[sel1] == "15", "spindle15",
  ifelse(data$geom_ID[sel1] == "8","double.cone", NA))

unique(data$genus)[85]
sel1 <- grep("Euglena", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "ellipsoid"

unique(data$genus)[86]
sel1 <- grep("Surirella", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[87]
sel1 <- grep("Aulacoseira", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

unique(data$genus)[88]
sel1 <- grep("Tetrastrum", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cone.half.sphere"

unique(data$genus)[89]
sel1 <- grep("Oocystis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spheroid"


unique(data$genus)[90]
sel1 <- grep("Merismopedia", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "sphere"

unique(data$genus)[91]
sel1 <- grep("Dissodinium", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spindle15"

unique(data$genus)[92]
sel1 <- grep("Corymbellus", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spheroid"

unique(data$genus)[93]
sel1 <- grep("Mesoporos", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spheroid"


unique(data$genus)[94]
sel1 <- grep("Triceratium", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "triang.prism"

unique(data$genus)[95]
sel1 <- grep("Cymbella", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-.8
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[96]
sel1 <- grep("Tetraedron", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-.75
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cuboid"

unique(data$genus)[97]
sel1 <- grep("Torodinium", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spheroid"

unique(data$genus)[98]
sel1 <- grep("Neocalyptrella", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "double.cone"

unique(data$genus)[99]
sel1 <- grep("Limnothrix", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

unique(data$genus)[100]
sel1 <- grep("Asterionella", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cuboid"

unique(data$genus)[101]
sel1 <- grep("Pediastrum", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-.67
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
#see Pseudopediastrum, but as C is never given, make it a A*B*B ciboid
data$equation[sel1]<- "cuboid1"

unique(data$genus)[102]
sel1 <- grep("Chroococcus", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spheroid"

unique(data$genus)[103]
sel1 <- grep("Alexandrium", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- ifelse(
  data$geom_ID[sel1] == "1", "sphere",
  ifelse(data$geom_ID[sel1] == "2","spheroid", NA))


unique(data$genus)[104]
sel1 <- grep("Oscillatoria", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

unique(data$genus)[105]
sel1 <- grep("Corethron", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

unique(data$genus)[106]
sel1 <- grep("Dinophysis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "ellipsoid"

unique(data$genus)[107]
sel1 <- grep("Ankistrodesmus", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "ellipsoid"

unique(data$genus)[108]
sel1 <- grep("Attheya", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[109]
sel1 <- grep("Pyrocystis", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "ellipsoid"

unique(data$genus)[110]
sel1 <- grep("Planktolyngbya", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

unique(data$genus)[111]
sel1 <- grep("Peridiniella", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "sphere"

unique(data$genus)[112]
sel1 <- grep("Trachelomonas", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spheroid"

unique(data$genus)[113]
sel1 <- grep("Diatoma", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[114]
sel1 <- grep("Chrysochromulina", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spheroid"

unique(data$genus)[115]
sel1 <- grep("Fragilaria", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cuboid"

unique(data$genus)[116]
sel1 <- grep("Synura", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "elliptic.cyl"

unique(data$genus)[117]
sel1 <- grep("Katodinium", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spheroid"

unique(data$genus)[118]
sel1 <- grep("Synedra", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cuboid"

unique(data$genus)[119]
sel1 <- grep("Aulacodiscus", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

unique(data$genus)[120]
sel1 <- grep("Pyrophacus", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "sphere"

unique(data$genus)[121]
sel1 <- grep("Staurastrum", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "sphere"

unique(data$genus)[122]
sel1 <- grep("Ochromonas", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spheroid"

unique(data$genus)[123]
sel1 <- grep("Cryptomonas", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "spheroid"

unique(data$genus)[124]
sel1 <- grep("Pachysphaera", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "sphere"

unique(data$genus)[125]
sel1 <- grep("Gloeocapsa", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "sphere"

unique(data$genus)[126]
sel1 <- grep("Stephanodiscus", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"

unique(data$genus)[127]
sel1 <- grep("Cyclotella", data$genus)
summary(data[sel1,c("geom_ID","geom_corp","corr.fac","DimA.needed","DimB.needed","DimC.needed")])
data$corr.fac[sel1]<-1
xyplot(DimA~year|specname+geom_ID, data[sel1,])
xyplot(DimB~year|specname+geom_ID, data[sel1,])
xyplot(DimC~year|specname+geom_ID, data[sel1,])
summary(data[sel1,c("DimA","DimB","DimC")])
data$equation[sel1]<- "cylinder"


#Creating the final file

summary(as.factor(data$equation))
source('~/R/phytosize/scripts/biovol.R')
summary(data$mean.size)
bwplot(log(mean.size)~genus|phylum,data[!is.na(data$mean.size),])        
bwplot(log(cell1)~genus|phylum,data[!is.na(data$mean.size),])        
bwplot(log(cell1)~as.factor(equation),data[!is.na(data$mean.size),])        
plot(log(cell1)~log(mean.size),data)

summary(data[,139:168])

hist(log(data$mean.size))


write.csv(data,'~/R/phytosize/data/ppall_corr.csv')
