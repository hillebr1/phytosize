### Measurement of cell volumes by geometric figure

## spindle 15
#required A and B, fully measurable
spindle15<-grep("spindle15", data$equation)
data$mean.size[spindle15]<- 2/15*pi*data$DimA[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15]
# for single cells
# if Ai and Bi are given, use both
# if Ai is given, without Bi, use DimB

## spindle11
#spindle11 needs A and B, sometimes, even if rarely, C is involved

spindle11<-grep("spindle11", data$equation)
data$mean.size[spindle11]<- 2/15*pi*data$DimA[spindle11]*data$DimB[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11]
# for single cells
# if Ai, Bi and Ci are given, use all
# if Ai, Bi and mean C are given, use Ai, Bi and DimC
# if Ai and Bi, use both and B²
# if Ai is given and DimB, use both and B2

## rhomb needs A, B and C, C is almost never given
rhomb<-grep("rhomb", data$equation)
data$mean.size[rhomb]<- 1/2*data$DimA[rhomb]*data$DimB[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb]


pyramid<-grep("pyramid", data$equation)

data$mean.size[pyramid]<- 1/3*data$DimA[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid]

## Cylinder
# A & B needed, always present
cylinder<-grep("cylinder", data$equation)
data$mean.size[cylinder]<- 1/4*pi*data$DimA[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder]


elliptic.cyl<-grep("elliptic.cyl", data$equation)
data$mean.size[elliptic.cyl]<- 1/4*pi*data$DimA[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl]


lanceol.cyl<-grep("lanceol.cyl", data$equation)
data$mean.size[lanceol.cyl]<- 2/pi*data$DimA[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl]


triang.prism<-grep("triang.prism", data$equation)

data$mean.size[triang.prism]<- 1/2*data$DimA[triang.prism]*data$DimB[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism]


sphere<-grep("sphere", data$equation)

data$mean.size[sphere]<- 1/6*pi*data$DimA[sphere]**3*data$corr.fac[sphere]


spheroid<-grep("spheroid", data$equation)

data$mean.size[spheroid]<- 1/6*pi*data$DimA[spheroid]**2*data$DimB[spheroid]*data$corr.fac[spheroid]

ellipsoid<-grep("ellipsoid", data$equation)

data$mean.size[ellipsoid]<- 1/6*pi*data$DimA[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid]

cone<-grep("cone", data$equation)

data$mean.size[cone]<- 1/12*pi*data$DimA[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$DimA[cone]**2))*data$corr.fac[cone]

cone.half.sphere<-grep("cone.half.sphere", data$equation)

data$mean.size[cone.half.sphere]<- 1/12*pi*data$DimA[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$DimA[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere]

double.cone<-grep("double.cone", data$equation)

data$mean.size[double.cone]<- 1/12*pi*data$DimA[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone]




cuboid<-grep("cuboid", data$equation)


data$mean.size[cuboid]<- data$DimA[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid]


cuboid1<-grep("cuboid1", data$equation)


data$mean.size[cuboid1]<- data$DimA[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1]



### Single cell measurements

#Cell1

data$cell1[spindle15]<-ifelse(!is.na(data$A1[spindle15])&!is.na(data$B1[spindle15]),
                              2/15*pi*data$A1[spindle15]**2*data$B1[spindle15]*data$corr.fac[spindle15],
                              ifelse(!is.na(data$A1[spindle15]),
                                     2/15*pi*data$A1[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                     NA))

data$cell1[spindle11]<-
  ifelse(!is.na(data$A1[spindle11])&!is.na(data$B1[spindle11])&!is.na(data$C1[spindle11]),
  2/15*pi*data$A1[spindle11]*data$B1[spindle11]*data$C1[spindle11]*data$corr.fac[spindle11],
  ifelse(!is.na(data$A1[spindle11])&!is.na(data$B1[spindle11])&!is.na(data$DimC[spindle11]),
         2/15*pi*data$A1[spindle11]*data$B1[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A1[spindle11])&!is.na(data$B1[spindle11]),
                2/15*pi*data$A1[spindle11]*data$B1[spindle11]*data$B1[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A1[spindle11])&!is.na(data$DimB[spindle11]),
                       2/15*pi*data$A1[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                       NA))))

data$cell1[rhomb]<-
  ifelse(!is.na(data$A1[rhomb])&!is.na(data$B1[rhomb])&!is.na(data$C1[rhomb]),
  1/2*data$A1[rhomb]*data$B1[rhomb]*data$C1[rhomb]*data$corr.fac[rhomb],
  ifelse(!is.na(data$A1[rhomb])&!is.na(data$B1[rhomb])&!is.na(data$DimC[rhomb]),
         1/2*data$A1[rhomb]*data$B1[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A1[rhomb])&!is.na(data$B1[rhomb]),
                1/2*data$A1[rhomb]*data$B1[rhomb]*.75*data$B1[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A1[rhomb])&!is.na(data$DimB[rhomb]),
                       1/2*data$A1[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                       NA))))


data$cell1[cylinder]<-ifelse(!is.na(data$A1[cylinder])&!is.na(data$B1[cylinder]),
                             1/4*pi*data$A1[cylinder]**2*data$B1[cylinder]*data$corr.fac[cylinder],
                             ifelse(!is.na(data$A1[cylinder]),
                                     1/4*pi*data$A1[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell1[elliptic.cyl]<-
  ifelse(!is.na(data$A1[elliptic.cyl])&!is.na(data$B1[elliptic.cyl])&!is.na(data$C1[elliptic.cyl]),
         1/4*pi*data$A1[elliptic.cyl]*data$B1[elliptic.cyl]*data$C1[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A1[elliptic.cyl])&!is.na(data$B1[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A1[elliptic.cyl]*data$B1[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A1[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C1[elliptic.cyl]),
                       1/4*pi*data$A1[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C1[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A1[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A1[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell1[triang.prism]<-
  ifelse(!is.na(data$A1[triang.prism])&!is.na(data$B1[triang.prism])&!is.na(data$C1[triang.prism]),
         1/2*data$A1[triang.prism]*data$B1[triang.prism]*data$C1[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A1[triang.prism])&!is.na(data$B1[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A1[triang.prism]*data$B1[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A1[triang.prism])&!is.na(data$B1[triang.prism]),
                       1/2*data$A1[triang.prism]*data$B1[triang.prism]*.75*data$B1[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A1[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A1[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell1[sphere]<-
  ifelse(!is.na(data$A1[sphere]),
         1/6*pi*data$A1[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell1[spheroid]<-
  ifelse(!is.na(data$A1[spheroid])&!is.na(data$B1[spheroid]),
         1/6*pi*data$A1[spheroid]*data$B1[spheroid]*data$A1[spheroid]*data$corr.fac[spheroid],
                 ifelse(!is.na(data$A1[spheroid])&!is.na(data$DimB[spheroid]),
                       1/6*pi*data$A1[spheroid]*data$DimB[spheroid]*data$A1[spheroid]*data$corr.fac[spheroid],
                              NA))

data$cell1[ellipsoid]<-
  ifelse(!is.na(data$A1[ellipsoid])&!is.na(data$B1[ellipsoid])&!is.na(data$C1[ellipsoid]),
         1/6*pi*data$A1[ellipsoid]*data$B1[ellipsoid]*data$C1[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A1[ellipsoid])&!is.na(data$B1[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A1[ellipsoid]*data$B1[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A1[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C1[ellipsoid]),
                       1/6*pi*data$A1[ellipsoid]*data$DimB[ellipsoid]*data$C1[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A1[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A1[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell1[cone]<-
  ifelse(!is.na(data$A1[cone])&!is.na(data$B1[cone]),
         1/12*pi*data$A1[cone]**2*(sqrt(data$B1[cone]**2-.25*data$A1[cone]**2))*data$corr.fac[cone],
                   ifelse(!is.na(data$A1[cone])&!is.na(data$DimB[cone]),
                       1/12*pi*data$A1[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A1[cone]**2))*data$corr.fac[cone],
                             NA))

data$cell1[cone.half.sphere]<-
  ifelse(!is.na(data$A1[cone.half.sphere])&!is.na(data$B1[cone.half.sphere]),
         1/12*pi*data$A1[cone.half.sphere]**2*(data$B1[cone.half.sphere]+.5*data$A1[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A1[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A1[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A1[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell1[double.cone]<-
  ifelse(!is.na(data$A1[double.cone])&!is.na(data$B1[double.cone]),
         1/12*pi*data$A1[double.cone]**2*data$B1[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A1[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A1[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell1[pyramid]<-
  ifelse(!is.na(data$A1[pyramid])&!is.na(data$B1[pyramid])&!is.na(data$C1[pyramid]),
         1/3*data$A1[pyramid]*data$B1[pyramid]*data$C1[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A1[pyramid])&!is.na(data$B1[pyramid])&!is.na(data$DimC[pyramid]),
         1/3*data$A1[pyramid]*data$B1[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A1[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C1[pyramid]),
         1/3*data$A1[pyramid]*data$DimB[pyramid]*data$C1[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A1[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
         1/3*data$A1[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
         NA))))

data$cell1[cuboid]<-
  ifelse(!is.na(data$A1[cuboid])&!is.na(data$B1[cuboid])&!is.na(data$C1[cuboid]),
         data$A1[cuboid]*data$B1[cuboid]*data$C1[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A1[cuboid])&!is.na(data$B1[cuboid])&!is.na(data$DimC[cuboid]),
                data$A1[cuboid]*data$B1[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A1[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C1[cuboid]),
                       data$A1[cuboid]*data$DimB[cuboid]*data$C1[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A1[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A1[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell1[lanceol.cyl]<-
  ifelse(!is.na(data$A1[lanceol.cyl])&!is.na(data$B1[lanceol.cyl])&!is.na(data$C1[lanceol.cyl]),
         2/pi*data$A1[lanceol.cyl]*data$B1[lanceol.cyl]*data$C1[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A1[lanceol.cyl])&!is.na(data$B1[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A1[lanceol.cyl]*data$B1[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A1[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C1[lanceol.cyl]),
                       2/pi*data$A1[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C1[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A1[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A1[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell1[cuboid1]<-
  ifelse(!is.na(data$A1[cuboid1])&!is.na(data$B1[cuboid1]),
         data$A1[cuboid1]*data$B1[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A1[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A1[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))



#A,B,C,Cell
#Cell1

data$cell2[spindle15]<-ifelse(!is.na(data$A2[spindle15])&!is.na(data$B2[spindle15]),
                              2/15*pi*data$A2[spindle15]**2*data$B2[spindle15]*data$corr.fac[spindle15],
                              ifelse(!is.na(data$A2[spindle15]),
                                     2/15*pi*data$A2[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                     NA))

data$cell2[spindle11]<-
  ifelse(!is.na(data$A2[spindle11])&!is.na(data$B2[spindle11])&!is.na(data$C2[spindle11]),
         2/15*pi*data$A2[spindle11]*data$B2[spindle11]*data$C2[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A2[spindle11])&!is.na(data$B2[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A2[spindle11]*data$B2[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A2[spindle11])&!is.na(data$B2[spindle11]),
                       2/15*pi*data$A2[spindle11]*data$B2[spindle11]*data$B2[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A2[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A2[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell2[rhomb]<-
  ifelse(!is.na(data$A2[rhomb])&!is.na(data$B2[rhomb])&!is.na(data$C2[rhomb]),
         1/2*data$A2[rhomb]*data$B2[rhomb]*data$C2[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A2[rhomb])&!is.na(data$B2[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A2[rhomb]*data$B2[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A2[rhomb])&!is.na(data$B2[rhomb]),
                       1/2*data$A2[rhomb]*data$B2[rhomb]*.75*data$B2[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A2[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A2[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell2[cylinder]<-ifelse(!is.na(data$A2[cylinder])&!is.na(data$B2[cylinder]),
                             1/4*pi*data$A2[cylinder]**2*data$B2[cylinder]*data$corr.fac[cylinder],
                             ifelse(!is.na(data$A2[cylinder]),
                                    1/4*pi*data$A2[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                    NA))

data$cell2[elliptic.cyl]<-
  ifelse(!is.na(data$A2[elliptic.cyl])&!is.na(data$B2[elliptic.cyl])&!is.na(data$C2[elliptic.cyl]),
         1/4*pi*data$A2[elliptic.cyl]*data$B2[elliptic.cyl]*data$C2[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A2[elliptic.cyl])&!is.na(data$B2[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A2[elliptic.cyl]*data$B2[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A2[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C2[elliptic.cyl]),
                       1/4*pi*data$A2[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C2[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A2[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A2[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell2[triang.prism]<-
  ifelse(!is.na(data$A2[triang.prism])&!is.na(data$B2[triang.prism])&!is.na(data$C2[triang.prism]),
         1/2*data$A2[triang.prism]*data$B2[triang.prism]*data$C2[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A2[triang.prism])&!is.na(data$B2[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A2[triang.prism]*data$B2[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A2[triang.prism])&!is.na(data$B2[triang.prism]),
                       1/2*data$A2[triang.prism]*data$B2[triang.prism]*.75*data$B2[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A2[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A2[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell2[sphere]<-
  ifelse(!is.na(data$A2[sphere]),
         1/6*pi*data$A2[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell2[spheroid]<-
  ifelse(!is.na(data$A2[spheroid])&!is.na(data$B2[spheroid]),
         1/6*pi*data$A2[spheroid]*data$B2[spheroid]*data$A2[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A2[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A2[spheroid]*data$DimB[spheroid]*data$A2[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell2[ellipsoid]<-
  ifelse(!is.na(data$A2[ellipsoid])&!is.na(data$B2[ellipsoid])&!is.na(data$C2[ellipsoid]),
         1/6*pi*data$A2[ellipsoid]*data$B2[ellipsoid]*data$C2[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A2[ellipsoid])&!is.na(data$B2[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A2[ellipsoid]*data$B2[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A2[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C2[ellipsoid]),
                       1/6*pi*data$A2[ellipsoid]*data$DimB[ellipsoid]*data$C2[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A2[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A2[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell2[cone]<-
  ifelse(!is.na(data$A2[cone])&!is.na(data$B2[cone]),
         1/12*pi*data$A2[cone]**2*(sqrt(data$B2[cone]**2-.25*data$A2[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A2[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A2[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A2[cone]**2))*data$corr.fac[cone],
                NA))

data$cell2[cone.half.sphere]<-
  ifelse(!is.na(data$A2[cone.half.sphere])&!is.na(data$B2[cone.half.sphere]),
         1/12*pi*data$A2[cone.half.sphere]**2*(data$B2[cone.half.sphere]+.5*data$A2[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A2[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A2[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A2[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell2[double.cone]<-
  ifelse(!is.na(data$A2[double.cone])&!is.na(data$B2[double.cone]),
         1/12*pi*data$A2[double.cone]**2*data$B2[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A2[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A2[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell2[pyramid]<-
  ifelse(!is.na(data$A2[pyramid])&!is.na(data$B2[pyramid])&!is.na(data$C2[pyramid]),
         1/3*data$A2[pyramid]*data$B2[pyramid]*data$C2[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A2[pyramid])&!is.na(data$B2[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A2[pyramid]*data$B2[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A2[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C2[pyramid]),
                       1/3*data$A2[pyramid]*data$DimB[pyramid]*data$C2[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A2[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A2[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell2[cuboid]<-
  ifelse(!is.na(data$A2[cuboid])&!is.na(data$B2[cuboid])&!is.na(data$C2[cuboid]),
         data$A2[cuboid]*data$B2[cuboid]*data$C2[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A2[cuboid])&!is.na(data$B2[cuboid])&!is.na(data$DimC[cuboid]),
                data$A2[cuboid]*data$B2[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A2[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C2[cuboid]),
                       data$A2[cuboid]*data$DimB[cuboid]*data$C2[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A2[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A2[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell2[lanceol.cyl]<-
  ifelse(!is.na(data$A2[lanceol.cyl])&!is.na(data$B2[lanceol.cyl])&!is.na(data$C2[lanceol.cyl]),
         2/pi*data$A2[lanceol.cyl]*data$B2[lanceol.cyl]*data$C2[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A2[lanceol.cyl])&!is.na(data$B2[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A2[lanceol.cyl]*data$B2[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A2[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C2[lanceol.cyl]),
                       2/pi*data$A2[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C2[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A2[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A2[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell2[cuboid1]<-
  ifelse(!is.na(data$A2[cuboid1])&!is.na(data$B2[cuboid1]),
         data$A2[cuboid1]*data$B2[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A2[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A2[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell3[spindle15]<-ifelse(!is.na(data$A3[spindle15])&!is.na(data$B3[spindle15]),
                              2/15*pi*data$A3[spindle15]**2*data$B3[spindle15]*data$corr.fac[spindle15],
                              ifelse(!is.na(data$A3[spindle15]),
                                     2/15*pi*data$A3[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                     NA))

data$cell3[spindle11]<-
  ifelse(!is.na(data$A3[spindle11])&!is.na(data$B3[spindle11])&!is.na(data$C3[spindle11]),
         2/15*pi*data$A3[spindle11]*data$B3[spindle11]*data$C3[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A3[spindle11])&!is.na(data$B3[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A3[spindle11]*data$B3[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A3[spindle11])&!is.na(data$B3[spindle11]),
                       2/15*pi*data$A3[spindle11]*data$B3[spindle11]*data$B3[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A3[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A3[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell3[rhomb]<-
  ifelse(!is.na(data$A3[rhomb])&!is.na(data$B3[rhomb])&!is.na(data$C3[rhomb]),
         1/2*data$A3[rhomb]*data$B3[rhomb]*data$C3[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A3[rhomb])&!is.na(data$B3[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A3[rhomb]*data$B3[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A3[rhomb])&!is.na(data$B3[rhomb]),
                       1/2*data$A3[rhomb]*data$B3[rhomb]*.75*data$B3[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A3[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A3[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell3[cylinder]<-ifelse(!is.na(data$A3[cylinder])&!is.na(data$B3[cylinder]),
                             1/4*pi*data$A3[cylinder]**2*data$B3[cylinder]*data$corr.fac[cylinder],
                             ifelse(!is.na(data$A3[cylinder]),
                                    1/4*pi*data$A3[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                    NA))

data$cell3[elliptic.cyl]<-
  ifelse(!is.na(data$A3[elliptic.cyl])&!is.na(data$B3[elliptic.cyl])&!is.na(data$C3[elliptic.cyl]),
         1/4*pi*data$A3[elliptic.cyl]*data$B3[elliptic.cyl]*data$C3[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A3[elliptic.cyl])&!is.na(data$B3[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A3[elliptic.cyl]*data$B3[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A3[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C3[elliptic.cyl]),
                       1/4*pi*data$A3[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C3[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A3[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A3[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell3[triang.prism]<-
  ifelse(!is.na(data$A3[triang.prism])&!is.na(data$B3[triang.prism])&!is.na(data$C3[triang.prism]),
         1/2*data$A3[triang.prism]*data$B3[triang.prism]*data$C3[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A3[triang.prism])&!is.na(data$B3[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A3[triang.prism]*data$B3[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A3[triang.prism])&!is.na(data$B3[triang.prism]),
                       1/2*data$A3[triang.prism]*data$B3[triang.prism]*.75*data$B3[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A3[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A3[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell3[sphere]<-
  ifelse(!is.na(data$A3[sphere]),
         1/6*pi*data$A3[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell3[spheroid]<-
  ifelse(!is.na(data$A3[spheroid])&!is.na(data$B3[spheroid]),
         1/6*pi*data$A3[spheroid]*data$B3[spheroid]*data$A3[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A3[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A3[spheroid]*data$DimB[spheroid]*data$A3[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell3[ellipsoid]<-
  ifelse(!is.na(data$A3[ellipsoid])&!is.na(data$B3[ellipsoid])&!is.na(data$C3[ellipsoid]),
         1/6*pi*data$A3[ellipsoid]*data$B3[ellipsoid]*data$C3[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A3[ellipsoid])&!is.na(data$B3[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A3[ellipsoid]*data$B3[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A3[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C3[ellipsoid]),
                       1/6*pi*data$A3[ellipsoid]*data$DimB[ellipsoid]*data$C3[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A3[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A3[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell3[cone]<-
  ifelse(!is.na(data$A3[cone])&!is.na(data$B3[cone]),
         1/12*pi*data$A3[cone]**2*(sqrt(data$B3[cone]**2-.25*data$A3[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A3[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A3[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A3[cone]**2))*data$corr.fac[cone],
                NA))

data$cell3[cone.half.sphere]<-
  ifelse(!is.na(data$A3[cone.half.sphere])&!is.na(data$B3[cone.half.sphere]),
         1/12*pi*data$A3[cone.half.sphere]**2*(data$B3[cone.half.sphere]+.5*data$A3[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A3[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A3[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A3[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell3[double.cone]<-
  ifelse(!is.na(data$A3[double.cone])&!is.na(data$B3[double.cone]),
         1/12*pi*data$A3[double.cone]**2*data$B3[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A3[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A3[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell3[pyramid]<-
  ifelse(!is.na(data$A3[pyramid])&!is.na(data$B3[pyramid])&!is.na(data$C3[pyramid]),
         1/3*data$A3[pyramid]*data$B3[pyramid]*data$C3[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A3[pyramid])&!is.na(data$B3[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A3[pyramid]*data$B3[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A3[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C3[pyramid]),
                       1/3*data$A3[pyramid]*data$DimB[pyramid]*data$C3[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A3[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A3[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell3[cuboid]<-
  ifelse(!is.na(data$A3[cuboid])&!is.na(data$B3[cuboid])&!is.na(data$C3[cuboid]),
         data$A3[cuboid]*data$B3[cuboid]*data$C3[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A3[cuboid])&!is.na(data$B3[cuboid])&!is.na(data$DimC[cuboid]),
                data$A3[cuboid]*data$B3[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A3[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C3[cuboid]),
                       data$A3[cuboid]*data$DimB[cuboid]*data$C3[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A3[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A3[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell3[lanceol.cyl]<-
  ifelse(!is.na(data$A3[lanceol.cyl])&!is.na(data$B3[lanceol.cyl])&!is.na(data$C3[lanceol.cyl]),
         2/pi*data$A3[lanceol.cyl]*data$B3[lanceol.cyl]*data$C3[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A3[lanceol.cyl])&!is.na(data$B3[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A3[lanceol.cyl]*data$B3[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A3[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C3[lanceol.cyl]),
                       2/pi*data$A3[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C3[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A3[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A3[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell3[cuboid1]<-
  ifelse(!is.na(data$A3[cuboid1])&!is.na(data$B3[cuboid1]),
         data$A3[cuboid1]*data$B3[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A3[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A3[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))

#A,B,C,Cell
#Cell1

data$cell4[spindle15]<-ifelse(!is.na(data$A4[spindle15])&!is.na(data$B4[spindle15]),
                              2/15*pi*data$A4[spindle15]**2*data$B4[spindle15]*data$corr.fac[spindle15],
                              ifelse(!is.na(data$A4[spindle15]),
                                     2/15*pi*data$A4[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                     NA))

data$cell4[spindle11]<-
  ifelse(!is.na(data$A4[spindle11])&!is.na(data$B4[spindle11])&!is.na(data$C4[spindle11]),
         2/15*pi*data$A4[spindle11]*data$B4[spindle11]*data$C4[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A4[spindle11])&!is.na(data$B4[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A4[spindle11]*data$B4[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A4[spindle11])&!is.na(data$B4[spindle11]),
                       2/15*pi*data$A4[spindle11]*data$B4[spindle11]*data$B4[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A4[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A4[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell4[rhomb]<-
  ifelse(!is.na(data$A4[rhomb])&!is.na(data$B4[rhomb])&!is.na(data$C4[rhomb]),
         1/2*data$A4[rhomb]*data$B4[rhomb]*data$C4[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A4[rhomb])&!is.na(data$B4[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A4[rhomb]*data$B4[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A4[rhomb])&!is.na(data$B4[rhomb]),
                       1/2*data$A4[rhomb]*data$B4[rhomb]*.75*data$B4[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A4[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A4[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell4[cylinder]<-ifelse(!is.na(data$A4[cylinder])&!is.na(data$B4[cylinder]),
                             1/4*pi*data$A4[cylinder]**2*data$B4[cylinder]*data$corr.fac[cylinder],
                             ifelse(!is.na(data$A4[cylinder]),
                                    1/4*pi*data$A4[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                    NA))

data$cell4[elliptic.cyl]<-
  ifelse(!is.na(data$A4[elliptic.cyl])&!is.na(data$B4[elliptic.cyl])&!is.na(data$C4[elliptic.cyl]),
         1/4*pi*data$A4[elliptic.cyl]*data$B4[elliptic.cyl]*data$C4[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A4[elliptic.cyl])&!is.na(data$B4[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A4[elliptic.cyl]*data$B4[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A4[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C4[elliptic.cyl]),
                       1/4*pi*data$A4[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C4[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A4[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A4[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell4[triang.prism]<-
  ifelse(!is.na(data$A4[triang.prism])&!is.na(data$B4[triang.prism])&!is.na(data$C4[triang.prism]),
         1/2*data$A4[triang.prism]*data$B4[triang.prism]*data$C4[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A4[triang.prism])&!is.na(data$B4[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A4[triang.prism]*data$B4[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A4[triang.prism])&!is.na(data$B4[triang.prism]),
                       1/2*data$A4[triang.prism]*data$B4[triang.prism]*.75*data$B4[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A4[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A4[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell4[sphere]<-
  ifelse(!is.na(data$A4[sphere]),
         1/6*pi*data$A4[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell4[spheroid]<-
  ifelse(!is.na(data$A4[spheroid])&!is.na(data$B4[spheroid]),
         1/6*pi*data$A4[spheroid]*data$B4[spheroid]*data$A4[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A4[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A4[spheroid]*data$DimB[spheroid]*data$A4[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell4[ellipsoid]<-
  ifelse(!is.na(data$A4[ellipsoid])&!is.na(data$B4[ellipsoid])&!is.na(data$C4[ellipsoid]),
         1/6*pi*data$A4[ellipsoid]*data$B4[ellipsoid]*data$C4[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A4[ellipsoid])&!is.na(data$B4[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A4[ellipsoid]*data$B4[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A4[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C4[ellipsoid]),
                       1/6*pi*data$A4[ellipsoid]*data$DimB[ellipsoid]*data$C4[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A4[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A4[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell4[cone]<-
  ifelse(!is.na(data$A4[cone])&!is.na(data$B4[cone]),
         1/12*pi*data$A4[cone]**2*(sqrt(data$B4[cone]**2-.25*data$A4[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A4[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A4[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A4[cone]**2))*data$corr.fac[cone],
                NA))

data$cell4[cone.half.sphere]<-
  ifelse(!is.na(data$A4[cone.half.sphere])&!is.na(data$B4[cone.half.sphere]),
         1/12*pi*data$A4[cone.half.sphere]**2*(data$B4[cone.half.sphere]+.5*data$A4[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A4[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A4[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A4[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell4[double.cone]<-
  ifelse(!is.na(data$A4[double.cone])&!is.na(data$B4[double.cone]),
         1/12*pi*data$A4[double.cone]**2*data$B4[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A4[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A4[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell4[pyramid]<-
  ifelse(!is.na(data$A4[pyramid])&!is.na(data$B4[pyramid])&!is.na(data$C4[pyramid]),
         1/3*data$A4[pyramid]*data$B4[pyramid]*data$C4[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A4[pyramid])&!is.na(data$B4[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A4[pyramid]*data$B4[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A4[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C4[pyramid]),
                       1/3*data$A4[pyramid]*data$DimB[pyramid]*data$C4[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A4[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A4[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell4[cuboid]<-
  ifelse(!is.na(data$A4[cuboid])&!is.na(data$B4[cuboid])&!is.na(data$C4[cuboid]),
         data$A4[cuboid]*data$B4[cuboid]*data$C4[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A4[cuboid])&!is.na(data$B4[cuboid])&!is.na(data$DimC[cuboid]),
                data$A4[cuboid]*data$B4[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A4[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C4[cuboid]),
                       data$A4[cuboid]*data$DimB[cuboid]*data$C4[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A4[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A4[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell4[lanceol.cyl]<-
  ifelse(!is.na(data$A4[lanceol.cyl])&!is.na(data$B4[lanceol.cyl])&!is.na(data$C4[lanceol.cyl]),
         2/pi*data$A4[lanceol.cyl]*data$B4[lanceol.cyl]*data$C4[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A4[lanceol.cyl])&!is.na(data$B4[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A4[lanceol.cyl]*data$B4[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A4[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C4[lanceol.cyl]),
                       2/pi*data$A4[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C4[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A4[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A4[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell4[cuboid1]<-
  ifelse(!is.na(data$A4[cuboid1])&!is.na(data$B4[cuboid1]),
         data$A4[cuboid1]*data$B4[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A4[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A4[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell5[spindle15]<-ifelse(!is.na(data$A5[spindle15])&!is.na(data$B5[spindle15]),
                              2/15*pi*data$A5[spindle15]**2*data$B5[spindle15]*data$corr.fac[spindle15],
                              ifelse(!is.na(data$A5[spindle15]),
                                     2/15*pi*data$A5[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                     NA))

data$cell5[spindle11]<-
  ifelse(!is.na(data$A5[spindle11])&!is.na(data$B5[spindle11])&!is.na(data$C5[spindle11]),
         2/15*pi*data$A5[spindle11]*data$B5[spindle11]*data$C5[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A5[spindle11])&!is.na(data$B5[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A5[spindle11]*data$B5[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A5[spindle11])&!is.na(data$B5[spindle11]),
                       2/15*pi*data$A5[spindle11]*data$B5[spindle11]*data$B5[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A5[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A5[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell5[rhomb]<-
  ifelse(!is.na(data$A5[rhomb])&!is.na(data$B5[rhomb])&!is.na(data$C5[rhomb]),
         1/2*data$A5[rhomb]*data$B5[rhomb]*data$C5[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A5[rhomb])&!is.na(data$B5[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A5[rhomb]*data$B5[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A5[rhomb])&!is.na(data$B5[rhomb]),
                       1/2*data$A5[rhomb]*data$B5[rhomb]*.75*data$B5[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A5[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A5[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell5[cylinder]<-ifelse(!is.na(data$A5[cylinder])&!is.na(data$B5[cylinder]),
                             1/4*pi*data$A5[cylinder]**2*data$B5[cylinder]*data$corr.fac[cylinder],
                             ifelse(!is.na(data$A5[cylinder]),
                                    1/4*pi*data$A5[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                    NA))

data$cell5[elliptic.cyl]<-
  ifelse(!is.na(data$A5[elliptic.cyl])&!is.na(data$B5[elliptic.cyl])&!is.na(data$C5[elliptic.cyl]),
         1/4*pi*data$A5[elliptic.cyl]*data$B5[elliptic.cyl]*data$C5[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A5[elliptic.cyl])&!is.na(data$B5[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A5[elliptic.cyl]*data$B5[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A5[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C5[elliptic.cyl]),
                       1/4*pi*data$A5[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C5[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A5[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A5[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell5[triang.prism]<-
  ifelse(!is.na(data$A5[triang.prism])&!is.na(data$B5[triang.prism])&!is.na(data$C5[triang.prism]),
         1/2*data$A5[triang.prism]*data$B5[triang.prism]*data$C5[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A5[triang.prism])&!is.na(data$B5[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A5[triang.prism]*data$B5[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A5[triang.prism])&!is.na(data$B5[triang.prism]),
                       1/2*data$A5[triang.prism]*data$B5[triang.prism]*.75*data$B5[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A5[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A5[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell5[sphere]<-
  ifelse(!is.na(data$A5[sphere]),
         1/6*pi*data$A5[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell5[spheroid]<-
  ifelse(!is.na(data$A5[spheroid])&!is.na(data$B5[spheroid]),
         1/6*pi*data$A5[spheroid]*data$B5[spheroid]*data$A5[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A5[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A5[spheroid]*data$DimB[spheroid]*data$A5[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell5[ellipsoid]<-
  ifelse(!is.na(data$A5[ellipsoid])&!is.na(data$B5[ellipsoid])&!is.na(data$C5[ellipsoid]),
         1/6*pi*data$A5[ellipsoid]*data$B5[ellipsoid]*data$C5[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A5[ellipsoid])&!is.na(data$B5[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A5[ellipsoid]*data$B5[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A5[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C5[ellipsoid]),
                       1/6*pi*data$A5[ellipsoid]*data$DimB[ellipsoid]*data$C5[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A5[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A5[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell5[cone]<-
  ifelse(!is.na(data$A5[cone])&!is.na(data$B5[cone]),
         1/12*pi*data$A5[cone]**2*(sqrt(data$B5[cone]**2-.25*data$A5[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A5[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A5[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A5[cone]**2))*data$corr.fac[cone],
                NA))

data$cell5[cone.half.sphere]<-
  ifelse(!is.na(data$A5[cone.half.sphere])&!is.na(data$B5[cone.half.sphere]),
         1/12*pi*data$A5[cone.half.sphere]**2*(data$B5[cone.half.sphere]+.5*data$A5[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A5[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A5[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A5[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell5[double.cone]<-
  ifelse(!is.na(data$A5[double.cone])&!is.na(data$B5[double.cone]),
         1/12*pi*data$A5[double.cone]**2*data$B5[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A5[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A5[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell5[pyramid]<-
  ifelse(!is.na(data$A5[pyramid])&!is.na(data$B5[pyramid])&!is.na(data$C5[pyramid]),
         1/3*data$A5[pyramid]*data$B5[pyramid]*data$C5[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A5[pyramid])&!is.na(data$B5[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A5[pyramid]*data$B5[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A5[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C5[pyramid]),
                       1/3*data$A5[pyramid]*data$DimB[pyramid]*data$C5[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A5[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A5[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell5[cuboid]<-
  ifelse(!is.na(data$A5[cuboid])&!is.na(data$B5[cuboid])&!is.na(data$C5[cuboid]),
         data$A5[cuboid]*data$B5[cuboid]*data$C5[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A5[cuboid])&!is.na(data$B5[cuboid])&!is.na(data$DimC[cuboid]),
                data$A5[cuboid]*data$B5[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A5[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C5[cuboid]),
                       data$A5[cuboid]*data$DimB[cuboid]*data$C5[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A5[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A5[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell5[lanceol.cyl]<-
  ifelse(!is.na(data$A5[lanceol.cyl])&!is.na(data$B5[lanceol.cyl])&!is.na(data$C5[lanceol.cyl]),
         2/pi*data$A5[lanceol.cyl]*data$B5[lanceol.cyl]*data$C5[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A5[lanceol.cyl])&!is.na(data$B5[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A5[lanceol.cyl]*data$B5[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A5[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C5[lanceol.cyl]),
                       2/pi*data$A5[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C5[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A5[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A5[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell5[cuboid1]<-
  ifelse(!is.na(data$A5[cuboid1])&!is.na(data$B5[cuboid1]),
         data$A5[cuboid1]*data$B5[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A5[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A5[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))

#A,B,C,Cell
#Cell1

data$cell6[spindle15]<-ifelse(!is.na(data$A6[spindle15])&!is.na(data$B6[spindle15]),
                              2/15*pi*data$A6[spindle15]**2*data$B6[spindle15]*data$corr.fac[spindle15],
                              ifelse(!is.na(data$A6[spindle15]),
                                     2/15*pi*data$A6[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                     NA))

data$cell6[spindle11]<-
  ifelse(!is.na(data$A6[spindle11])&!is.na(data$B6[spindle11])&!is.na(data$C6[spindle11]),
         2/15*pi*data$A6[spindle11]*data$B6[spindle11]*data$C6[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A6[spindle11])&!is.na(data$B6[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A6[spindle11]*data$B6[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A6[spindle11])&!is.na(data$B6[spindle11]),
                       2/15*pi*data$A6[spindle11]*data$B6[spindle11]*data$B6[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A6[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A6[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell6[rhomb]<-
  ifelse(!is.na(data$A6[rhomb])&!is.na(data$B6[rhomb])&!is.na(data$C6[rhomb]),
         1/2*data$A6[rhomb]*data$B6[rhomb]*data$C6[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A6[rhomb])&!is.na(data$B6[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A6[rhomb]*data$B6[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A6[rhomb])&!is.na(data$B6[rhomb]),
                       1/2*data$A6[rhomb]*data$B6[rhomb]*.75*data$B6[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A6[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A6[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell6[cylinder]<-ifelse(!is.na(data$A6[cylinder])&!is.na(data$B6[cylinder]),
                             1/4*pi*data$A6[cylinder]**2*data$B6[cylinder]*data$corr.fac[cylinder],
                             ifelse(!is.na(data$A6[cylinder]),
                                    1/4*pi*data$A6[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                    NA))

data$cell6[elliptic.cyl]<-
  ifelse(!is.na(data$A6[elliptic.cyl])&!is.na(data$B6[elliptic.cyl])&!is.na(data$C6[elliptic.cyl]),
         1/4*pi*data$A6[elliptic.cyl]*data$B6[elliptic.cyl]*data$C6[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A6[elliptic.cyl])&!is.na(data$B6[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A6[elliptic.cyl]*data$B6[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A6[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C6[elliptic.cyl]),
                       1/4*pi*data$A6[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C6[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A6[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A6[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell6[triang.prism]<-
  ifelse(!is.na(data$A6[triang.prism])&!is.na(data$B6[triang.prism])&!is.na(data$C6[triang.prism]),
         1/2*data$A6[triang.prism]*data$B6[triang.prism]*data$C6[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A6[triang.prism])&!is.na(data$B6[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A6[triang.prism]*data$B6[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A6[triang.prism])&!is.na(data$B6[triang.prism]),
                       1/2*data$A6[triang.prism]*data$B6[triang.prism]*.75*data$B6[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A6[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A6[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell6[sphere]<-
  ifelse(!is.na(data$A6[sphere]),
         1/6*pi*data$A6[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell6[spheroid]<-
  ifelse(!is.na(data$A6[spheroid])&!is.na(data$B6[spheroid]),
         1/6*pi*data$A6[spheroid]*data$B6[spheroid]*data$A6[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A6[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A6[spheroid]*data$DimB[spheroid]*data$A6[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell6[ellipsoid]<-
  ifelse(!is.na(data$A6[ellipsoid])&!is.na(data$B6[ellipsoid])&!is.na(data$C6[ellipsoid]),
         1/6*pi*data$A6[ellipsoid]*data$B6[ellipsoid]*data$C6[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A6[ellipsoid])&!is.na(data$B6[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A6[ellipsoid]*data$B6[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A6[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C6[ellipsoid]),
                       1/6*pi*data$A6[ellipsoid]*data$DimB[ellipsoid]*data$C6[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A6[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A6[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell6[cone]<-
  ifelse(!is.na(data$A6[cone])&!is.na(data$B6[cone]),
         1/12*pi*data$A6[cone]**2*(sqrt(data$B6[cone]**2-.25*data$A6[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A6[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A6[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A6[cone]**2))*data$corr.fac[cone],
                NA))

data$cell6[cone.half.sphere]<-
  ifelse(!is.na(data$A6[cone.half.sphere])&!is.na(data$B6[cone.half.sphere]),
         1/12*pi*data$A6[cone.half.sphere]**2*(data$B6[cone.half.sphere]+.5*data$A6[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A6[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A6[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A6[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell6[double.cone]<-
  ifelse(!is.na(data$A6[double.cone])&!is.na(data$B6[double.cone]),
         1/12*pi*data$A6[double.cone]**2*data$B6[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A6[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A6[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell6[pyramid]<-
  ifelse(!is.na(data$A6[pyramid])&!is.na(data$B6[pyramid])&!is.na(data$C6[pyramid]),
         1/3*data$A6[pyramid]*data$B6[pyramid]*data$C6[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A6[pyramid])&!is.na(data$B6[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A6[pyramid]*data$B6[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A6[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C6[pyramid]),
                       1/3*data$A6[pyramid]*data$DimB[pyramid]*data$C6[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A6[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A6[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell6[cuboid]<-
  ifelse(!is.na(data$A6[cuboid])&!is.na(data$B6[cuboid])&!is.na(data$C6[cuboid]),
         data$A6[cuboid]*data$B6[cuboid]*data$C6[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A6[cuboid])&!is.na(data$B6[cuboid])&!is.na(data$DimC[cuboid]),
                data$A6[cuboid]*data$B6[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A6[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C6[cuboid]),
                       data$A6[cuboid]*data$DimB[cuboid]*data$C6[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A6[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A6[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell6[lanceol.cyl]<-
  ifelse(!is.na(data$A6[lanceol.cyl])&!is.na(data$B6[lanceol.cyl])&!is.na(data$C6[lanceol.cyl]),
         2/pi*data$A6[lanceol.cyl]*data$B6[lanceol.cyl]*data$C6[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A6[lanceol.cyl])&!is.na(data$B6[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A6[lanceol.cyl]*data$B6[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A6[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C6[lanceol.cyl]),
                       2/pi*data$A6[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C6[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A6[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A6[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell6[cuboid1]<-
  ifelse(!is.na(data$A6[cuboid1])&!is.na(data$B6[cuboid1]),
         data$A6[cuboid1]*data$B6[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A6[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A6[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))

#A,B,C,Cell
#Cell1

data$cell7[spindle15]<-ifelse(!is.na(data$A7[spindle15])&!is.na(data$B7[spindle15]),
                              2/15*pi*data$A7[spindle15]**2*data$B7[spindle15]*data$corr.fac[spindle15],
                              ifelse(!is.na(data$A7[spindle15]),
                                     2/15*pi*data$A7[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                     NA))

data$cell7[spindle11]<-
  ifelse(!is.na(data$A7[spindle11])&!is.na(data$B7[spindle11])&!is.na(data$C7[spindle11]),
         2/15*pi*data$A7[spindle11]*data$B7[spindle11]*data$C7[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A7[spindle11])&!is.na(data$B7[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A7[spindle11]*data$B7[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A7[spindle11])&!is.na(data$B7[spindle11]),
                       2/15*pi*data$A7[spindle11]*data$B7[spindle11]*data$B7[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A7[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A7[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell7[rhomb]<-
  ifelse(!is.na(data$A7[rhomb])&!is.na(data$B7[rhomb])&!is.na(data$C7[rhomb]),
         1/2*data$A7[rhomb]*data$B7[rhomb]*data$C7[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A7[rhomb])&!is.na(data$B7[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A7[rhomb]*data$B7[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A7[rhomb])&!is.na(data$B7[rhomb]),
                       1/2*data$A7[rhomb]*data$B7[rhomb]*.75*data$B7[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A7[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A7[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell7[cylinder]<-ifelse(!is.na(data$A7[cylinder])&!is.na(data$B7[cylinder]),
                             1/4*pi*data$A7[cylinder]**2*data$B7[cylinder]*data$corr.fac[cylinder],
                             ifelse(!is.na(data$A7[cylinder]),
                                    1/4*pi*data$A7[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                    NA))

data$cell7[elliptic.cyl]<-
  ifelse(!is.na(data$A7[elliptic.cyl])&!is.na(data$B7[elliptic.cyl])&!is.na(data$C7[elliptic.cyl]),
         1/4*pi*data$A7[elliptic.cyl]*data$B7[elliptic.cyl]*data$C7[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A7[elliptic.cyl])&!is.na(data$B7[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A7[elliptic.cyl]*data$B7[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A7[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C7[elliptic.cyl]),
                       1/4*pi*data$A7[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C7[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A7[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A7[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell7[triang.prism]<-
  ifelse(!is.na(data$A7[triang.prism])&!is.na(data$B7[triang.prism])&!is.na(data$C7[triang.prism]),
         1/2*data$A7[triang.prism]*data$B7[triang.prism]*data$C7[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A7[triang.prism])&!is.na(data$B7[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A7[triang.prism]*data$B7[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A7[triang.prism])&!is.na(data$B7[triang.prism]),
                       1/2*data$A7[triang.prism]*data$B7[triang.prism]*.75*data$B7[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A7[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A7[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell7[sphere]<-
  ifelse(!is.na(data$A7[sphere]),
         1/6*pi*data$A7[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell7[spheroid]<-
  ifelse(!is.na(data$A7[spheroid])&!is.na(data$B7[spheroid]),
         1/6*pi*data$A7[spheroid]*data$B7[spheroid]*data$A7[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A7[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A7[spheroid]*data$DimB[spheroid]*data$A7[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell7[ellipsoid]<-
  ifelse(!is.na(data$A7[ellipsoid])&!is.na(data$B7[ellipsoid])&!is.na(data$C7[ellipsoid]),
         1/6*pi*data$A7[ellipsoid]*data$B7[ellipsoid]*data$C7[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A7[ellipsoid])&!is.na(data$B7[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A7[ellipsoid]*data$B7[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A7[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C7[ellipsoid]),
                       1/6*pi*data$A7[ellipsoid]*data$DimB[ellipsoid]*data$C7[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A7[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A7[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell7[cone]<-
  ifelse(!is.na(data$A7[cone])&!is.na(data$B7[cone]),
         1/12*pi*data$A7[cone]**2*(sqrt(data$B7[cone]**2-.25*data$A7[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A7[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A7[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A7[cone]**2))*data$corr.fac[cone],
                NA))

data$cell7[cone.half.sphere]<-
  ifelse(!is.na(data$A7[cone.half.sphere])&!is.na(data$B7[cone.half.sphere]),
         1/12*pi*data$A7[cone.half.sphere]**2*(data$B7[cone.half.sphere]+.5*data$A7[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A7[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A7[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A7[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell7[double.cone]<-
  ifelse(!is.na(data$A7[double.cone])&!is.na(data$B7[double.cone]),
         1/12*pi*data$A7[double.cone]**2*data$B7[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A7[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A7[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell7[pyramid]<-
  ifelse(!is.na(data$A7[pyramid])&!is.na(data$B7[pyramid])&!is.na(data$C7[pyramid]),
         1/3*data$A7[pyramid]*data$B7[pyramid]*data$C7[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A7[pyramid])&!is.na(data$B7[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A7[pyramid]*data$B7[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A7[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C7[pyramid]),
                       1/3*data$A7[pyramid]*data$DimB[pyramid]*data$C7[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A7[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A7[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell7[cuboid]<-
  ifelse(!is.na(data$A7[cuboid])&!is.na(data$B7[cuboid])&!is.na(data$C7[cuboid]),
         data$A7[cuboid]*data$B7[cuboid]*data$C7[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A7[cuboid])&!is.na(data$B7[cuboid])&!is.na(data$DimC[cuboid]),
                data$A7[cuboid]*data$B7[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A7[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C7[cuboid]),
                       data$A7[cuboid]*data$DimB[cuboid]*data$C7[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A7[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A7[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell7[lanceol.cyl]<-
  ifelse(!is.na(data$A7[lanceol.cyl])&!is.na(data$B7[lanceol.cyl])&!is.na(data$C7[lanceol.cyl]),
         2/pi*data$A7[lanceol.cyl]*data$B7[lanceol.cyl]*data$C7[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A7[lanceol.cyl])&!is.na(data$B7[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A7[lanceol.cyl]*data$B7[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A7[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C7[lanceol.cyl]),
                       2/pi*data$A7[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C7[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A7[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A7[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell7[cuboid1]<-
  ifelse(!is.na(data$A7[cuboid1])&!is.na(data$B7[cuboid1]),
         data$A7[cuboid1]*data$B7[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A7[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A7[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))

#A,B,C,Cell
#Cell1

data$cell8[spindle15]<-ifelse(!is.na(data$A8[spindle15])&!is.na(data$B8[spindle15]),
                              2/15*pi*data$A8[spindle15]**2*data$B8[spindle15]*data$corr.fac[spindle15],
                              ifelse(!is.na(data$A8[spindle15]),
                                     2/15*pi*data$A8[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                     NA))

data$cell8[spindle11]<-
  ifelse(!is.na(data$A8[spindle11])&!is.na(data$B8[spindle11])&!is.na(data$C8[spindle11]),
         2/15*pi*data$A8[spindle11]*data$B8[spindle11]*data$C8[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A8[spindle11])&!is.na(data$B8[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A8[spindle11]*data$B8[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A8[spindle11])&!is.na(data$B8[spindle11]),
                       2/15*pi*data$A8[spindle11]*data$B8[spindle11]*data$B8[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A8[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A8[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell8[rhomb]<-
  ifelse(!is.na(data$A8[rhomb])&!is.na(data$B8[rhomb])&!is.na(data$C8[rhomb]),
         1/2*data$A8[rhomb]*data$B8[rhomb]*data$C8[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A8[rhomb])&!is.na(data$B8[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A8[rhomb]*data$B8[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A8[rhomb])&!is.na(data$B8[rhomb]),
                       1/2*data$A8[rhomb]*data$B8[rhomb]*.75*data$B8[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A8[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A8[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell8[cylinder]<-ifelse(!is.na(data$A8[cylinder])&!is.na(data$B8[cylinder]),
                             1/4*pi*data$A8[cylinder]**2*data$B8[cylinder]*data$corr.fac[cylinder],
                             ifelse(!is.na(data$A8[cylinder]),
                                    1/4*pi*data$A8[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                    NA))

data$cell8[elliptic.cyl]<-
  ifelse(!is.na(data$A8[elliptic.cyl])&!is.na(data$B8[elliptic.cyl])&!is.na(data$C8[elliptic.cyl]),
         1/4*pi*data$A8[elliptic.cyl]*data$B8[elliptic.cyl]*data$C8[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A8[elliptic.cyl])&!is.na(data$B8[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A8[elliptic.cyl]*data$B8[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A8[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C8[elliptic.cyl]),
                       1/4*pi*data$A8[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C8[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A8[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A8[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell8[triang.prism]<-
  ifelse(!is.na(data$A8[triang.prism])&!is.na(data$B8[triang.prism])&!is.na(data$C8[triang.prism]),
         1/2*data$A8[triang.prism]*data$B8[triang.prism]*data$C8[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A8[triang.prism])&!is.na(data$B8[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A8[triang.prism]*data$B8[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A8[triang.prism])&!is.na(data$B8[triang.prism]),
                       1/2*data$A8[triang.prism]*data$B8[triang.prism]*.75*data$B8[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A8[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A8[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell8[sphere]<-
  ifelse(!is.na(data$A8[sphere]),
         1/6*pi*data$A8[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell8[spheroid]<-
  ifelse(!is.na(data$A8[spheroid])&!is.na(data$B8[spheroid]),
         1/6*pi*data$A8[spheroid]*data$B8[spheroid]*data$A8[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A8[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A8[spheroid]*data$DimB[spheroid]*data$A8[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell8[ellipsoid]<-
  ifelse(!is.na(data$A8[ellipsoid])&!is.na(data$B8[ellipsoid])&!is.na(data$C8[ellipsoid]),
         1/6*pi*data$A8[ellipsoid]*data$B8[ellipsoid]*data$C8[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A8[ellipsoid])&!is.na(data$B8[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A8[ellipsoid]*data$B8[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A8[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C8[ellipsoid]),
                       1/6*pi*data$A8[ellipsoid]*data$DimB[ellipsoid]*data$C8[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A8[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A8[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell8[cone]<-
  ifelse(!is.na(data$A8[cone])&!is.na(data$B8[cone]),
         1/12*pi*data$A8[cone]**2*(sqrt(data$B8[cone]**2-.25*data$A8[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A8[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A8[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A8[cone]**2))*data$corr.fac[cone],
                NA))

data$cell8[cone.half.sphere]<-
  ifelse(!is.na(data$A8[cone.half.sphere])&!is.na(data$B8[cone.half.sphere]),
         1/12*pi*data$A8[cone.half.sphere]**2*(data$B8[cone.half.sphere]+.5*data$A8[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A8[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A8[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A8[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell8[double.cone]<-
  ifelse(!is.na(data$A8[double.cone])&!is.na(data$B8[double.cone]),
         1/12*pi*data$A8[double.cone]**2*data$B8[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A8[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A8[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell8[pyramid]<-
  ifelse(!is.na(data$A8[pyramid])&!is.na(data$B8[pyramid])&!is.na(data$C8[pyramid]),
         1/3*data$A8[pyramid]*data$B8[pyramid]*data$C8[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A8[pyramid])&!is.na(data$B8[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A8[pyramid]*data$B8[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A8[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C8[pyramid]),
                       1/3*data$A8[pyramid]*data$DimB[pyramid]*data$C8[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A8[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A8[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell8[cuboid]<-
  ifelse(!is.na(data$A8[cuboid])&!is.na(data$B8[cuboid])&!is.na(data$C8[cuboid]),
         data$A8[cuboid]*data$B8[cuboid]*data$C8[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A8[cuboid])&!is.na(data$B8[cuboid])&!is.na(data$DimC[cuboid]),
                data$A8[cuboid]*data$B8[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A8[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C8[cuboid]),
                       data$A8[cuboid]*data$DimB[cuboid]*data$C8[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A8[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A8[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell8[lanceol.cyl]<-
  ifelse(!is.na(data$A8[lanceol.cyl])&!is.na(data$B8[lanceol.cyl])&!is.na(data$C8[lanceol.cyl]),
         2/pi*data$A8[lanceol.cyl]*data$B8[lanceol.cyl]*data$C8[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A8[lanceol.cyl])&!is.na(data$B8[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A8[lanceol.cyl]*data$B8[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A8[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C8[lanceol.cyl]),
                       2/pi*data$A8[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C8[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A8[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A8[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell8[cuboid1]<-
  ifelse(!is.na(data$A8[cuboid1])&!is.na(data$B8[cuboid1]),
         data$A8[cuboid1]*data$B8[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A8[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A8[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))

#A,B,C,Cell
#Cell1

data$cell9[spindle15]<-ifelse(!is.na(data$A9[spindle15])&!is.na(data$B9[spindle15]),
                              2/15*pi*data$A9[spindle15]**2*data$B9[spindle15]*data$corr.fac[spindle15],
                              ifelse(!is.na(data$A9[spindle15]),
                                     2/15*pi*data$A9[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                     NA))

data$cell9[spindle11]<-
  ifelse(!is.na(data$A9[spindle11])&!is.na(data$B9[spindle11])&!is.na(data$C9[spindle11]),
         2/15*pi*data$A9[spindle11]*data$B9[spindle11]*data$C9[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A9[spindle11])&!is.na(data$B9[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A9[spindle11]*data$B9[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A9[spindle11])&!is.na(data$B9[spindle11]),
                       2/15*pi*data$A9[spindle11]*data$B9[spindle11]*data$B9[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A9[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A9[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell9[rhomb]<-
  ifelse(!is.na(data$A9[rhomb])&!is.na(data$B9[rhomb])&!is.na(data$C9[rhomb]),
         1/2*data$A9[rhomb]*data$B9[rhomb]*data$C9[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A9[rhomb])&!is.na(data$B9[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A9[rhomb]*data$B9[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A9[rhomb])&!is.na(data$B9[rhomb]),
                       1/2*data$A9[rhomb]*data$B9[rhomb]*.75*data$B9[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A9[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A9[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell9[cylinder]<-ifelse(!is.na(data$A9[cylinder])&!is.na(data$B9[cylinder]),
                             1/4*pi*data$A9[cylinder]**2*data$B9[cylinder]*data$corr.fac[cylinder],
                             ifelse(!is.na(data$A9[cylinder]),
                                    1/4*pi*data$A9[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                    NA))

data$cell9[elliptic.cyl]<-
  ifelse(!is.na(data$A9[elliptic.cyl])&!is.na(data$B9[elliptic.cyl])&!is.na(data$C9[elliptic.cyl]),
         1/4*pi*data$A9[elliptic.cyl]*data$B9[elliptic.cyl]*data$C9[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A9[elliptic.cyl])&!is.na(data$B9[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A9[elliptic.cyl]*data$B9[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A9[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C9[elliptic.cyl]),
                       1/4*pi*data$A9[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C9[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A9[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A9[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell9[triang.prism]<-
  ifelse(!is.na(data$A9[triang.prism])&!is.na(data$B9[triang.prism])&!is.na(data$C9[triang.prism]),
         1/2*data$A9[triang.prism]*data$B9[triang.prism]*data$C9[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A9[triang.prism])&!is.na(data$B9[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A9[triang.prism]*data$B9[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A9[triang.prism])&!is.na(data$B9[triang.prism]),
                       1/2*data$A9[triang.prism]*data$B9[triang.prism]*.75*data$B9[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A9[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A9[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell9[sphere]<-
  ifelse(!is.na(data$A9[sphere]),
         1/6*pi*data$A9[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell9[spheroid]<-
  ifelse(!is.na(data$A9[spheroid])&!is.na(data$B9[spheroid]),
         1/6*pi*data$A9[spheroid]*data$B9[spheroid]*data$A9[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A9[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A9[spheroid]*data$DimB[spheroid]*data$A9[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell9[ellipsoid]<-
  ifelse(!is.na(data$A9[ellipsoid])&!is.na(data$B9[ellipsoid])&!is.na(data$C9[ellipsoid]),
         1/6*pi*data$A9[ellipsoid]*data$B9[ellipsoid]*data$C9[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A9[ellipsoid])&!is.na(data$B9[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A9[ellipsoid]*data$B9[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A9[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C9[ellipsoid]),
                       1/6*pi*data$A9[ellipsoid]*data$DimB[ellipsoid]*data$C9[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A9[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A9[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell9[cone]<-
  ifelse(!is.na(data$A9[cone])&!is.na(data$B9[cone]),
         1/12*pi*data$A9[cone]**2*(sqrt(data$B9[cone]**2-.25*data$A9[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A9[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A9[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A9[cone]**2))*data$corr.fac[cone],
                NA))

data$cell9[cone.half.sphere]<-
  ifelse(!is.na(data$A9[cone.half.sphere])&!is.na(data$B9[cone.half.sphere]),
         1/12*pi*data$A9[cone.half.sphere]**2*(data$B9[cone.half.sphere]+.5*data$A9[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A9[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A9[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A9[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell9[double.cone]<-
  ifelse(!is.na(data$A9[double.cone])&!is.na(data$B9[double.cone]),
         1/12*pi*data$A9[double.cone]**2*data$B9[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A9[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A9[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell9[pyramid]<-
  ifelse(!is.na(data$A9[pyramid])&!is.na(data$B9[pyramid])&!is.na(data$C9[pyramid]),
         1/3*data$A9[pyramid]*data$B9[pyramid]*data$C9[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A9[pyramid])&!is.na(data$B9[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A9[pyramid]*data$B9[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A9[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C9[pyramid]),
                       1/3*data$A9[pyramid]*data$DimB[pyramid]*data$C9[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A9[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A9[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell9[cuboid]<-
  ifelse(!is.na(data$A9[cuboid])&!is.na(data$B9[cuboid])&!is.na(data$C9[cuboid]),
         data$A9[cuboid]*data$B9[cuboid]*data$C9[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A9[cuboid])&!is.na(data$B9[cuboid])&!is.na(data$DimC[cuboid]),
                data$A9[cuboid]*data$B9[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A9[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C9[cuboid]),
                       data$A9[cuboid]*data$DimB[cuboid]*data$C9[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A9[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A9[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell9[lanceol.cyl]<-
  ifelse(!is.na(data$A9[lanceol.cyl])&!is.na(data$B9[lanceol.cyl])&!is.na(data$C9[lanceol.cyl]),
         2/pi*data$A9[lanceol.cyl]*data$B9[lanceol.cyl]*data$C9[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A9[lanceol.cyl])&!is.na(data$B9[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A9[lanceol.cyl]*data$B9[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A9[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C9[lanceol.cyl]),
                       2/pi*data$A9[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C9[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A9[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A9[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell9[cuboid1]<-
  ifelse(!is.na(data$A9[cuboid1])&!is.na(data$B9[cuboid1]),
         data$A9[cuboid1]*data$B9[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A9[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A9[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))

#A,B,C,Cell
#Cell1

data$cell10[spindle15]<-ifelse(!is.na(data$A10[spindle15])&!is.na(data$B10[spindle15]),
                               2/15*pi*data$A10[spindle15]**2*data$B10[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A10[spindle15]),
                                      2/15*pi*data$A10[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell10[spindle11]<-
  ifelse(!is.na(data$A10[spindle11])&!is.na(data$B10[spindle11])&!is.na(data$C10[spindle11]),
         2/15*pi*data$A10[spindle11]*data$B10[spindle11]*data$C10[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A10[spindle11])&!is.na(data$B10[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A10[spindle11]*data$B10[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A10[spindle11])&!is.na(data$B10[spindle11]),
                       2/15*pi*data$A10[spindle11]*data$B10[spindle11]*data$B10[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A10[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A10[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell10[rhomb]<-
  ifelse(!is.na(data$A10[rhomb])&!is.na(data$B10[rhomb])&!is.na(data$C10[rhomb]),
         1/2*data$A10[rhomb]*data$B10[rhomb]*data$C10[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A10[rhomb])&!is.na(data$B10[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A10[rhomb]*data$B10[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A10[rhomb])&!is.na(data$B10[rhomb]),
                       1/2*data$A10[rhomb]*data$B10[rhomb]*.75*data$B10[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A10[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A10[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell10[cylinder]<-ifelse(!is.na(data$A10[cylinder])&!is.na(data$B10[cylinder]),
                              1/4*pi*data$A10[cylinder]**2*data$B10[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A10[cylinder]),
                                     1/4*pi*data$A10[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell10[elliptic.cyl]<-
  ifelse(!is.na(data$A10[elliptic.cyl])&!is.na(data$B10[elliptic.cyl])&!is.na(data$C10[elliptic.cyl]),
         1/4*pi*data$A10[elliptic.cyl]*data$B10[elliptic.cyl]*data$C10[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A10[elliptic.cyl])&!is.na(data$B10[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A10[elliptic.cyl]*data$B10[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A10[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C10[elliptic.cyl]),
                       1/4*pi*data$A10[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C10[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A10[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A10[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell10[triang.prism]<-
  ifelse(!is.na(data$A10[triang.prism])&!is.na(data$B10[triang.prism])&!is.na(data$C10[triang.prism]),
         1/2*data$A10[triang.prism]*data$B10[triang.prism]*data$C10[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A10[triang.prism])&!is.na(data$B10[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A10[triang.prism]*data$B10[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A10[triang.prism])&!is.na(data$B10[triang.prism]),
                       1/2*data$A10[triang.prism]*data$B10[triang.prism]*.75*data$B10[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A10[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A10[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell10[sphere]<-
  ifelse(!is.na(data$A10[sphere]),
         1/6*pi*data$A10[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell10[spheroid]<-
  ifelse(!is.na(data$A10[spheroid])&!is.na(data$B10[spheroid]),
         1/6*pi*data$A10[spheroid]*data$B10[spheroid]*data$A10[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A10[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A10[spheroid]*data$DimB[spheroid]*data$A10[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell10[ellipsoid]<-
  ifelse(!is.na(data$A10[ellipsoid])&!is.na(data$B10[ellipsoid])&!is.na(data$C10[ellipsoid]),
         1/6*pi*data$A10[ellipsoid]*data$B10[ellipsoid]*data$C10[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A10[ellipsoid])&!is.na(data$B10[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A10[ellipsoid]*data$B10[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A10[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C10[ellipsoid]),
                       1/6*pi*data$A10[ellipsoid]*data$DimB[ellipsoid]*data$C10[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A10[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A10[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell10[cone]<-
  ifelse(!is.na(data$A10[cone])&!is.na(data$B10[cone]),
         1/12*pi*data$A10[cone]**2*(sqrt(data$B10[cone]**2-.25*data$A10[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A10[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A10[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A10[cone]**2))*data$corr.fac[cone],
                NA))

data$cell10[cone.half.sphere]<-
  ifelse(!is.na(data$A10[cone.half.sphere])&!is.na(data$B10[cone.half.sphere]),
         1/12*pi*data$A10[cone.half.sphere]**2*(data$B10[cone.half.sphere]+.5*data$A10[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A10[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A10[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A10[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell10[double.cone]<-
  ifelse(!is.na(data$A10[double.cone])&!is.na(data$B10[double.cone]),
         1/12*pi*data$A10[double.cone]**2*data$B10[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A10[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A10[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell10[pyramid]<-
  ifelse(!is.na(data$A10[pyramid])&!is.na(data$B10[pyramid])&!is.na(data$C10[pyramid]),
         1/3*data$A10[pyramid]*data$B10[pyramid]*data$C10[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A10[pyramid])&!is.na(data$B10[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A10[pyramid]*data$B10[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A10[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C10[pyramid]),
                       1/3*data$A10[pyramid]*data$DimB[pyramid]*data$C10[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A10[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A10[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell10[cuboid]<-
  ifelse(!is.na(data$A10[cuboid])&!is.na(data$B10[cuboid])&!is.na(data$C10[cuboid]),
         data$A10[cuboid]*data$B10[cuboid]*data$C10[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A10[cuboid])&!is.na(data$B10[cuboid])&!is.na(data$DimC[cuboid]),
                data$A10[cuboid]*data$B10[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A10[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C10[cuboid]),
                       data$A10[cuboid]*data$DimB[cuboid]*data$C10[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A10[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A10[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell10[lanceol.cyl]<-
  ifelse(!is.na(data$A10[lanceol.cyl])&!is.na(data$B10[lanceol.cyl])&!is.na(data$C10[lanceol.cyl]),
         2/pi*data$A10[lanceol.cyl]*data$B10[lanceol.cyl]*data$C10[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A10[lanceol.cyl])&!is.na(data$B10[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A10[lanceol.cyl]*data$B10[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A10[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C10[lanceol.cyl]),
                       2/pi*data$A10[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C10[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A10[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A10[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell10[cuboid1]<-
  ifelse(!is.na(data$A10[cuboid1])&!is.na(data$B10[cuboid1]),
         data$A10[cuboid1]*data$B10[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A10[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A10[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell11[spindle15]<-ifelse(!is.na(data$A11[spindle15])&!is.na(data$B11[spindle15]),
                               2/15*pi*data$A11[spindle15]**2*data$B11[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A11[spindle15]),
                                      2/15*pi*data$A11[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell11[spindle11]<-
  ifelse(!is.na(data$A11[spindle11])&!is.na(data$B11[spindle11])&!is.na(data$C11[spindle11]),
         2/15*pi*data$A11[spindle11]*data$B11[spindle11]*data$C11[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A11[spindle11])&!is.na(data$B11[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A11[spindle11]*data$B11[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A11[spindle11])&!is.na(data$B11[spindle11]),
                       2/15*pi*data$A11[spindle11]*data$B11[spindle11]*data$B11[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A11[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A11[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell11[rhomb]<-
  ifelse(!is.na(data$A11[rhomb])&!is.na(data$B11[rhomb])&!is.na(data$C11[rhomb]),
         1/2*data$A11[rhomb]*data$B11[rhomb]*data$C11[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A11[rhomb])&!is.na(data$B11[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A11[rhomb]*data$B11[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A11[rhomb])&!is.na(data$B11[rhomb]),
                       1/2*data$A11[rhomb]*data$B11[rhomb]*.75*data$B11[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A11[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A11[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell11[cylinder]<-ifelse(!is.na(data$A11[cylinder])&!is.na(data$B11[cylinder]),
                              1/4*pi*data$A11[cylinder]**2*data$B11[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A11[cylinder]),
                                     1/4*pi*data$A11[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell11[elliptic.cyl]<-
  ifelse(!is.na(data$A11[elliptic.cyl])&!is.na(data$B11[elliptic.cyl])&!is.na(data$C11[elliptic.cyl]),
         1/4*pi*data$A11[elliptic.cyl]*data$B11[elliptic.cyl]*data$C11[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A11[elliptic.cyl])&!is.na(data$B11[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A11[elliptic.cyl]*data$B11[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A11[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C11[elliptic.cyl]),
                       1/4*pi*data$A11[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C11[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A11[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A11[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell11[triang.prism]<-
  ifelse(!is.na(data$A11[triang.prism])&!is.na(data$B11[triang.prism])&!is.na(data$C11[triang.prism]),
         1/2*data$A11[triang.prism]*data$B11[triang.prism]*data$C11[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A11[triang.prism])&!is.na(data$B11[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A11[triang.prism]*data$B11[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A11[triang.prism])&!is.na(data$B11[triang.prism]),
                       1/2*data$A11[triang.prism]*data$B11[triang.prism]*.75*data$B11[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A11[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A11[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell11[sphere]<-
  ifelse(!is.na(data$A11[sphere]),
         1/6*pi*data$A11[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell11[spheroid]<-
  ifelse(!is.na(data$A11[spheroid])&!is.na(data$B11[spheroid]),
         1/6*pi*data$A11[spheroid]*data$B11[spheroid]*data$A11[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A11[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A11[spheroid]*data$DimB[spheroid]*data$A11[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell11[ellipsoid]<-
  ifelse(!is.na(data$A11[ellipsoid])&!is.na(data$B11[ellipsoid])&!is.na(data$C11[ellipsoid]),
         1/6*pi*data$A11[ellipsoid]*data$B11[ellipsoid]*data$C11[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A11[ellipsoid])&!is.na(data$B11[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A11[ellipsoid]*data$B11[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A11[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C11[ellipsoid]),
                       1/6*pi*data$A11[ellipsoid]*data$DimB[ellipsoid]*data$C11[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A11[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A11[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell11[cone]<-
  ifelse(!is.na(data$A11[cone])&!is.na(data$B11[cone]),
         1/12*pi*data$A11[cone]**2*(sqrt(data$B11[cone]**2-.25*data$A11[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A11[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A11[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A11[cone]**2))*data$corr.fac[cone],
                NA))

data$cell11[cone.half.sphere]<-
  ifelse(!is.na(data$A11[cone.half.sphere])&!is.na(data$B11[cone.half.sphere]),
         1/12*pi*data$A11[cone.half.sphere]**2*(data$B11[cone.half.sphere]+.5*data$A11[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A11[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A11[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A11[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell11[double.cone]<-
  ifelse(!is.na(data$A11[double.cone])&!is.na(data$B11[double.cone]),
         1/12*pi*data$A11[double.cone]**2*data$B11[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A11[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A11[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell11[pyramid]<-
  ifelse(!is.na(data$A11[pyramid])&!is.na(data$B11[pyramid])&!is.na(data$C11[pyramid]),
         1/3*data$A11[pyramid]*data$B11[pyramid]*data$C11[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A11[pyramid])&!is.na(data$B11[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A11[pyramid]*data$B11[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A11[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C11[pyramid]),
                       1/3*data$A11[pyramid]*data$DimB[pyramid]*data$C11[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A11[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A11[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell11[cuboid]<-
  ifelse(!is.na(data$A11[cuboid])&!is.na(data$B11[cuboid])&!is.na(data$C11[cuboid]),
         data$A11[cuboid]*data$B11[cuboid]*data$C11[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A11[cuboid])&!is.na(data$B11[cuboid])&!is.na(data$DimC[cuboid]),
                data$A11[cuboid]*data$B11[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A11[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C11[cuboid]),
                       data$A11[cuboid]*data$DimB[cuboid]*data$C11[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A11[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A11[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell11[lanceol.cyl]<-
  ifelse(!is.na(data$A11[lanceol.cyl])&!is.na(data$B11[lanceol.cyl])&!is.na(data$C11[lanceol.cyl]),
         2/pi*data$A11[lanceol.cyl]*data$B11[lanceol.cyl]*data$C11[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A11[lanceol.cyl])&!is.na(data$B11[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A11[lanceol.cyl]*data$B11[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A11[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C11[lanceol.cyl]),
                       2/pi*data$A11[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C11[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A11[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A11[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell11[cuboid1]<-
  ifelse(!is.na(data$A11[cuboid1])&!is.na(data$B11[cuboid1]),
         data$A11[cuboid1]*data$B11[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A11[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A11[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell12[spindle15]<-ifelse(!is.na(data$A12[spindle15])&!is.na(data$B12[spindle15]),
                               2/15*pi*data$A12[spindle15]**2*data$B12[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A12[spindle15]),
                                      2/15*pi*data$A12[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell12[spindle11]<-
  ifelse(!is.na(data$A12[spindle11])&!is.na(data$B12[spindle11])&!is.na(data$C12[spindle11]),
         2/15*pi*data$A12[spindle11]*data$B12[spindle11]*data$C12[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A12[spindle11])&!is.na(data$B12[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A12[spindle11]*data$B12[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A12[spindle11])&!is.na(data$B12[spindle11]),
                       2/15*pi*data$A12[spindle11]*data$B12[spindle11]*data$B12[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A12[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A12[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell12[rhomb]<-
  ifelse(!is.na(data$A12[rhomb])&!is.na(data$B12[rhomb])&!is.na(data$C12[rhomb]),
         1/2*data$A12[rhomb]*data$B12[rhomb]*data$C12[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A12[rhomb])&!is.na(data$B12[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A12[rhomb]*data$B12[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A12[rhomb])&!is.na(data$B12[rhomb]),
                       1/2*data$A12[rhomb]*data$B12[rhomb]*.75*data$B12[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A12[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A12[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell12[cylinder]<-ifelse(!is.na(data$A12[cylinder])&!is.na(data$B12[cylinder]),
                              1/4*pi*data$A12[cylinder]**2*data$B12[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A12[cylinder]),
                                     1/4*pi*data$A12[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell12[elliptic.cyl]<-
  ifelse(!is.na(data$A12[elliptic.cyl])&!is.na(data$B12[elliptic.cyl])&!is.na(data$C12[elliptic.cyl]),
         1/4*pi*data$A12[elliptic.cyl]*data$B12[elliptic.cyl]*data$C12[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A12[elliptic.cyl])&!is.na(data$B12[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A12[elliptic.cyl]*data$B12[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A12[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C12[elliptic.cyl]),
                       1/4*pi*data$A12[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C12[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A12[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A12[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell12[triang.prism]<-
  ifelse(!is.na(data$A12[triang.prism])&!is.na(data$B12[triang.prism])&!is.na(data$C12[triang.prism]),
         1/2*data$A12[triang.prism]*data$B12[triang.prism]*data$C12[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A12[triang.prism])&!is.na(data$B12[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A12[triang.prism]*data$B12[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A12[triang.prism])&!is.na(data$B12[triang.prism]),
                       1/2*data$A12[triang.prism]*data$B12[triang.prism]*.75*data$B12[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A12[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A12[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell12[sphere]<-
  ifelse(!is.na(data$A12[sphere]),
         1/6*pi*data$A12[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell12[spheroid]<-
  ifelse(!is.na(data$A12[spheroid])&!is.na(data$B12[spheroid]),
         1/6*pi*data$A12[spheroid]*data$B12[spheroid]*data$A12[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A12[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A12[spheroid]*data$DimB[spheroid]*data$A12[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell12[ellipsoid]<-
  ifelse(!is.na(data$A12[ellipsoid])&!is.na(data$B12[ellipsoid])&!is.na(data$C12[ellipsoid]),
         1/6*pi*data$A12[ellipsoid]*data$B12[ellipsoid]*data$C12[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A12[ellipsoid])&!is.na(data$B12[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A12[ellipsoid]*data$B12[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A12[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C12[ellipsoid]),
                       1/6*pi*data$A12[ellipsoid]*data$DimB[ellipsoid]*data$C12[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A12[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A12[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell12[cone]<-
  ifelse(!is.na(data$A12[cone])&!is.na(data$B12[cone]),
         1/12*pi*data$A12[cone]**2*(sqrt(data$B12[cone]**2-.25*data$A12[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A12[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A12[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A12[cone]**2))*data$corr.fac[cone],
                NA))

data$cell12[cone.half.sphere]<-
  ifelse(!is.na(data$A12[cone.half.sphere])&!is.na(data$B12[cone.half.sphere]),
         1/12*pi*data$A12[cone.half.sphere]**2*(data$B12[cone.half.sphere]+.5*data$A12[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A12[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A12[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A12[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell12[double.cone]<-
  ifelse(!is.na(data$A12[double.cone])&!is.na(data$B12[double.cone]),
         1/12*pi*data$A12[double.cone]**2*data$B12[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A12[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A12[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell12[pyramid]<-
  ifelse(!is.na(data$A12[pyramid])&!is.na(data$B12[pyramid])&!is.na(data$C12[pyramid]),
         1/3*data$A12[pyramid]*data$B12[pyramid]*data$C12[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A12[pyramid])&!is.na(data$B12[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A12[pyramid]*data$B12[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A12[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C12[pyramid]),
                       1/3*data$A12[pyramid]*data$DimB[pyramid]*data$C12[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A12[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A12[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell12[cuboid]<-
  ifelse(!is.na(data$A12[cuboid])&!is.na(data$B12[cuboid])&!is.na(data$C12[cuboid]),
         data$A12[cuboid]*data$B12[cuboid]*data$C12[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A12[cuboid])&!is.na(data$B12[cuboid])&!is.na(data$DimC[cuboid]),
                data$A12[cuboid]*data$B12[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A12[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C12[cuboid]),
                       data$A12[cuboid]*data$DimB[cuboid]*data$C12[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A12[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A12[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell12[lanceol.cyl]<-
  ifelse(!is.na(data$A12[lanceol.cyl])&!is.na(data$B12[lanceol.cyl])&!is.na(data$C12[lanceol.cyl]),
         2/pi*data$A12[lanceol.cyl]*data$B12[lanceol.cyl]*data$C12[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A12[lanceol.cyl])&!is.na(data$B12[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A12[lanceol.cyl]*data$B12[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A12[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C12[lanceol.cyl]),
                       2/pi*data$A12[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C12[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A12[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A12[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell12[cuboid1]<-
  ifelse(!is.na(data$A12[cuboid1])&!is.na(data$B12[cuboid1]),
         data$A12[cuboid1]*data$B12[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A12[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A12[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell13[spindle15]<-ifelse(!is.na(data$A13[spindle15])&!is.na(data$B13[spindle15]),
                               2/15*pi*data$A13[spindle15]**2*data$B13[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A13[spindle15]),
                                      2/15*pi*data$A13[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell13[spindle11]<-
  ifelse(!is.na(data$A13[spindle11])&!is.na(data$B13[spindle11])&!is.na(data$C13[spindle11]),
         2/15*pi*data$A13[spindle11]*data$B13[spindle11]*data$C13[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A13[spindle11])&!is.na(data$B13[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A13[spindle11]*data$B13[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A13[spindle11])&!is.na(data$B13[spindle11]),
                       2/15*pi*data$A13[spindle11]*data$B13[spindle11]*data$B13[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A13[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A13[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell13[rhomb]<-
  ifelse(!is.na(data$A13[rhomb])&!is.na(data$B13[rhomb])&!is.na(data$C13[rhomb]),
         1/2*data$A13[rhomb]*data$B13[rhomb]*data$C13[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A13[rhomb])&!is.na(data$B13[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A13[rhomb]*data$B13[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A13[rhomb])&!is.na(data$B13[rhomb]),
                       1/2*data$A13[rhomb]*data$B13[rhomb]*.75*data$B13[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A13[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A13[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell13[cylinder]<-ifelse(!is.na(data$A13[cylinder])&!is.na(data$B13[cylinder]),
                              1/4*pi*data$A13[cylinder]**2*data$B13[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A13[cylinder]),
                                     1/4*pi*data$A13[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell13[elliptic.cyl]<-
  ifelse(!is.na(data$A13[elliptic.cyl])&!is.na(data$B13[elliptic.cyl])&!is.na(data$C13[elliptic.cyl]),
         1/4*pi*data$A13[elliptic.cyl]*data$B13[elliptic.cyl]*data$C13[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A13[elliptic.cyl])&!is.na(data$B13[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A13[elliptic.cyl]*data$B13[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A13[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C13[elliptic.cyl]),
                       1/4*pi*data$A13[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C13[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A13[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A13[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell13[triang.prism]<-
  ifelse(!is.na(data$A13[triang.prism])&!is.na(data$B13[triang.prism])&!is.na(data$C13[triang.prism]),
         1/2*data$A13[triang.prism]*data$B13[triang.prism]*data$C13[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A13[triang.prism])&!is.na(data$B13[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A13[triang.prism]*data$B13[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A13[triang.prism])&!is.na(data$B13[triang.prism]),
                       1/2*data$A13[triang.prism]*data$B13[triang.prism]*.75*data$B13[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A13[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A13[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell13[sphere]<-
  ifelse(!is.na(data$A13[sphere]),
         1/6*pi*data$A13[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell13[spheroid]<-
  ifelse(!is.na(data$A13[spheroid])&!is.na(data$B13[spheroid]),
         1/6*pi*data$A13[spheroid]*data$B13[spheroid]*data$A13[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A13[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A13[spheroid]*data$DimB[spheroid]*data$A13[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell13[ellipsoid]<-
  ifelse(!is.na(data$A13[ellipsoid])&!is.na(data$B13[ellipsoid])&!is.na(data$C13[ellipsoid]),
         1/6*pi*data$A13[ellipsoid]*data$B13[ellipsoid]*data$C13[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A13[ellipsoid])&!is.na(data$B13[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A13[ellipsoid]*data$B13[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A13[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C13[ellipsoid]),
                       1/6*pi*data$A13[ellipsoid]*data$DimB[ellipsoid]*data$C13[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A13[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A13[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell13[cone]<-
  ifelse(!is.na(data$A13[cone])&!is.na(data$B13[cone]),
         1/12*pi*data$A13[cone]**2*(sqrt(data$B13[cone]**2-.25*data$A13[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A13[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A13[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A13[cone]**2))*data$corr.fac[cone],
                NA))

data$cell13[cone.half.sphere]<-
  ifelse(!is.na(data$A13[cone.half.sphere])&!is.na(data$B13[cone.half.sphere]),
         1/12*pi*data$A13[cone.half.sphere]**2*(data$B13[cone.half.sphere]+.5*data$A13[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A13[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A13[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A13[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell13[double.cone]<-
  ifelse(!is.na(data$A13[double.cone])&!is.na(data$B13[double.cone]),
         1/12*pi*data$A13[double.cone]**2*data$B13[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A13[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A13[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell13[pyramid]<-
  ifelse(!is.na(data$A13[pyramid])&!is.na(data$B13[pyramid])&!is.na(data$C13[pyramid]),
         1/3*data$A13[pyramid]*data$B13[pyramid]*data$C13[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A13[pyramid])&!is.na(data$B13[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A13[pyramid]*data$B13[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A13[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C13[pyramid]),
                       1/3*data$A13[pyramid]*data$DimB[pyramid]*data$C13[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A13[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A13[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell13[cuboid]<-
  ifelse(!is.na(data$A13[cuboid])&!is.na(data$B13[cuboid])&!is.na(data$C13[cuboid]),
         data$A13[cuboid]*data$B13[cuboid]*data$C13[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A13[cuboid])&!is.na(data$B13[cuboid])&!is.na(data$DimC[cuboid]),
                data$A13[cuboid]*data$B13[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A13[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C13[cuboid]),
                       data$A13[cuboid]*data$DimB[cuboid]*data$C13[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A13[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A13[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell13[lanceol.cyl]<-
  ifelse(!is.na(data$A13[lanceol.cyl])&!is.na(data$B13[lanceol.cyl])&!is.na(data$C13[lanceol.cyl]),
         2/pi*data$A13[lanceol.cyl]*data$B13[lanceol.cyl]*data$C13[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A13[lanceol.cyl])&!is.na(data$B13[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A13[lanceol.cyl]*data$B13[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A13[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C13[lanceol.cyl]),
                       2/pi*data$A13[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C13[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A13[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A13[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell13[cuboid1]<-
  ifelse(!is.na(data$A13[cuboid1])&!is.na(data$B13[cuboid1]),
         data$A13[cuboid1]*data$B13[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A13[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A13[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell14[spindle15]<-ifelse(!is.na(data$A14[spindle15])&!is.na(data$B14[spindle15]),
                               2/15*pi*data$A14[spindle15]**2*data$B14[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A14[spindle15]),
                                      2/15*pi*data$A14[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell14[spindle11]<-
  ifelse(!is.na(data$A14[spindle11])&!is.na(data$B14[spindle11])&!is.na(data$C14[spindle11]),
         2/15*pi*data$A14[spindle11]*data$B14[spindle11]*data$C14[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A14[spindle11])&!is.na(data$B14[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A14[spindle11]*data$B14[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A14[spindle11])&!is.na(data$B14[spindle11]),
                       2/15*pi*data$A14[spindle11]*data$B14[spindle11]*data$B14[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A14[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A14[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell14[rhomb]<-
  ifelse(!is.na(data$A14[rhomb])&!is.na(data$B14[rhomb])&!is.na(data$C14[rhomb]),
         1/2*data$A14[rhomb]*data$B14[rhomb]*data$C14[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A14[rhomb])&!is.na(data$B14[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A14[rhomb]*data$B14[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A14[rhomb])&!is.na(data$B14[rhomb]),
                       1/2*data$A14[rhomb]*data$B14[rhomb]*.75*data$B14[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A14[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A14[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell14[cylinder]<-ifelse(!is.na(data$A14[cylinder])&!is.na(data$B14[cylinder]),
                              1/4*pi*data$A14[cylinder]**2*data$B14[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A14[cylinder]),
                                     1/4*pi*data$A14[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell14[elliptic.cyl]<-
  ifelse(!is.na(data$A14[elliptic.cyl])&!is.na(data$B14[elliptic.cyl])&!is.na(data$C14[elliptic.cyl]),
         1/4*pi*data$A14[elliptic.cyl]*data$B14[elliptic.cyl]*data$C14[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A14[elliptic.cyl])&!is.na(data$B14[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A14[elliptic.cyl]*data$B14[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A14[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C14[elliptic.cyl]),
                       1/4*pi*data$A14[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C14[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A14[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A14[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell14[triang.prism]<-
  ifelse(!is.na(data$A14[triang.prism])&!is.na(data$B14[triang.prism])&!is.na(data$C14[triang.prism]),
         1/2*data$A14[triang.prism]*data$B14[triang.prism]*data$C14[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A14[triang.prism])&!is.na(data$B14[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A14[triang.prism]*data$B14[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A14[triang.prism])&!is.na(data$B14[triang.prism]),
                       1/2*data$A14[triang.prism]*data$B14[triang.prism]*.75*data$B14[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A14[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A14[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell14[sphere]<-
  ifelse(!is.na(data$A14[sphere]),
         1/6*pi*data$A14[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell14[spheroid]<-
  ifelse(!is.na(data$A14[spheroid])&!is.na(data$B14[spheroid]),
         1/6*pi*data$A14[spheroid]*data$B14[spheroid]*data$A14[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A14[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A14[spheroid]*data$DimB[spheroid]*data$A14[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell14[ellipsoid]<-
  ifelse(!is.na(data$A14[ellipsoid])&!is.na(data$B14[ellipsoid])&!is.na(data$C14[ellipsoid]),
         1/6*pi*data$A14[ellipsoid]*data$B14[ellipsoid]*data$C14[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A14[ellipsoid])&!is.na(data$B14[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A14[ellipsoid]*data$B14[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A14[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C14[ellipsoid]),
                       1/6*pi*data$A14[ellipsoid]*data$DimB[ellipsoid]*data$C14[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A14[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A14[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell14[cone]<-
  ifelse(!is.na(data$A14[cone])&!is.na(data$B14[cone]),
         1/12*pi*data$A14[cone]**2*(sqrt(data$B14[cone]**2-.25*data$A14[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A14[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A14[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A14[cone]**2))*data$corr.fac[cone],
                NA))

data$cell14[cone.half.sphere]<-
  ifelse(!is.na(data$A14[cone.half.sphere])&!is.na(data$B14[cone.half.sphere]),
         1/12*pi*data$A14[cone.half.sphere]**2*(data$B14[cone.half.sphere]+.5*data$A14[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A14[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A14[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A14[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell14[double.cone]<-
  ifelse(!is.na(data$A14[double.cone])&!is.na(data$B14[double.cone]),
         1/12*pi*data$A14[double.cone]**2*data$B14[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A14[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A14[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell14[pyramid]<-
  ifelse(!is.na(data$A14[pyramid])&!is.na(data$B14[pyramid])&!is.na(data$C14[pyramid]),
         1/3*data$A14[pyramid]*data$B14[pyramid]*data$C14[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A14[pyramid])&!is.na(data$B14[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A14[pyramid]*data$B14[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A14[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C14[pyramid]),
                       1/3*data$A14[pyramid]*data$DimB[pyramid]*data$C14[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A14[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A14[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell14[cuboid]<-
  ifelse(!is.na(data$A14[cuboid])&!is.na(data$B14[cuboid])&!is.na(data$C14[cuboid]),
         data$A14[cuboid]*data$B14[cuboid]*data$C14[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A14[cuboid])&!is.na(data$B14[cuboid])&!is.na(data$DimC[cuboid]),
                data$A14[cuboid]*data$B14[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A14[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C14[cuboid]),
                       data$A14[cuboid]*data$DimB[cuboid]*data$C14[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A14[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A14[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell14[lanceol.cyl]<-
  ifelse(!is.na(data$A14[lanceol.cyl])&!is.na(data$B14[lanceol.cyl])&!is.na(data$C14[lanceol.cyl]),
         2/pi*data$A14[lanceol.cyl]*data$B14[lanceol.cyl]*data$C14[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A14[lanceol.cyl])&!is.na(data$B14[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A14[lanceol.cyl]*data$B14[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A14[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C14[lanceol.cyl]),
                       2/pi*data$A14[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C14[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A14[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A14[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell14[cuboid1]<-
  ifelse(!is.na(data$A14[cuboid1])&!is.na(data$B14[cuboid1]),
         data$A14[cuboid1]*data$B14[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A14[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A14[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))



#A,B,C,Cell
#Cell1

data$cell15[spindle15]<-ifelse(!is.na(data$A15[spindle15])&!is.na(data$B15[spindle15]),
                               2/15*pi*data$A15[spindle15]**2*data$B15[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A15[spindle15]),
                                      2/15*pi*data$A15[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell15[spindle11]<-
  ifelse(!is.na(data$A15[spindle11])&!is.na(data$B15[spindle11])&!is.na(data$C15[spindle11]),
         2/15*pi*data$A15[spindle11]*data$B15[spindle11]*data$C15[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A15[spindle11])&!is.na(data$B15[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A15[spindle11]*data$B15[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A15[spindle11])&!is.na(data$B15[spindle11]),
                       2/15*pi*data$A15[spindle11]*data$B15[spindle11]*data$B15[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A15[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A15[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell15[rhomb]<-
  ifelse(!is.na(data$A15[rhomb])&!is.na(data$B15[rhomb])&!is.na(data$C15[rhomb]),
         1/2*data$A15[rhomb]*data$B15[rhomb]*data$C15[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A15[rhomb])&!is.na(data$B15[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A15[rhomb]*data$B15[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A15[rhomb])&!is.na(data$B15[rhomb]),
                       1/2*data$A15[rhomb]*data$B15[rhomb]*.75*data$B15[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A15[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A15[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell15[cylinder]<-ifelse(!is.na(data$A15[cylinder])&!is.na(data$B15[cylinder]),
                              1/4*pi*data$A15[cylinder]**2*data$B15[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A15[cylinder]),
                                     1/4*pi*data$A15[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell15[elliptic.cyl]<-
  ifelse(!is.na(data$A15[elliptic.cyl])&!is.na(data$B15[elliptic.cyl])&!is.na(data$C15[elliptic.cyl]),
         1/4*pi*data$A15[elliptic.cyl]*data$B15[elliptic.cyl]*data$C15[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A15[elliptic.cyl])&!is.na(data$B15[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A15[elliptic.cyl]*data$B15[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A15[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C15[elliptic.cyl]),
                       1/4*pi*data$A15[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C15[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A15[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A15[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell15[triang.prism]<-
  ifelse(!is.na(data$A15[triang.prism])&!is.na(data$B15[triang.prism])&!is.na(data$C15[triang.prism]),
         1/2*data$A15[triang.prism]*data$B15[triang.prism]*data$C15[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A15[triang.prism])&!is.na(data$B15[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A15[triang.prism]*data$B15[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A15[triang.prism])&!is.na(data$B15[triang.prism]),
                       1/2*data$A15[triang.prism]*data$B15[triang.prism]*.75*data$B15[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A15[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A15[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell15[sphere]<-
  ifelse(!is.na(data$A15[sphere]),
         1/6*pi*data$A15[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell15[spheroid]<-
  ifelse(!is.na(data$A15[spheroid])&!is.na(data$B15[spheroid]),
         1/6*pi*data$A15[spheroid]*data$B15[spheroid]*data$A15[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A15[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A15[spheroid]*data$DimB[spheroid]*data$A15[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell15[ellipsoid]<-
  ifelse(!is.na(data$A15[ellipsoid])&!is.na(data$B15[ellipsoid])&!is.na(data$C15[ellipsoid]),
         1/6*pi*data$A15[ellipsoid]*data$B15[ellipsoid]*data$C15[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A15[ellipsoid])&!is.na(data$B15[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A15[ellipsoid]*data$B15[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A15[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C15[ellipsoid]),
                       1/6*pi*data$A15[ellipsoid]*data$DimB[ellipsoid]*data$C15[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A15[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A15[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell15[cone]<-
  ifelse(!is.na(data$A15[cone])&!is.na(data$B15[cone]),
         1/12*pi*data$A15[cone]**2*(sqrt(data$B15[cone]**2-.25*data$A15[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A15[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A15[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A15[cone]**2))*data$corr.fac[cone],
                NA))

data$cell15[cone.half.sphere]<-
  ifelse(!is.na(data$A15[cone.half.sphere])&!is.na(data$B15[cone.half.sphere]),
         1/12*pi*data$A15[cone.half.sphere]**2*(data$B15[cone.half.sphere]+.5*data$A15[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A15[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A15[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A15[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell15[double.cone]<-
  ifelse(!is.na(data$A15[double.cone])&!is.na(data$B15[double.cone]),
         1/12*pi*data$A15[double.cone]**2*data$B15[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A15[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A15[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell15[pyramid]<-
  ifelse(!is.na(data$A15[pyramid])&!is.na(data$B15[pyramid])&!is.na(data$C15[pyramid]),
         1/3*data$A15[pyramid]*data$B15[pyramid]*data$C15[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A15[pyramid])&!is.na(data$B15[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A15[pyramid]*data$B15[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A15[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C15[pyramid]),
                       1/3*data$A15[pyramid]*data$DimB[pyramid]*data$C15[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A15[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A15[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell15[cuboid]<-
  ifelse(!is.na(data$A15[cuboid])&!is.na(data$B15[cuboid])&!is.na(data$C15[cuboid]),
         data$A15[cuboid]*data$B15[cuboid]*data$C15[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A15[cuboid])&!is.na(data$B15[cuboid])&!is.na(data$DimC[cuboid]),
                data$A15[cuboid]*data$B15[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A15[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C15[cuboid]),
                       data$A15[cuboid]*data$DimB[cuboid]*data$C15[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A15[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A15[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell15[lanceol.cyl]<-
  ifelse(!is.na(data$A15[lanceol.cyl])&!is.na(data$B15[lanceol.cyl])&!is.na(data$C15[lanceol.cyl]),
         2/pi*data$A15[lanceol.cyl]*data$B15[lanceol.cyl]*data$C15[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A15[lanceol.cyl])&!is.na(data$B15[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A15[lanceol.cyl]*data$B15[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A15[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C15[lanceol.cyl]),
                       2/pi*data$A15[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C15[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A15[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A15[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell15[cuboid1]<-
  ifelse(!is.na(data$A15[cuboid1])&!is.na(data$B15[cuboid1]),
         data$A15[cuboid1]*data$B15[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A15[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A15[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell16[spindle15]<-ifelse(!is.na(data$A16[spindle15])&!is.na(data$B16[spindle15]),
                               2/15*pi*data$A16[spindle15]**2*data$B16[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A16[spindle15]),
                                      2/15*pi*data$A16[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell16[spindle11]<-
  ifelse(!is.na(data$A16[spindle11])&!is.na(data$B16[spindle11])&!is.na(data$C16[spindle11]),
         2/15*pi*data$A16[spindle11]*data$B16[spindle11]*data$C16[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A16[spindle11])&!is.na(data$B16[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A16[spindle11]*data$B16[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A16[spindle11])&!is.na(data$B16[spindle11]),
                       2/15*pi*data$A16[spindle11]*data$B16[spindle11]*data$B16[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A16[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A16[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell16[rhomb]<-
  ifelse(!is.na(data$A16[rhomb])&!is.na(data$B16[rhomb])&!is.na(data$C16[rhomb]),
         1/2*data$A16[rhomb]*data$B16[rhomb]*data$C16[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A16[rhomb])&!is.na(data$B16[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A16[rhomb]*data$B16[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A16[rhomb])&!is.na(data$B16[rhomb]),
                       1/2*data$A16[rhomb]*data$B16[rhomb]*.75*data$B16[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A16[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A16[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell16[cylinder]<-ifelse(!is.na(data$A16[cylinder])&!is.na(data$B16[cylinder]),
                              1/4*pi*data$A16[cylinder]**2*data$B16[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A16[cylinder]),
                                     1/4*pi*data$A16[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell16[elliptic.cyl]<-
  ifelse(!is.na(data$A16[elliptic.cyl])&!is.na(data$B16[elliptic.cyl])&!is.na(data$C16[elliptic.cyl]),
         1/4*pi*data$A16[elliptic.cyl]*data$B16[elliptic.cyl]*data$C16[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A16[elliptic.cyl])&!is.na(data$B16[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A16[elliptic.cyl]*data$B16[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A16[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C16[elliptic.cyl]),
                       1/4*pi*data$A16[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C16[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A16[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A16[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell16[triang.prism]<-
  ifelse(!is.na(data$A16[triang.prism])&!is.na(data$B16[triang.prism])&!is.na(data$C16[triang.prism]),
         1/2*data$A16[triang.prism]*data$B16[triang.prism]*data$C16[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A16[triang.prism])&!is.na(data$B16[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A16[triang.prism]*data$B16[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A16[triang.prism])&!is.na(data$B16[triang.prism]),
                       1/2*data$A16[triang.prism]*data$B16[triang.prism]*.75*data$B16[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A16[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A16[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell16[sphere]<-
  ifelse(!is.na(data$A16[sphere]),
         1/6*pi*data$A16[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell16[spheroid]<-
  ifelse(!is.na(data$A16[spheroid])&!is.na(data$B16[spheroid]),
         1/6*pi*data$A16[spheroid]*data$B16[spheroid]*data$A16[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A16[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A16[spheroid]*data$DimB[spheroid]*data$A16[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell16[ellipsoid]<-
  ifelse(!is.na(data$A16[ellipsoid])&!is.na(data$B16[ellipsoid])&!is.na(data$C16[ellipsoid]),
         1/6*pi*data$A16[ellipsoid]*data$B16[ellipsoid]*data$C16[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A16[ellipsoid])&!is.na(data$B16[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A16[ellipsoid]*data$B16[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A16[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C16[ellipsoid]),
                       1/6*pi*data$A16[ellipsoid]*data$DimB[ellipsoid]*data$C16[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A16[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A16[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell16[cone]<-
  ifelse(!is.na(data$A16[cone])&!is.na(data$B16[cone]),
         1/12*pi*data$A16[cone]**2*(sqrt(data$B16[cone]**2-.25*data$A16[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A16[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A16[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A16[cone]**2))*data$corr.fac[cone],
                NA))

data$cell16[cone.half.sphere]<-
  ifelse(!is.na(data$A16[cone.half.sphere])&!is.na(data$B16[cone.half.sphere]),
         1/12*pi*data$A16[cone.half.sphere]**2*(data$B16[cone.half.sphere]+.5*data$A16[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A16[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A16[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A16[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell16[double.cone]<-
  ifelse(!is.na(data$A16[double.cone])&!is.na(data$B16[double.cone]),
         1/12*pi*data$A16[double.cone]**2*data$B16[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A16[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A16[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell16[pyramid]<-
  ifelse(!is.na(data$A16[pyramid])&!is.na(data$B16[pyramid])&!is.na(data$C16[pyramid]),
         1/3*data$A16[pyramid]*data$B16[pyramid]*data$C16[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A16[pyramid])&!is.na(data$B16[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A16[pyramid]*data$B16[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A16[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C16[pyramid]),
                       1/3*data$A16[pyramid]*data$DimB[pyramid]*data$C16[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A16[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A16[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell16[cuboid]<-
  ifelse(!is.na(data$A16[cuboid])&!is.na(data$B16[cuboid])&!is.na(data$C16[cuboid]),
         data$A16[cuboid]*data$B16[cuboid]*data$C16[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A16[cuboid])&!is.na(data$B16[cuboid])&!is.na(data$DimC[cuboid]),
                data$A16[cuboid]*data$B16[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A16[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C16[cuboid]),
                       data$A16[cuboid]*data$DimB[cuboid]*data$C16[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A16[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A16[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell16[lanceol.cyl]<-
  ifelse(!is.na(data$A16[lanceol.cyl])&!is.na(data$B16[lanceol.cyl])&!is.na(data$C16[lanceol.cyl]),
         2/pi*data$A16[lanceol.cyl]*data$B16[lanceol.cyl]*data$C16[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A16[lanceol.cyl])&!is.na(data$B16[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A16[lanceol.cyl]*data$B16[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A16[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C16[lanceol.cyl]),
                       2/pi*data$A16[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C16[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A16[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A16[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell16[cuboid1]<-
  ifelse(!is.na(data$A16[cuboid1])&!is.na(data$B16[cuboid1]),
         data$A16[cuboid1]*data$B16[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A16[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A16[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell17[spindle15]<-ifelse(!is.na(data$A17[spindle15])&!is.na(data$B17[spindle15]),
                               2/15*pi*data$A17[spindle15]**2*data$B17[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A17[spindle15]),
                                      2/15*pi*data$A17[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell17[spindle11]<-
  ifelse(!is.na(data$A17[spindle11])&!is.na(data$B17[spindle11])&!is.na(data$C17[spindle11]),
         2/15*pi*data$A17[spindle11]*data$B17[spindle11]*data$C17[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A17[spindle11])&!is.na(data$B17[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A17[spindle11]*data$B17[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A17[spindle11])&!is.na(data$B17[spindle11]),
                       2/15*pi*data$A17[spindle11]*data$B17[spindle11]*data$B17[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A17[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A17[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell17[rhomb]<-
  ifelse(!is.na(data$A17[rhomb])&!is.na(data$B17[rhomb])&!is.na(data$C17[rhomb]),
         1/2*data$A17[rhomb]*data$B17[rhomb]*data$C17[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A17[rhomb])&!is.na(data$B17[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A17[rhomb]*data$B17[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A17[rhomb])&!is.na(data$B17[rhomb]),
                       1/2*data$A17[rhomb]*data$B17[rhomb]*.75*data$B17[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A17[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A17[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell17[cylinder]<-ifelse(!is.na(data$A17[cylinder])&!is.na(data$B17[cylinder]),
                              1/4*pi*data$A17[cylinder]**2*data$B17[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A17[cylinder]),
                                     1/4*pi*data$A17[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell17[elliptic.cyl]<-
  ifelse(!is.na(data$A17[elliptic.cyl])&!is.na(data$B17[elliptic.cyl])&!is.na(data$C17[elliptic.cyl]),
         1/4*pi*data$A17[elliptic.cyl]*data$B17[elliptic.cyl]*data$C17[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A17[elliptic.cyl])&!is.na(data$B17[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A17[elliptic.cyl]*data$B17[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A17[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C17[elliptic.cyl]),
                       1/4*pi*data$A17[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C17[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A17[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A17[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell17[triang.prism]<-
  ifelse(!is.na(data$A17[triang.prism])&!is.na(data$B17[triang.prism])&!is.na(data$C17[triang.prism]),
         1/2*data$A17[triang.prism]*data$B17[triang.prism]*data$C17[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A17[triang.prism])&!is.na(data$B17[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A17[triang.prism]*data$B17[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A17[triang.prism])&!is.na(data$B17[triang.prism]),
                       1/2*data$A17[triang.prism]*data$B17[triang.prism]*.75*data$B17[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A17[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A17[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell17[sphere]<-
  ifelse(!is.na(data$A17[sphere]),
         1/6*pi*data$A17[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell17[spheroid]<-
  ifelse(!is.na(data$A17[spheroid])&!is.na(data$B17[spheroid]),
         1/6*pi*data$A17[spheroid]*data$B17[spheroid]*data$A17[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A17[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A17[spheroid]*data$DimB[spheroid]*data$A17[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell17[ellipsoid]<-
  ifelse(!is.na(data$A17[ellipsoid])&!is.na(data$B17[ellipsoid])&!is.na(data$C17[ellipsoid]),
         1/6*pi*data$A17[ellipsoid]*data$B17[ellipsoid]*data$C17[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A17[ellipsoid])&!is.na(data$B17[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A17[ellipsoid]*data$B17[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A17[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C17[ellipsoid]),
                       1/6*pi*data$A17[ellipsoid]*data$DimB[ellipsoid]*data$C17[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A17[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A17[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell17[cone]<-
  ifelse(!is.na(data$A17[cone])&!is.na(data$B17[cone]),
         1/12*pi*data$A17[cone]**2*(sqrt(data$B17[cone]**2-.25*data$A17[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A17[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A17[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A17[cone]**2))*data$corr.fac[cone],
                NA))

data$cell17[cone.half.sphere]<-
  ifelse(!is.na(data$A17[cone.half.sphere])&!is.na(data$B17[cone.half.sphere]),
         1/12*pi*data$A17[cone.half.sphere]**2*(data$B17[cone.half.sphere]+.5*data$A17[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A17[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A17[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A17[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell17[double.cone]<-
  ifelse(!is.na(data$A17[double.cone])&!is.na(data$B17[double.cone]),
         1/12*pi*data$A17[double.cone]**2*data$B17[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A17[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A17[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell17[pyramid]<-
  ifelse(!is.na(data$A17[pyramid])&!is.na(data$B17[pyramid])&!is.na(data$C17[pyramid]),
         1/3*data$A17[pyramid]*data$B17[pyramid]*data$C17[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A17[pyramid])&!is.na(data$B17[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A17[pyramid]*data$B17[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A17[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C17[pyramid]),
                       1/3*data$A17[pyramid]*data$DimB[pyramid]*data$C17[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A17[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A17[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell17[cuboid]<-
  ifelse(!is.na(data$A17[cuboid])&!is.na(data$B17[cuboid])&!is.na(data$C17[cuboid]),
         data$A17[cuboid]*data$B17[cuboid]*data$C17[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A17[cuboid])&!is.na(data$B17[cuboid])&!is.na(data$DimC[cuboid]),
                data$A17[cuboid]*data$B17[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A17[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C17[cuboid]),
                       data$A17[cuboid]*data$DimB[cuboid]*data$C17[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A17[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A17[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell17[lanceol.cyl]<-
  ifelse(!is.na(data$A17[lanceol.cyl])&!is.na(data$B17[lanceol.cyl])&!is.na(data$C17[lanceol.cyl]),
         2/pi*data$A17[lanceol.cyl]*data$B17[lanceol.cyl]*data$C17[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A17[lanceol.cyl])&!is.na(data$B17[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A17[lanceol.cyl]*data$B17[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A17[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C17[lanceol.cyl]),
                       2/pi*data$A17[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C17[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A17[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A17[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell17[cuboid1]<-
  ifelse(!is.na(data$A17[cuboid1])&!is.na(data$B17[cuboid1]),
         data$A17[cuboid1]*data$B17[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A17[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A17[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))



#A,B,C,Cell
#Cell1

data$cell18[spindle15]<-ifelse(!is.na(data$A18[spindle15])&!is.na(data$B18[spindle15]),
                               2/15*pi*data$A18[spindle15]**2*data$B18[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A18[spindle15]),
                                      2/15*pi*data$A18[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell18[spindle11]<-
  ifelse(!is.na(data$A18[spindle11])&!is.na(data$B18[spindle11])&!is.na(data$C18[spindle11]),
         2/15*pi*data$A18[spindle11]*data$B18[spindle11]*data$C18[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A18[spindle11])&!is.na(data$B18[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A18[spindle11]*data$B18[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A18[spindle11])&!is.na(data$B18[spindle11]),
                       2/15*pi*data$A18[spindle11]*data$B18[spindle11]*data$B18[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A18[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A18[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell18[rhomb]<-
  ifelse(!is.na(data$A18[rhomb])&!is.na(data$B18[rhomb])&!is.na(data$C18[rhomb]),
         1/2*data$A18[rhomb]*data$B18[rhomb]*data$C18[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A18[rhomb])&!is.na(data$B18[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A18[rhomb]*data$B18[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A18[rhomb])&!is.na(data$B18[rhomb]),
                       1/2*data$A18[rhomb]*data$B18[rhomb]*.75*data$B18[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A18[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A18[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell18[cylinder]<-ifelse(!is.na(data$A18[cylinder])&!is.na(data$B18[cylinder]),
                              1/4*pi*data$A18[cylinder]**2*data$B18[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A18[cylinder]),
                                     1/4*pi*data$A18[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell18[elliptic.cyl]<-
  ifelse(!is.na(data$A18[elliptic.cyl])&!is.na(data$B18[elliptic.cyl])&!is.na(data$C18[elliptic.cyl]),
         1/4*pi*data$A18[elliptic.cyl]*data$B18[elliptic.cyl]*data$C18[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A18[elliptic.cyl])&!is.na(data$B18[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A18[elliptic.cyl]*data$B18[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A18[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C18[elliptic.cyl]),
                       1/4*pi*data$A18[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C18[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A18[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A18[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell18[triang.prism]<-
  ifelse(!is.na(data$A18[triang.prism])&!is.na(data$B18[triang.prism])&!is.na(data$C18[triang.prism]),
         1/2*data$A18[triang.prism]*data$B18[triang.prism]*data$C18[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A18[triang.prism])&!is.na(data$B18[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A18[triang.prism]*data$B18[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A18[triang.prism])&!is.na(data$B18[triang.prism]),
                       1/2*data$A18[triang.prism]*data$B18[triang.prism]*.75*data$B18[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A18[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A18[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell18[sphere]<-
  ifelse(!is.na(data$A18[sphere]),
         1/6*pi*data$A18[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell18[spheroid]<-
  ifelse(!is.na(data$A18[spheroid])&!is.na(data$B18[spheroid]),
         1/6*pi*data$A18[spheroid]*data$B18[spheroid]*data$A18[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A18[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A18[spheroid]*data$DimB[spheroid]*data$A18[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell18[ellipsoid]<-
  ifelse(!is.na(data$A18[ellipsoid])&!is.na(data$B18[ellipsoid])&!is.na(data$C18[ellipsoid]),
         1/6*pi*data$A18[ellipsoid]*data$B18[ellipsoid]*data$C18[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A18[ellipsoid])&!is.na(data$B18[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A18[ellipsoid]*data$B18[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A18[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C18[ellipsoid]),
                       1/6*pi*data$A18[ellipsoid]*data$DimB[ellipsoid]*data$C18[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A18[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A18[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell18[cone]<-
  ifelse(!is.na(data$A18[cone])&!is.na(data$B18[cone]),
         1/12*pi*data$A18[cone]**2*(sqrt(data$B18[cone]**2-.25*data$A18[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A18[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A18[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A18[cone]**2))*data$corr.fac[cone],
                NA))

data$cell18[cone.half.sphere]<-
  ifelse(!is.na(data$A18[cone.half.sphere])&!is.na(data$B18[cone.half.sphere]),
         1/12*pi*data$A18[cone.half.sphere]**2*(data$B18[cone.half.sphere]+.5*data$A18[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A18[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A18[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A18[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell18[double.cone]<-
  ifelse(!is.na(data$A18[double.cone])&!is.na(data$B18[double.cone]),
         1/12*pi*data$A18[double.cone]**2*data$B18[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A18[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A18[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell18[pyramid]<-
  ifelse(!is.na(data$A18[pyramid])&!is.na(data$B18[pyramid])&!is.na(data$C18[pyramid]),
         1/3*data$A18[pyramid]*data$B18[pyramid]*data$C18[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A18[pyramid])&!is.na(data$B18[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A18[pyramid]*data$B18[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A18[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C18[pyramid]),
                       1/3*data$A18[pyramid]*data$DimB[pyramid]*data$C18[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A18[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A18[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell18[cuboid]<-
  ifelse(!is.na(data$A18[cuboid])&!is.na(data$B18[cuboid])&!is.na(data$C18[cuboid]),
         data$A18[cuboid]*data$B18[cuboid]*data$C18[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A18[cuboid])&!is.na(data$B18[cuboid])&!is.na(data$DimC[cuboid]),
                data$A18[cuboid]*data$B18[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A18[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C18[cuboid]),
                       data$A18[cuboid]*data$DimB[cuboid]*data$C18[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A18[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A18[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell18[lanceol.cyl]<-
  ifelse(!is.na(data$A18[lanceol.cyl])&!is.na(data$B18[lanceol.cyl])&!is.na(data$C18[lanceol.cyl]),
         2/pi*data$A18[lanceol.cyl]*data$B18[lanceol.cyl]*data$C18[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A18[lanceol.cyl])&!is.na(data$B18[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A18[lanceol.cyl]*data$B18[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A18[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C18[lanceol.cyl]),
                       2/pi*data$A18[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C18[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A18[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A18[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell18[cuboid1]<-
  ifelse(!is.na(data$A18[cuboid1])&!is.na(data$B18[cuboid1]),
         data$A18[cuboid1]*data$B18[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A18[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A18[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell19[spindle15]<-ifelse(!is.na(data$A19[spindle15])&!is.na(data$B19[spindle15]),
                               2/15*pi*data$A19[spindle15]**2*data$B19[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A19[spindle15]),
                                      2/15*pi*data$A19[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell19[spindle11]<-
  ifelse(!is.na(data$A19[spindle11])&!is.na(data$B19[spindle11])&!is.na(data$C19[spindle11]),
         2/15*pi*data$A19[spindle11]*data$B19[spindle11]*data$C19[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A19[spindle11])&!is.na(data$B19[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A19[spindle11]*data$B19[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A19[spindle11])&!is.na(data$B19[spindle11]),
                       2/15*pi*data$A19[spindle11]*data$B19[spindle11]*data$B19[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A19[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A19[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell19[rhomb]<-
  ifelse(!is.na(data$A19[rhomb])&!is.na(data$B19[rhomb])&!is.na(data$C19[rhomb]),
         1/2*data$A19[rhomb]*data$B19[rhomb]*data$C19[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A19[rhomb])&!is.na(data$B19[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A19[rhomb]*data$B19[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A19[rhomb])&!is.na(data$B19[rhomb]),
                       1/2*data$A19[rhomb]*data$B19[rhomb]*.75*data$B19[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A19[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A19[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell19[cylinder]<-ifelse(!is.na(data$A19[cylinder])&!is.na(data$B19[cylinder]),
                              1/4*pi*data$A19[cylinder]**2*data$B19[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A19[cylinder]),
                                     1/4*pi*data$A19[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell19[elliptic.cyl]<-
  ifelse(!is.na(data$A19[elliptic.cyl])&!is.na(data$B19[elliptic.cyl])&!is.na(data$C19[elliptic.cyl]),
         1/4*pi*data$A19[elliptic.cyl]*data$B19[elliptic.cyl]*data$C19[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A19[elliptic.cyl])&!is.na(data$B19[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A19[elliptic.cyl]*data$B19[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A19[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C19[elliptic.cyl]),
                       1/4*pi*data$A19[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C19[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A19[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A19[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell19[triang.prism]<-
  ifelse(!is.na(data$A19[triang.prism])&!is.na(data$B19[triang.prism])&!is.na(data$C19[triang.prism]),
         1/2*data$A19[triang.prism]*data$B19[triang.prism]*data$C19[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A19[triang.prism])&!is.na(data$B19[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A19[triang.prism]*data$B19[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A19[triang.prism])&!is.na(data$B19[triang.prism]),
                       1/2*data$A19[triang.prism]*data$B19[triang.prism]*.75*data$B19[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A19[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A19[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell19[sphere]<-
  ifelse(!is.na(data$A19[sphere]),
         1/6*pi*data$A19[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell19[spheroid]<-
  ifelse(!is.na(data$A19[spheroid])&!is.na(data$B19[spheroid]),
         1/6*pi*data$A19[spheroid]*data$B19[spheroid]*data$A19[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A19[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A19[spheroid]*data$DimB[spheroid]*data$A19[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell19[ellipsoid]<-
  ifelse(!is.na(data$A19[ellipsoid])&!is.na(data$B19[ellipsoid])&!is.na(data$C19[ellipsoid]),
         1/6*pi*data$A19[ellipsoid]*data$B19[ellipsoid]*data$C19[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A19[ellipsoid])&!is.na(data$B19[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A19[ellipsoid]*data$B19[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A19[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C19[ellipsoid]),
                       1/6*pi*data$A19[ellipsoid]*data$DimB[ellipsoid]*data$C19[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A19[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A19[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell19[cone]<-
  ifelse(!is.na(data$A19[cone])&!is.na(data$B19[cone]),
         1/12*pi*data$A19[cone]**2*(sqrt(data$B19[cone]**2-.25*data$A19[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A19[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A19[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A19[cone]**2))*data$corr.fac[cone],
                NA))

data$cell19[cone.half.sphere]<-
  ifelse(!is.na(data$A19[cone.half.sphere])&!is.na(data$B19[cone.half.sphere]),
         1/12*pi*data$A19[cone.half.sphere]**2*(data$B19[cone.half.sphere]+.5*data$A19[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A19[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A19[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A19[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell19[double.cone]<-
  ifelse(!is.na(data$A19[double.cone])&!is.na(data$B19[double.cone]),
         1/12*pi*data$A19[double.cone]**2*data$B19[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A19[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A19[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell19[pyramid]<-
  ifelse(!is.na(data$A19[pyramid])&!is.na(data$B19[pyramid])&!is.na(data$C19[pyramid]),
         1/3*data$A19[pyramid]*data$B19[pyramid]*data$C19[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A19[pyramid])&!is.na(data$B19[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A19[pyramid]*data$B19[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A19[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C19[pyramid]),
                       1/3*data$A19[pyramid]*data$DimB[pyramid]*data$C19[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A19[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A19[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell19[cuboid]<-
  ifelse(!is.na(data$A19[cuboid])&!is.na(data$B19[cuboid])&!is.na(data$C19[cuboid]),
         data$A19[cuboid]*data$B19[cuboid]*data$C19[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A19[cuboid])&!is.na(data$B19[cuboid])&!is.na(data$DimC[cuboid]),
                data$A19[cuboid]*data$B19[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A19[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C19[cuboid]),
                       data$A19[cuboid]*data$DimB[cuboid]*data$C19[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A19[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A19[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell19[lanceol.cyl]<-
  ifelse(!is.na(data$A19[lanceol.cyl])&!is.na(data$B19[lanceol.cyl])&!is.na(data$C19[lanceol.cyl]),
         2/pi*data$A19[lanceol.cyl]*data$B19[lanceol.cyl]*data$C19[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A19[lanceol.cyl])&!is.na(data$B19[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A19[lanceol.cyl]*data$B19[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A19[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C19[lanceol.cyl]),
                       2/pi*data$A19[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C19[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A19[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A19[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell19[cuboid1]<-
  ifelse(!is.na(data$A19[cuboid1])&!is.na(data$B19[cuboid1]),
         data$A19[cuboid1]*data$B19[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A19[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A19[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))

#A,B,C,Cell
#Cell1

data$cell20[spindle15]<-ifelse(!is.na(data$A20[spindle15])&!is.na(data$B20[spindle15]),
                               2/15*pi*data$A20[spindle15]**2*data$B20[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A20[spindle15]),
                                      2/15*pi*data$A20[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell20[spindle11]<-
  ifelse(!is.na(data$A20[spindle11])&!is.na(data$B20[spindle11])&!is.na(data$C20[spindle11]),
         2/15*pi*data$A20[spindle11]*data$B20[spindle11]*data$C20[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A20[spindle11])&!is.na(data$B20[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A20[spindle11]*data$B20[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A20[spindle11])&!is.na(data$B20[spindle11]),
                       2/15*pi*data$A20[spindle11]*data$B20[spindle11]*data$B20[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A20[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A20[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell20[rhomb]<-
  ifelse(!is.na(data$A20[rhomb])&!is.na(data$B20[rhomb])&!is.na(data$C20[rhomb]),
         1/2*data$A20[rhomb]*data$B20[rhomb]*data$C20[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A20[rhomb])&!is.na(data$B20[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A20[rhomb]*data$B20[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A20[rhomb])&!is.na(data$B20[rhomb]),
                       1/2*data$A20[rhomb]*data$B20[rhomb]*.75*data$B20[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A20[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A20[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell20[cylinder]<-ifelse(!is.na(data$A20[cylinder])&!is.na(data$B20[cylinder]),
                              1/4*pi*data$A20[cylinder]**2*data$B20[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A20[cylinder]),
                                     1/4*pi*data$A20[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell20[elliptic.cyl]<-
  ifelse(!is.na(data$A20[elliptic.cyl])&!is.na(data$B20[elliptic.cyl])&!is.na(data$C20[elliptic.cyl]),
         1/4*pi*data$A20[elliptic.cyl]*data$B20[elliptic.cyl]*data$C20[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A20[elliptic.cyl])&!is.na(data$B20[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A20[elliptic.cyl]*data$B20[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A20[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C20[elliptic.cyl]),
                       1/4*pi*data$A20[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C20[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A20[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A20[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell20[triang.prism]<-
  ifelse(!is.na(data$A20[triang.prism])&!is.na(data$B20[triang.prism])&!is.na(data$C20[triang.prism]),
         1/2*data$A20[triang.prism]*data$B20[triang.prism]*data$C20[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A20[triang.prism])&!is.na(data$B20[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A20[triang.prism]*data$B20[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A20[triang.prism])&!is.na(data$B20[triang.prism]),
                       1/2*data$A20[triang.prism]*data$B20[triang.prism]*.75*data$B20[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A20[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A20[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell20[sphere]<-
  ifelse(!is.na(data$A20[sphere]),
         1/6*pi*data$A20[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell20[spheroid]<-
  ifelse(!is.na(data$A20[spheroid])&!is.na(data$B20[spheroid]),
         1/6*pi*data$A20[spheroid]*data$B20[spheroid]*data$A20[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A20[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A20[spheroid]*data$DimB[spheroid]*data$A20[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell20[ellipsoid]<-
  ifelse(!is.na(data$A20[ellipsoid])&!is.na(data$B20[ellipsoid])&!is.na(data$C20[ellipsoid]),
         1/6*pi*data$A20[ellipsoid]*data$B20[ellipsoid]*data$C20[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A20[ellipsoid])&!is.na(data$B20[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A20[ellipsoid]*data$B20[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A20[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C20[ellipsoid]),
                       1/6*pi*data$A20[ellipsoid]*data$DimB[ellipsoid]*data$C20[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A20[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A20[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell20[cone]<-
  ifelse(!is.na(data$A20[cone])&!is.na(data$B20[cone]),
         1/12*pi*data$A20[cone]**2*(sqrt(data$B20[cone]**2-.25*data$A20[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A20[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A20[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A20[cone]**2))*data$corr.fac[cone],
                NA))

data$cell20[cone.half.sphere]<-
  ifelse(!is.na(data$A20[cone.half.sphere])&!is.na(data$B20[cone.half.sphere]),
         1/12*pi*data$A20[cone.half.sphere]**2*(data$B20[cone.half.sphere]+.5*data$A20[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A20[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A20[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A20[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell20[double.cone]<-
  ifelse(!is.na(data$A20[double.cone])&!is.na(data$B20[double.cone]),
         1/12*pi*data$A20[double.cone]**2*data$B20[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A20[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A20[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell20[pyramid]<-
  ifelse(!is.na(data$A20[pyramid])&!is.na(data$B20[pyramid])&!is.na(data$C20[pyramid]),
         1/3*data$A20[pyramid]*data$B20[pyramid]*data$C20[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A20[pyramid])&!is.na(data$B20[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A20[pyramid]*data$B20[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A20[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C20[pyramid]),
                       1/3*data$A20[pyramid]*data$DimB[pyramid]*data$C20[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A20[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A20[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell20[cuboid]<-
  ifelse(!is.na(data$A20[cuboid])&!is.na(data$B20[cuboid])&!is.na(data$C20[cuboid]),
         data$A20[cuboid]*data$B20[cuboid]*data$C20[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A20[cuboid])&!is.na(data$B20[cuboid])&!is.na(data$DimC[cuboid]),
                data$A20[cuboid]*data$B20[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A20[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C20[cuboid]),
                       data$A20[cuboid]*data$DimB[cuboid]*data$C20[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A20[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A20[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell20[lanceol.cyl]<-
  ifelse(!is.na(data$A20[lanceol.cyl])&!is.na(data$B20[lanceol.cyl])&!is.na(data$C20[lanceol.cyl]),
         2/pi*data$A20[lanceol.cyl]*data$B20[lanceol.cyl]*data$C20[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A20[lanceol.cyl])&!is.na(data$B20[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A20[lanceol.cyl]*data$B20[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A20[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C20[lanceol.cyl]),
                       2/pi*data$A20[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C20[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A20[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A20[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell20[cuboid1]<-
  ifelse(!is.na(data$A20[cuboid1])&!is.na(data$B20[cuboid1]),
         data$A20[cuboid1]*data$B20[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A20[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A20[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell21[spindle15]<-ifelse(!is.na(data$A21[spindle15])&!is.na(data$B21[spindle15]),
                               2/15*pi*data$A21[spindle15]**2*data$B21[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A21[spindle15]),
                                      2/15*pi*data$A21[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell21[spindle11]<-
  ifelse(!is.na(data$A21[spindle11])&!is.na(data$B21[spindle11])&!is.na(data$C21[spindle11]),
         2/15*pi*data$A21[spindle11]*data$B21[spindle11]*data$C21[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A21[spindle11])&!is.na(data$B21[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A21[spindle11]*data$B21[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A21[spindle11])&!is.na(data$B21[spindle11]),
                       2/15*pi*data$A21[spindle11]*data$B21[spindle11]*data$B21[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A21[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A21[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell21[rhomb]<-
  ifelse(!is.na(data$A21[rhomb])&!is.na(data$B21[rhomb])&!is.na(data$C21[rhomb]),
         1/2*data$A21[rhomb]*data$B21[rhomb]*data$C21[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A21[rhomb])&!is.na(data$B21[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A21[rhomb]*data$B21[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A21[rhomb])&!is.na(data$B21[rhomb]),
                       1/2*data$A21[rhomb]*data$B21[rhomb]*.75*data$B21[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A21[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A21[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell21[cylinder]<-ifelse(!is.na(data$A21[cylinder])&!is.na(data$B21[cylinder]),
                              1/4*pi*data$A21[cylinder]**2*data$B21[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A21[cylinder]),
                                     1/4*pi*data$A21[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell21[elliptic.cyl]<-
  ifelse(!is.na(data$A21[elliptic.cyl])&!is.na(data$B21[elliptic.cyl])&!is.na(data$C21[elliptic.cyl]),
         1/4*pi*data$A21[elliptic.cyl]*data$B21[elliptic.cyl]*data$C21[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A21[elliptic.cyl])&!is.na(data$B21[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A21[elliptic.cyl]*data$B21[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A21[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C21[elliptic.cyl]),
                       1/4*pi*data$A21[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C21[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A21[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A21[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell21[triang.prism]<-
  ifelse(!is.na(data$A21[triang.prism])&!is.na(data$B21[triang.prism])&!is.na(data$C21[triang.prism]),
         1/2*data$A21[triang.prism]*data$B21[triang.prism]*data$C21[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A21[triang.prism])&!is.na(data$B21[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A21[triang.prism]*data$B21[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A21[triang.prism])&!is.na(data$B21[triang.prism]),
                       1/2*data$A21[triang.prism]*data$B21[triang.prism]*.75*data$B21[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A21[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A21[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell21[sphere]<-
  ifelse(!is.na(data$A21[sphere]),
         1/6*pi*data$A21[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell21[spheroid]<-
  ifelse(!is.na(data$A21[spheroid])&!is.na(data$B21[spheroid]),
         1/6*pi*data$A21[spheroid]*data$B21[spheroid]*data$A21[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A21[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A21[spheroid]*data$DimB[spheroid]*data$A21[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell21[ellipsoid]<-
  ifelse(!is.na(data$A21[ellipsoid])&!is.na(data$B21[ellipsoid])&!is.na(data$C21[ellipsoid]),
         1/6*pi*data$A21[ellipsoid]*data$B21[ellipsoid]*data$C21[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A21[ellipsoid])&!is.na(data$B21[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A21[ellipsoid]*data$B21[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A21[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C21[ellipsoid]),
                       1/6*pi*data$A21[ellipsoid]*data$DimB[ellipsoid]*data$C21[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A21[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A21[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell21[cone]<-
  ifelse(!is.na(data$A21[cone])&!is.na(data$B21[cone]),
         1/12*pi*data$A21[cone]**2*(sqrt(data$B21[cone]**2-.25*data$A21[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A21[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A21[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A21[cone]**2))*data$corr.fac[cone],
                NA))

data$cell21[cone.half.sphere]<-
  ifelse(!is.na(data$A21[cone.half.sphere])&!is.na(data$B21[cone.half.sphere]),
         1/12*pi*data$A21[cone.half.sphere]**2*(data$B21[cone.half.sphere]+.5*data$A21[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A21[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A21[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A21[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell21[double.cone]<-
  ifelse(!is.na(data$A21[double.cone])&!is.na(data$B21[double.cone]),
         1/12*pi*data$A21[double.cone]**2*data$B21[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A21[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A21[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell21[pyramid]<-
  ifelse(!is.na(data$A21[pyramid])&!is.na(data$B21[pyramid])&!is.na(data$C21[pyramid]),
         1/3*data$A21[pyramid]*data$B21[pyramid]*data$C21[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A21[pyramid])&!is.na(data$B21[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A21[pyramid]*data$B21[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A21[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C21[pyramid]),
                       1/3*data$A21[pyramid]*data$DimB[pyramid]*data$C21[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A21[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A21[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell21[cuboid]<-
  ifelse(!is.na(data$A21[cuboid])&!is.na(data$B21[cuboid])&!is.na(data$C21[cuboid]),
         data$A21[cuboid]*data$B21[cuboid]*data$C21[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A21[cuboid])&!is.na(data$B21[cuboid])&!is.na(data$DimC[cuboid]),
                data$A21[cuboid]*data$B21[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A21[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C21[cuboid]),
                       data$A21[cuboid]*data$DimB[cuboid]*data$C21[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A21[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A21[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell21[lanceol.cyl]<-
  ifelse(!is.na(data$A21[lanceol.cyl])&!is.na(data$B21[lanceol.cyl])&!is.na(data$C21[lanceol.cyl]),
         2/pi*data$A21[lanceol.cyl]*data$B21[lanceol.cyl]*data$C21[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A21[lanceol.cyl])&!is.na(data$B21[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A21[lanceol.cyl]*data$B21[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A21[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C21[lanceol.cyl]),
                       2/pi*data$A21[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C21[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A21[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A21[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell21[cuboid1]<-
  ifelse(!is.na(data$A21[cuboid1])&!is.na(data$B21[cuboid1]),
         data$A21[cuboid1]*data$B21[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A21[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A21[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell22[spindle15]<-ifelse(!is.na(data$A22[spindle15])&!is.na(data$B22[spindle15]),
                               2/15*pi*data$A22[spindle15]**2*data$B22[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A22[spindle15]),
                                      2/15*pi*data$A22[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell22[spindle11]<-
  ifelse(!is.na(data$A22[spindle11])&!is.na(data$B22[spindle11])&!is.na(data$C22[spindle11]),
         2/15*pi*data$A22[spindle11]*data$B22[spindle11]*data$C22[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A22[spindle11])&!is.na(data$B22[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A22[spindle11]*data$B22[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A22[spindle11])&!is.na(data$B22[spindle11]),
                       2/15*pi*data$A22[spindle11]*data$B22[spindle11]*data$B22[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A22[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A22[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell22[rhomb]<-
  ifelse(!is.na(data$A22[rhomb])&!is.na(data$B22[rhomb])&!is.na(data$C22[rhomb]),
         1/2*data$A22[rhomb]*data$B22[rhomb]*data$C22[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A22[rhomb])&!is.na(data$B22[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A22[rhomb]*data$B22[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A22[rhomb])&!is.na(data$B22[rhomb]),
                       1/2*data$A22[rhomb]*data$B22[rhomb]*.75*data$B22[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A22[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A22[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell22[cylinder]<-ifelse(!is.na(data$A22[cylinder])&!is.na(data$B22[cylinder]),
                              1/4*pi*data$A22[cylinder]**2*data$B22[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A22[cylinder]),
                                     1/4*pi*data$A22[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell22[elliptic.cyl]<-
  ifelse(!is.na(data$A22[elliptic.cyl])&!is.na(data$B22[elliptic.cyl])&!is.na(data$C22[elliptic.cyl]),
         1/4*pi*data$A22[elliptic.cyl]*data$B22[elliptic.cyl]*data$C22[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A22[elliptic.cyl])&!is.na(data$B22[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A22[elliptic.cyl]*data$B22[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A22[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C22[elliptic.cyl]),
                       1/4*pi*data$A22[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C22[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A22[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A22[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell22[triang.prism]<-
  ifelse(!is.na(data$A22[triang.prism])&!is.na(data$B22[triang.prism])&!is.na(data$C22[triang.prism]),
         1/2*data$A22[triang.prism]*data$B22[triang.prism]*data$C22[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A22[triang.prism])&!is.na(data$B22[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A22[triang.prism]*data$B22[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A22[triang.prism])&!is.na(data$B22[triang.prism]),
                       1/2*data$A22[triang.prism]*data$B22[triang.prism]*.75*data$B22[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A22[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A22[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell22[sphere]<-
  ifelse(!is.na(data$A22[sphere]),
         1/6*pi*data$A22[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell22[spheroid]<-
  ifelse(!is.na(data$A22[spheroid])&!is.na(data$B22[spheroid]),
         1/6*pi*data$A22[spheroid]*data$B22[spheroid]*data$A22[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A22[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A22[spheroid]*data$DimB[spheroid]*data$A22[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell22[ellipsoid]<-
  ifelse(!is.na(data$A22[ellipsoid])&!is.na(data$B22[ellipsoid])&!is.na(data$C22[ellipsoid]),
         1/6*pi*data$A22[ellipsoid]*data$B22[ellipsoid]*data$C22[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A22[ellipsoid])&!is.na(data$B22[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A22[ellipsoid]*data$B22[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A22[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C22[ellipsoid]),
                       1/6*pi*data$A22[ellipsoid]*data$DimB[ellipsoid]*data$C22[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A22[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A22[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell22[cone]<-
  ifelse(!is.na(data$A22[cone])&!is.na(data$B22[cone]),
         1/12*pi*data$A22[cone]**2*(sqrt(data$B22[cone]**2-.25*data$A22[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A22[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A22[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A22[cone]**2))*data$corr.fac[cone],
                NA))

data$cell22[cone.half.sphere]<-
  ifelse(!is.na(data$A22[cone.half.sphere])&!is.na(data$B22[cone.half.sphere]),
         1/12*pi*data$A22[cone.half.sphere]**2*(data$B22[cone.half.sphere]+.5*data$A22[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A22[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A22[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A22[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell22[double.cone]<-
  ifelse(!is.na(data$A22[double.cone])&!is.na(data$B22[double.cone]),
         1/12*pi*data$A22[double.cone]**2*data$B22[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A22[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A22[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell22[pyramid]<-
  ifelse(!is.na(data$A22[pyramid])&!is.na(data$B22[pyramid])&!is.na(data$C22[pyramid]),
         1/3*data$A22[pyramid]*data$B22[pyramid]*data$C22[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A22[pyramid])&!is.na(data$B22[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A22[pyramid]*data$B22[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A22[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C22[pyramid]),
                       1/3*data$A22[pyramid]*data$DimB[pyramid]*data$C22[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A22[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A22[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell22[cuboid]<-
  ifelse(!is.na(data$A22[cuboid])&!is.na(data$B22[cuboid])&!is.na(data$C22[cuboid]),
         data$A22[cuboid]*data$B22[cuboid]*data$C22[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A22[cuboid])&!is.na(data$B22[cuboid])&!is.na(data$DimC[cuboid]),
                data$A22[cuboid]*data$B22[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A22[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C22[cuboid]),
                       data$A22[cuboid]*data$DimB[cuboid]*data$C22[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A22[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A22[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell22[lanceol.cyl]<-
  ifelse(!is.na(data$A22[lanceol.cyl])&!is.na(data$B22[lanceol.cyl])&!is.na(data$C22[lanceol.cyl]),
         2/pi*data$A22[lanceol.cyl]*data$B22[lanceol.cyl]*data$C22[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A22[lanceol.cyl])&!is.na(data$B22[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A22[lanceol.cyl]*data$B22[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A22[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C22[lanceol.cyl]),
                       2/pi*data$A22[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C22[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A22[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A22[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell22[cuboid1]<-
  ifelse(!is.na(data$A22[cuboid1])&!is.na(data$B22[cuboid1]),
         data$A22[cuboid1]*data$B22[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A22[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A22[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell23[spindle15]<-ifelse(!is.na(data$A23[spindle15])&!is.na(data$B23[spindle15]),
                               2/15*pi*data$A23[spindle15]**2*data$B23[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A23[spindle15]),
                                      2/15*pi*data$A23[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell23[spindle11]<-
  ifelse(!is.na(data$A23[spindle11])&!is.na(data$B23[spindle11])&!is.na(data$C23[spindle11]),
         2/15*pi*data$A23[spindle11]*data$B23[spindle11]*data$C23[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A23[spindle11])&!is.na(data$B23[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A23[spindle11]*data$B23[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A23[spindle11])&!is.na(data$B23[spindle11]),
                       2/15*pi*data$A23[spindle11]*data$B23[spindle11]*data$B23[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A23[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A23[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell23[rhomb]<-
  ifelse(!is.na(data$A23[rhomb])&!is.na(data$B23[rhomb])&!is.na(data$C23[rhomb]),
         1/2*data$A23[rhomb]*data$B23[rhomb]*data$C23[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A23[rhomb])&!is.na(data$B23[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A23[rhomb]*data$B23[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A23[rhomb])&!is.na(data$B23[rhomb]),
                       1/2*data$A23[rhomb]*data$B23[rhomb]*.75*data$B23[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A23[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A23[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell23[cylinder]<-ifelse(!is.na(data$A23[cylinder])&!is.na(data$B23[cylinder]),
                              1/4*pi*data$A23[cylinder]**2*data$B23[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A23[cylinder]),
                                     1/4*pi*data$A23[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell23[elliptic.cyl]<-
  ifelse(!is.na(data$A23[elliptic.cyl])&!is.na(data$B23[elliptic.cyl])&!is.na(data$C23[elliptic.cyl]),
         1/4*pi*data$A23[elliptic.cyl]*data$B23[elliptic.cyl]*data$C23[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A23[elliptic.cyl])&!is.na(data$B23[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A23[elliptic.cyl]*data$B23[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A23[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C23[elliptic.cyl]),
                       1/4*pi*data$A23[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C23[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A23[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A23[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell23[triang.prism]<-
  ifelse(!is.na(data$A23[triang.prism])&!is.na(data$B23[triang.prism])&!is.na(data$C23[triang.prism]),
         1/2*data$A23[triang.prism]*data$B23[triang.prism]*data$C23[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A23[triang.prism])&!is.na(data$B23[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A23[triang.prism]*data$B23[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A23[triang.prism])&!is.na(data$B23[triang.prism]),
                       1/2*data$A23[triang.prism]*data$B23[triang.prism]*.75*data$B23[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A23[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A23[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell23[sphere]<-
  ifelse(!is.na(data$A23[sphere]),
         1/6*pi*data$A23[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell23[spheroid]<-
  ifelse(!is.na(data$A23[spheroid])&!is.na(data$B23[spheroid]),
         1/6*pi*data$A23[spheroid]*data$B23[spheroid]*data$A23[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A23[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A23[spheroid]*data$DimB[spheroid]*data$A23[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell23[ellipsoid]<-
  ifelse(!is.na(data$A23[ellipsoid])&!is.na(data$B23[ellipsoid])&!is.na(data$C23[ellipsoid]),
         1/6*pi*data$A23[ellipsoid]*data$B23[ellipsoid]*data$C23[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A23[ellipsoid])&!is.na(data$B23[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A23[ellipsoid]*data$B23[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A23[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C23[ellipsoid]),
                       1/6*pi*data$A23[ellipsoid]*data$DimB[ellipsoid]*data$C23[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A23[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A23[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell23[cone]<-
  ifelse(!is.na(data$A23[cone])&!is.na(data$B23[cone]),
         1/12*pi*data$A23[cone]**2*(sqrt(data$B23[cone]**2-.25*data$A23[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A23[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A23[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A23[cone]**2))*data$corr.fac[cone],
                NA))

data$cell23[cone.half.sphere]<-
  ifelse(!is.na(data$A23[cone.half.sphere])&!is.na(data$B23[cone.half.sphere]),
         1/12*pi*data$A23[cone.half.sphere]**2*(data$B23[cone.half.sphere]+.5*data$A23[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A23[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A23[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A23[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell23[double.cone]<-
  ifelse(!is.na(data$A23[double.cone])&!is.na(data$B23[double.cone]),
         1/12*pi*data$A23[double.cone]**2*data$B23[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A23[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A23[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell23[pyramid]<-
  ifelse(!is.na(data$A23[pyramid])&!is.na(data$B23[pyramid])&!is.na(data$C23[pyramid]),
         1/3*data$A23[pyramid]*data$B23[pyramid]*data$C23[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A23[pyramid])&!is.na(data$B23[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A23[pyramid]*data$B23[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A23[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C23[pyramid]),
                       1/3*data$A23[pyramid]*data$DimB[pyramid]*data$C23[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A23[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A23[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell23[cuboid]<-
  ifelse(!is.na(data$A23[cuboid])&!is.na(data$B23[cuboid])&!is.na(data$C23[cuboid]),
         data$A23[cuboid]*data$B23[cuboid]*data$C23[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A23[cuboid])&!is.na(data$B23[cuboid])&!is.na(data$DimC[cuboid]),
                data$A23[cuboid]*data$B23[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A23[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C23[cuboid]),
                       data$A23[cuboid]*data$DimB[cuboid]*data$C23[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A23[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A23[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell23[lanceol.cyl]<-
  ifelse(!is.na(data$A23[lanceol.cyl])&!is.na(data$B23[lanceol.cyl])&!is.na(data$C23[lanceol.cyl]),
         2/pi*data$A23[lanceol.cyl]*data$B23[lanceol.cyl]*data$C23[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A23[lanceol.cyl])&!is.na(data$B23[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A23[lanceol.cyl]*data$B23[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A23[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C23[lanceol.cyl]),
                       2/pi*data$A23[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C23[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A23[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A23[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell23[cuboid1]<-
  ifelse(!is.na(data$A23[cuboid1])&!is.na(data$B23[cuboid1]),
         data$A23[cuboid1]*data$B23[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A23[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A23[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))

#A,B,C,Cell
#Cell1

data$cell24[spindle15]<-ifelse(!is.na(data$A24[spindle15])&!is.na(data$B24[spindle15]),
                               2/15*pi*data$A24[spindle15]**2*data$B24[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A24[spindle15]),
                                      2/15*pi*data$A24[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell24[spindle11]<-
  ifelse(!is.na(data$A24[spindle11])&!is.na(data$B24[spindle11])&!is.na(data$C24[spindle11]),
         2/15*pi*data$A24[spindle11]*data$B24[spindle11]*data$C24[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A24[spindle11])&!is.na(data$B24[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A24[spindle11]*data$B24[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A24[spindle11])&!is.na(data$B24[spindle11]),
                       2/15*pi*data$A24[spindle11]*data$B24[spindle11]*data$B24[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A24[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A24[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell24[rhomb]<-
  ifelse(!is.na(data$A24[rhomb])&!is.na(data$B24[rhomb])&!is.na(data$C24[rhomb]),
         1/2*data$A24[rhomb]*data$B24[rhomb]*data$C24[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A24[rhomb])&!is.na(data$B24[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A24[rhomb]*data$B24[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A24[rhomb])&!is.na(data$B24[rhomb]),
                       1/2*data$A24[rhomb]*data$B24[rhomb]*.75*data$B24[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A24[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A24[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell24[cylinder]<-ifelse(!is.na(data$A24[cylinder])&!is.na(data$B24[cylinder]),
                              1/4*pi*data$A24[cylinder]**2*data$B24[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A24[cylinder]),
                                     1/4*pi*data$A24[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell24[elliptic.cyl]<-
  ifelse(!is.na(data$A24[elliptic.cyl])&!is.na(data$B24[elliptic.cyl])&!is.na(data$C24[elliptic.cyl]),
         1/4*pi*data$A24[elliptic.cyl]*data$B24[elliptic.cyl]*data$C24[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A24[elliptic.cyl])&!is.na(data$B24[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A24[elliptic.cyl]*data$B24[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A24[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C24[elliptic.cyl]),
                       1/4*pi*data$A24[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C24[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A24[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A24[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell24[triang.prism]<-
  ifelse(!is.na(data$A24[triang.prism])&!is.na(data$B24[triang.prism])&!is.na(data$C24[triang.prism]),
         1/2*data$A24[triang.prism]*data$B24[triang.prism]*data$C24[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A24[triang.prism])&!is.na(data$B24[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A24[triang.prism]*data$B24[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A24[triang.prism])&!is.na(data$B24[triang.prism]),
                       1/2*data$A24[triang.prism]*data$B24[triang.prism]*.75*data$B24[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A24[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A24[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell24[sphere]<-
  ifelse(!is.na(data$A24[sphere]),
         1/6*pi*data$A24[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell24[spheroid]<-
  ifelse(!is.na(data$A24[spheroid])&!is.na(data$B24[spheroid]),
         1/6*pi*data$A24[spheroid]*data$B24[spheroid]*data$A24[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A24[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A24[spheroid]*data$DimB[spheroid]*data$A24[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell24[ellipsoid]<-
  ifelse(!is.na(data$A24[ellipsoid])&!is.na(data$B24[ellipsoid])&!is.na(data$C24[ellipsoid]),
         1/6*pi*data$A24[ellipsoid]*data$B24[ellipsoid]*data$C24[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A24[ellipsoid])&!is.na(data$B24[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A24[ellipsoid]*data$B24[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A24[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C24[ellipsoid]),
                       1/6*pi*data$A24[ellipsoid]*data$DimB[ellipsoid]*data$C24[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A24[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A24[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell24[cone]<-
  ifelse(!is.na(data$A24[cone])&!is.na(data$B24[cone]),
         1/12*pi*data$A24[cone]**2*(sqrt(data$B24[cone]**2-.25*data$A24[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A24[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A24[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A24[cone]**2))*data$corr.fac[cone],
                NA))

data$cell24[cone.half.sphere]<-
  ifelse(!is.na(data$A24[cone.half.sphere])&!is.na(data$B24[cone.half.sphere]),
         1/12*pi*data$A24[cone.half.sphere]**2*(data$B24[cone.half.sphere]+.5*data$A24[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A24[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A24[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A24[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell24[double.cone]<-
  ifelse(!is.na(data$A24[double.cone])&!is.na(data$B24[double.cone]),
         1/12*pi*data$A24[double.cone]**2*data$B24[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A24[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A24[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell24[pyramid]<-
  ifelse(!is.na(data$A24[pyramid])&!is.na(data$B24[pyramid])&!is.na(data$C24[pyramid]),
         1/3*data$A24[pyramid]*data$B24[pyramid]*data$C24[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A24[pyramid])&!is.na(data$B24[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A24[pyramid]*data$B24[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A24[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C24[pyramid]),
                       1/3*data$A24[pyramid]*data$DimB[pyramid]*data$C24[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A24[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A24[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell24[cuboid]<-
  ifelse(!is.na(data$A24[cuboid])&!is.na(data$B24[cuboid])&!is.na(data$C24[cuboid]),
         data$A24[cuboid]*data$B24[cuboid]*data$C24[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A24[cuboid])&!is.na(data$B24[cuboid])&!is.na(data$DimC[cuboid]),
                data$A24[cuboid]*data$B24[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A24[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C24[cuboid]),
                       data$A24[cuboid]*data$DimB[cuboid]*data$C24[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A24[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A24[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell24[lanceol.cyl]<-
  ifelse(!is.na(data$A24[lanceol.cyl])&!is.na(data$B24[lanceol.cyl])&!is.na(data$C24[lanceol.cyl]),
         2/pi*data$A24[lanceol.cyl]*data$B24[lanceol.cyl]*data$C24[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A24[lanceol.cyl])&!is.na(data$B24[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A24[lanceol.cyl]*data$B24[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A24[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C24[lanceol.cyl]),
                       2/pi*data$A24[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C24[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A24[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A24[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell24[cuboid1]<-
  ifelse(!is.na(data$A24[cuboid1])&!is.na(data$B24[cuboid1]),
         data$A24[cuboid1]*data$B24[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A24[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A24[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))



#A,B,C,Cell
#Cell1

data$cell25[spindle15]<-ifelse(!is.na(data$A25[spindle15])&!is.na(data$B25[spindle15]),
                               2/15*pi*data$A25[spindle15]**2*data$B25[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A25[spindle15]),
                                      2/15*pi*data$A25[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell25[spindle11]<-
  ifelse(!is.na(data$A25[spindle11])&!is.na(data$B25[spindle11])&!is.na(data$C25[spindle11]),
         2/15*pi*data$A25[spindle11]*data$B25[spindle11]*data$C25[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A25[spindle11])&!is.na(data$B25[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A25[spindle11]*data$B25[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A25[spindle11])&!is.na(data$B25[spindle11]),
                       2/15*pi*data$A25[spindle11]*data$B25[spindle11]*data$B25[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A25[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A25[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell25[rhomb]<-
  ifelse(!is.na(data$A25[rhomb])&!is.na(data$B25[rhomb])&!is.na(data$C25[rhomb]),
         1/2*data$A25[rhomb]*data$B25[rhomb]*data$C25[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A25[rhomb])&!is.na(data$B25[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A25[rhomb]*data$B25[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A25[rhomb])&!is.na(data$B25[rhomb]),
                       1/2*data$A25[rhomb]*data$B25[rhomb]*.75*data$B25[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A25[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A25[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell25[cylinder]<-ifelse(!is.na(data$A25[cylinder])&!is.na(data$B25[cylinder]),
                              1/4*pi*data$A25[cylinder]**2*data$B25[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A25[cylinder]),
                                     1/4*pi*data$A25[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell25[elliptic.cyl]<-
  ifelse(!is.na(data$A25[elliptic.cyl])&!is.na(data$B25[elliptic.cyl])&!is.na(data$C25[elliptic.cyl]),
         1/4*pi*data$A25[elliptic.cyl]*data$B25[elliptic.cyl]*data$C25[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A25[elliptic.cyl])&!is.na(data$B25[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A25[elliptic.cyl]*data$B25[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A25[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C25[elliptic.cyl]),
                       1/4*pi*data$A25[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C25[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A25[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A25[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell25[triang.prism]<-
  ifelse(!is.na(data$A25[triang.prism])&!is.na(data$B25[triang.prism])&!is.na(data$C25[triang.prism]),
         1/2*data$A25[triang.prism]*data$B25[triang.prism]*data$C25[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A25[triang.prism])&!is.na(data$B25[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A25[triang.prism]*data$B25[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A25[triang.prism])&!is.na(data$B25[triang.prism]),
                       1/2*data$A25[triang.prism]*data$B25[triang.prism]*.75*data$B25[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A25[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A25[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell25[sphere]<-
  ifelse(!is.na(data$A25[sphere]),
         1/6*pi*data$A25[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell25[spheroid]<-
  ifelse(!is.na(data$A25[spheroid])&!is.na(data$B25[spheroid]),
         1/6*pi*data$A25[spheroid]*data$B25[spheroid]*data$A25[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A25[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A25[spheroid]*data$DimB[spheroid]*data$A25[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell25[ellipsoid]<-
  ifelse(!is.na(data$A25[ellipsoid])&!is.na(data$B25[ellipsoid])&!is.na(data$C25[ellipsoid]),
         1/6*pi*data$A25[ellipsoid]*data$B25[ellipsoid]*data$C25[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A25[ellipsoid])&!is.na(data$B25[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A25[ellipsoid]*data$B25[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A25[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C25[ellipsoid]),
                       1/6*pi*data$A25[ellipsoid]*data$DimB[ellipsoid]*data$C25[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A25[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A25[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell25[cone]<-
  ifelse(!is.na(data$A25[cone])&!is.na(data$B25[cone]),
         1/12*pi*data$A25[cone]**2*(sqrt(data$B25[cone]**2-.25*data$A25[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A25[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A25[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A25[cone]**2))*data$corr.fac[cone],
                NA))

data$cell25[cone.half.sphere]<-
  ifelse(!is.na(data$A25[cone.half.sphere])&!is.na(data$B25[cone.half.sphere]),
         1/12*pi*data$A25[cone.half.sphere]**2*(data$B25[cone.half.sphere]+.5*data$A25[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A25[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A25[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A25[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell25[double.cone]<-
  ifelse(!is.na(data$A25[double.cone])&!is.na(data$B25[double.cone]),
         1/12*pi*data$A25[double.cone]**2*data$B25[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A25[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A25[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell25[pyramid]<-
  ifelse(!is.na(data$A25[pyramid])&!is.na(data$B25[pyramid])&!is.na(data$C25[pyramid]),
         1/3*data$A25[pyramid]*data$B25[pyramid]*data$C25[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A25[pyramid])&!is.na(data$B25[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A25[pyramid]*data$B25[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A25[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C25[pyramid]),
                       1/3*data$A25[pyramid]*data$DimB[pyramid]*data$C25[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A25[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A25[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell25[cuboid]<-
  ifelse(!is.na(data$A25[cuboid])&!is.na(data$B25[cuboid])&!is.na(data$C25[cuboid]),
         data$A25[cuboid]*data$B25[cuboid]*data$C25[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A25[cuboid])&!is.na(data$B25[cuboid])&!is.na(data$DimC[cuboid]),
                data$A25[cuboid]*data$B25[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A25[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C25[cuboid]),
                       data$A25[cuboid]*data$DimB[cuboid]*data$C25[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A25[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A25[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell25[lanceol.cyl]<-
  ifelse(!is.na(data$A25[lanceol.cyl])&!is.na(data$B25[lanceol.cyl])&!is.na(data$C25[lanceol.cyl]),
         2/pi*data$A25[lanceol.cyl]*data$B25[lanceol.cyl]*data$C25[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A25[lanceol.cyl])&!is.na(data$B25[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A25[lanceol.cyl]*data$B25[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A25[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C25[lanceol.cyl]),
                       2/pi*data$A25[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C25[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A25[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A25[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell25[cuboid1]<-
  ifelse(!is.na(data$A25[cuboid1])&!is.na(data$B25[cuboid1]),
         data$A25[cuboid1]*data$B25[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A25[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A25[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell26[spindle15]<-ifelse(!is.na(data$A26[spindle15])&!is.na(data$B26[spindle15]),
                               2/15*pi*data$A26[spindle15]**2*data$B26[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A26[spindle15]),
                                      2/15*pi*data$A26[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell26[spindle11]<-
  ifelse(!is.na(data$A26[spindle11])&!is.na(data$B26[spindle11])&!is.na(data$C26[spindle11]),
         2/15*pi*data$A26[spindle11]*data$B26[spindle11]*data$C26[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A26[spindle11])&!is.na(data$B26[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A26[spindle11]*data$B26[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A26[spindle11])&!is.na(data$B26[spindle11]),
                       2/15*pi*data$A26[spindle11]*data$B26[spindle11]*data$B26[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A26[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A26[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell26[rhomb]<-
  ifelse(!is.na(data$A26[rhomb])&!is.na(data$B26[rhomb])&!is.na(data$C26[rhomb]),
         1/2*data$A26[rhomb]*data$B26[rhomb]*data$C26[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A26[rhomb])&!is.na(data$B26[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A26[rhomb]*data$B26[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A26[rhomb])&!is.na(data$B26[rhomb]),
                       1/2*data$A26[rhomb]*data$B26[rhomb]*.75*data$B26[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A26[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A26[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell26[cylinder]<-ifelse(!is.na(data$A26[cylinder])&!is.na(data$B26[cylinder]),
                              1/4*pi*data$A26[cylinder]**2*data$B26[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A26[cylinder]),
                                     1/4*pi*data$A26[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell26[elliptic.cyl]<-
  ifelse(!is.na(data$A26[elliptic.cyl])&!is.na(data$B26[elliptic.cyl])&!is.na(data$C26[elliptic.cyl]),
         1/4*pi*data$A26[elliptic.cyl]*data$B26[elliptic.cyl]*data$C26[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A26[elliptic.cyl])&!is.na(data$B26[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A26[elliptic.cyl]*data$B26[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A26[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C26[elliptic.cyl]),
                       1/4*pi*data$A26[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C26[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A26[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A26[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell26[triang.prism]<-
  ifelse(!is.na(data$A26[triang.prism])&!is.na(data$B26[triang.prism])&!is.na(data$C26[triang.prism]),
         1/2*data$A26[triang.prism]*data$B26[triang.prism]*data$C26[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A26[triang.prism])&!is.na(data$B26[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A26[triang.prism]*data$B26[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A26[triang.prism])&!is.na(data$B26[triang.prism]),
                       1/2*data$A26[triang.prism]*data$B26[triang.prism]*.75*data$B26[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A26[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A26[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell26[sphere]<-
  ifelse(!is.na(data$A26[sphere]),
         1/6*pi*data$A26[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell26[spheroid]<-
  ifelse(!is.na(data$A26[spheroid])&!is.na(data$B26[spheroid]),
         1/6*pi*data$A26[spheroid]*data$B26[spheroid]*data$A26[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A26[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A26[spheroid]*data$DimB[spheroid]*data$A26[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell26[ellipsoid]<-
  ifelse(!is.na(data$A26[ellipsoid])&!is.na(data$B26[ellipsoid])&!is.na(data$C26[ellipsoid]),
         1/6*pi*data$A26[ellipsoid]*data$B26[ellipsoid]*data$C26[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A26[ellipsoid])&!is.na(data$B26[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A26[ellipsoid]*data$B26[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A26[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C26[ellipsoid]),
                       1/6*pi*data$A26[ellipsoid]*data$DimB[ellipsoid]*data$C26[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A26[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A26[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell26[cone]<-
  ifelse(!is.na(data$A26[cone])&!is.na(data$B26[cone]),
         1/12*pi*data$A26[cone]**2*(sqrt(data$B26[cone]**2-.25*data$A26[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A26[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A26[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A26[cone]**2))*data$corr.fac[cone],
                NA))

data$cell26[cone.half.sphere]<-
  ifelse(!is.na(data$A26[cone.half.sphere])&!is.na(data$B26[cone.half.sphere]),
         1/12*pi*data$A26[cone.half.sphere]**2*(data$B26[cone.half.sphere]+.5*data$A26[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A26[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A26[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A26[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell26[double.cone]<-
  ifelse(!is.na(data$A26[double.cone])&!is.na(data$B26[double.cone]),
         1/12*pi*data$A26[double.cone]**2*data$B26[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A26[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A26[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell26[pyramid]<-
  ifelse(!is.na(data$A26[pyramid])&!is.na(data$B26[pyramid])&!is.na(data$C26[pyramid]),
         1/3*data$A26[pyramid]*data$B26[pyramid]*data$C26[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A26[pyramid])&!is.na(data$B26[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A26[pyramid]*data$B26[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A26[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C26[pyramid]),
                       1/3*data$A26[pyramid]*data$DimB[pyramid]*data$C26[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A26[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A26[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell26[cuboid]<-
  ifelse(!is.na(data$A26[cuboid])&!is.na(data$B26[cuboid])&!is.na(data$C26[cuboid]),
         data$A26[cuboid]*data$B26[cuboid]*data$C26[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A26[cuboid])&!is.na(data$B26[cuboid])&!is.na(data$DimC[cuboid]),
                data$A26[cuboid]*data$B26[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A26[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C26[cuboid]),
                       data$A26[cuboid]*data$DimB[cuboid]*data$C26[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A26[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A26[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell26[lanceol.cyl]<-
  ifelse(!is.na(data$A26[lanceol.cyl])&!is.na(data$B26[lanceol.cyl])&!is.na(data$C26[lanceol.cyl]),
         2/pi*data$A26[lanceol.cyl]*data$B26[lanceol.cyl]*data$C26[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A26[lanceol.cyl])&!is.na(data$B26[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A26[lanceol.cyl]*data$B26[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A26[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C26[lanceol.cyl]),
                       2/pi*data$A26[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C26[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A26[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A26[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell26[cuboid1]<-
  ifelse(!is.na(data$A26[cuboid1])&!is.na(data$B26[cuboid1]),
         data$A26[cuboid1]*data$B26[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A26[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A26[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell27[spindle15]<-ifelse(!is.na(data$A27[spindle15])&!is.na(data$B27[spindle15]),
                               2/15*pi*data$A27[spindle15]**2*data$B27[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A27[spindle15]),
                                      2/15*pi*data$A27[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell27[spindle11]<-
  ifelse(!is.na(data$A27[spindle11])&!is.na(data$B27[spindle11])&!is.na(data$C27[spindle11]),
         2/15*pi*data$A27[spindle11]*data$B27[spindle11]*data$C27[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A27[spindle11])&!is.na(data$B27[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A27[spindle11]*data$B27[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A27[spindle11])&!is.na(data$B27[spindle11]),
                       2/15*pi*data$A27[spindle11]*data$B27[spindle11]*data$B27[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A27[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A27[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell27[rhomb]<-
  ifelse(!is.na(data$A27[rhomb])&!is.na(data$B27[rhomb])&!is.na(data$C27[rhomb]),
         1/2*data$A27[rhomb]*data$B27[rhomb]*data$C27[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A27[rhomb])&!is.na(data$B27[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A27[rhomb]*data$B27[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A27[rhomb])&!is.na(data$B27[rhomb]),
                       1/2*data$A27[rhomb]*data$B27[rhomb]*.75*data$B27[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A27[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A27[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell27[cylinder]<-ifelse(!is.na(data$A27[cylinder])&!is.na(data$B27[cylinder]),
                              1/4*pi*data$A27[cylinder]**2*data$B27[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A27[cylinder]),
                                     1/4*pi*data$A27[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell27[elliptic.cyl]<-
  ifelse(!is.na(data$A27[elliptic.cyl])&!is.na(data$B27[elliptic.cyl])&!is.na(data$C27[elliptic.cyl]),
         1/4*pi*data$A27[elliptic.cyl]*data$B27[elliptic.cyl]*data$C27[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A27[elliptic.cyl])&!is.na(data$B27[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A27[elliptic.cyl]*data$B27[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A27[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C27[elliptic.cyl]),
                       1/4*pi*data$A27[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C27[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A27[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A27[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell27[triang.prism]<-
  ifelse(!is.na(data$A27[triang.prism])&!is.na(data$B27[triang.prism])&!is.na(data$C27[triang.prism]),
         1/2*data$A27[triang.prism]*data$B27[triang.prism]*data$C27[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A27[triang.prism])&!is.na(data$B27[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A27[triang.prism]*data$B27[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A27[triang.prism])&!is.na(data$B27[triang.prism]),
                       1/2*data$A27[triang.prism]*data$B27[triang.prism]*.75*data$B27[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A27[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A27[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell27[sphere]<-
  ifelse(!is.na(data$A27[sphere]),
         1/6*pi*data$A27[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell27[spheroid]<-
  ifelse(!is.na(data$A27[spheroid])&!is.na(data$B27[spheroid]),
         1/6*pi*data$A27[spheroid]*data$B27[spheroid]*data$A27[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A27[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A27[spheroid]*data$DimB[spheroid]*data$A27[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell27[ellipsoid]<-
  ifelse(!is.na(data$A27[ellipsoid])&!is.na(data$B27[ellipsoid])&!is.na(data$C27[ellipsoid]),
         1/6*pi*data$A27[ellipsoid]*data$B27[ellipsoid]*data$C27[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A27[ellipsoid])&!is.na(data$B27[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A27[ellipsoid]*data$B27[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A27[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C27[ellipsoid]),
                       1/6*pi*data$A27[ellipsoid]*data$DimB[ellipsoid]*data$C27[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A27[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A27[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell27[cone]<-
  ifelse(!is.na(data$A27[cone])&!is.na(data$B27[cone]),
         1/12*pi*data$A27[cone]**2*(sqrt(data$B27[cone]**2-.25*data$A27[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A27[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A27[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A27[cone]**2))*data$corr.fac[cone],
                NA))

data$cell27[cone.half.sphere]<-
  ifelse(!is.na(data$A27[cone.half.sphere])&!is.na(data$B27[cone.half.sphere]),
         1/12*pi*data$A27[cone.half.sphere]**2*(data$B27[cone.half.sphere]+.5*data$A27[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A27[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A27[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A27[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell27[double.cone]<-
  ifelse(!is.na(data$A27[double.cone])&!is.na(data$B27[double.cone]),
         1/12*pi*data$A27[double.cone]**2*data$B27[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A27[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A27[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell27[pyramid]<-
  ifelse(!is.na(data$A27[pyramid])&!is.na(data$B27[pyramid])&!is.na(data$C27[pyramid]),
         1/3*data$A27[pyramid]*data$B27[pyramid]*data$C27[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A27[pyramid])&!is.na(data$B27[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A27[pyramid]*data$B27[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A27[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C27[pyramid]),
                       1/3*data$A27[pyramid]*data$DimB[pyramid]*data$C27[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A27[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A27[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell27[cuboid]<-
  ifelse(!is.na(data$A27[cuboid])&!is.na(data$B27[cuboid])&!is.na(data$C27[cuboid]),
         data$A27[cuboid]*data$B27[cuboid]*data$C27[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A27[cuboid])&!is.na(data$B27[cuboid])&!is.na(data$DimC[cuboid]),
                data$A27[cuboid]*data$B27[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A27[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C27[cuboid]),
                       data$A27[cuboid]*data$DimB[cuboid]*data$C27[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A27[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A27[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell27[lanceol.cyl]<-
  ifelse(!is.na(data$A27[lanceol.cyl])&!is.na(data$B27[lanceol.cyl])&!is.na(data$C27[lanceol.cyl]),
         2/pi*data$A27[lanceol.cyl]*data$B27[lanceol.cyl]*data$C27[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A27[lanceol.cyl])&!is.na(data$B27[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A27[lanceol.cyl]*data$B27[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A27[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C27[lanceol.cyl]),
                       2/pi*data$A27[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C27[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A27[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A27[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell27[cuboid1]<-
  ifelse(!is.na(data$A27[cuboid1])&!is.na(data$B27[cuboid1]),
         data$A27[cuboid1]*data$B27[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A27[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A27[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell28[spindle15]<-ifelse(!is.na(data$A28[spindle15])&!is.na(data$B28[spindle15]),
                               2/15*pi*data$A28[spindle15]**2*data$B28[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A28[spindle15]),
                                      2/15*pi*data$A28[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell28[spindle11]<-
  ifelse(!is.na(data$A28[spindle11])&!is.na(data$B28[spindle11])&!is.na(data$C28[spindle11]),
         2/15*pi*data$A28[spindle11]*data$B28[spindle11]*data$C28[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A28[spindle11])&!is.na(data$B28[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A28[spindle11]*data$B28[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A28[spindle11])&!is.na(data$B28[spindle11]),
                       2/15*pi*data$A28[spindle11]*data$B28[spindle11]*data$B28[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A28[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A28[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell28[rhomb]<-
  ifelse(!is.na(data$A28[rhomb])&!is.na(data$B28[rhomb])&!is.na(data$C28[rhomb]),
         1/2*data$A28[rhomb]*data$B28[rhomb]*data$C28[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A28[rhomb])&!is.na(data$B28[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A28[rhomb]*data$B28[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A28[rhomb])&!is.na(data$B28[rhomb]),
                       1/2*data$A28[rhomb]*data$B28[rhomb]*.75*data$B28[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A28[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A28[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell28[cylinder]<-ifelse(!is.na(data$A28[cylinder])&!is.na(data$B28[cylinder]),
                              1/4*pi*data$A28[cylinder]**2*data$B28[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A28[cylinder]),
                                     1/4*pi*data$A28[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell28[elliptic.cyl]<-
  ifelse(!is.na(data$A28[elliptic.cyl])&!is.na(data$B28[elliptic.cyl])&!is.na(data$C28[elliptic.cyl]),
         1/4*pi*data$A28[elliptic.cyl]*data$B28[elliptic.cyl]*data$C28[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A28[elliptic.cyl])&!is.na(data$B28[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A28[elliptic.cyl]*data$B28[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A28[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C28[elliptic.cyl]),
                       1/4*pi*data$A28[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C28[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A28[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A28[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell28[triang.prism]<-
  ifelse(!is.na(data$A28[triang.prism])&!is.na(data$B28[triang.prism])&!is.na(data$C28[triang.prism]),
         1/2*data$A28[triang.prism]*data$B28[triang.prism]*data$C28[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A28[triang.prism])&!is.na(data$B28[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A28[triang.prism]*data$B28[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A28[triang.prism])&!is.na(data$B28[triang.prism]),
                       1/2*data$A28[triang.prism]*data$B28[triang.prism]*.75*data$B28[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A28[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A28[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell28[sphere]<-
  ifelse(!is.na(data$A28[sphere]),
         1/6*pi*data$A28[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell28[spheroid]<-
  ifelse(!is.na(data$A28[spheroid])&!is.na(data$B28[spheroid]),
         1/6*pi*data$A28[spheroid]*data$B28[spheroid]*data$A28[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A28[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A28[spheroid]*data$DimB[spheroid]*data$A28[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell28[ellipsoid]<-
  ifelse(!is.na(data$A28[ellipsoid])&!is.na(data$B28[ellipsoid])&!is.na(data$C28[ellipsoid]),
         1/6*pi*data$A28[ellipsoid]*data$B28[ellipsoid]*data$C28[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A28[ellipsoid])&!is.na(data$B28[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A28[ellipsoid]*data$B28[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A28[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C28[ellipsoid]),
                       1/6*pi*data$A28[ellipsoid]*data$DimB[ellipsoid]*data$C28[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A28[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A28[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell28[cone]<-
  ifelse(!is.na(data$A28[cone])&!is.na(data$B28[cone]),
         1/12*pi*data$A28[cone]**2*(sqrt(data$B28[cone]**2-.25*data$A28[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A28[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A28[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A28[cone]**2))*data$corr.fac[cone],
                NA))

data$cell28[cone.half.sphere]<-
  ifelse(!is.na(data$A28[cone.half.sphere])&!is.na(data$B28[cone.half.sphere]),
         1/12*pi*data$A28[cone.half.sphere]**2*(data$B28[cone.half.sphere]+.5*data$A28[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A28[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A28[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A28[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell28[double.cone]<-
  ifelse(!is.na(data$A28[double.cone])&!is.na(data$B28[double.cone]),
         1/12*pi*data$A28[double.cone]**2*data$B28[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A28[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A28[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell28[pyramid]<-
  ifelse(!is.na(data$A28[pyramid])&!is.na(data$B28[pyramid])&!is.na(data$C28[pyramid]),
         1/3*data$A28[pyramid]*data$B28[pyramid]*data$C28[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A28[pyramid])&!is.na(data$B28[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A28[pyramid]*data$B28[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A28[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C28[pyramid]),
                       1/3*data$A28[pyramid]*data$DimB[pyramid]*data$C28[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A28[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A28[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell28[cuboid]<-
  ifelse(!is.na(data$A28[cuboid])&!is.na(data$B28[cuboid])&!is.na(data$C28[cuboid]),
         data$A28[cuboid]*data$B28[cuboid]*data$C28[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A28[cuboid])&!is.na(data$B28[cuboid])&!is.na(data$DimC[cuboid]),
                data$A28[cuboid]*data$B28[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A28[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C28[cuboid]),
                       data$A28[cuboid]*data$DimB[cuboid]*data$C28[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A28[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A28[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell28[lanceol.cyl]<-
  ifelse(!is.na(data$A28[lanceol.cyl])&!is.na(data$B28[lanceol.cyl])&!is.na(data$C28[lanceol.cyl]),
         2/pi*data$A28[lanceol.cyl]*data$B28[lanceol.cyl]*data$C28[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A28[lanceol.cyl])&!is.na(data$B28[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A28[lanceol.cyl]*data$B28[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A28[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C28[lanceol.cyl]),
                       2/pi*data$A28[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C28[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A28[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A28[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell28[cuboid1]<-
  ifelse(!is.na(data$A28[cuboid1])&!is.na(data$B28[cuboid1]),
         data$A28[cuboid1]*data$B28[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A28[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A28[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell29[spindle15]<-ifelse(!is.na(data$A29[spindle15])&!is.na(data$B29[spindle15]),
                               2/15*pi*data$A29[spindle15]**2*data$B29[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A29[spindle15]),
                                      2/15*pi*data$A29[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell29[spindle11]<-
  ifelse(!is.na(data$A29[spindle11])&!is.na(data$B29[spindle11])&!is.na(data$C29[spindle11]),
         2/15*pi*data$A29[spindle11]*data$B29[spindle11]*data$C29[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A29[spindle11])&!is.na(data$B29[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A29[spindle11]*data$B29[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A29[spindle11])&!is.na(data$B29[spindle11]),
                       2/15*pi*data$A29[spindle11]*data$B29[spindle11]*data$B29[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A29[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A29[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell29[rhomb]<-
  ifelse(!is.na(data$A29[rhomb])&!is.na(data$B29[rhomb])&!is.na(data$C29[rhomb]),
         1/2*data$A29[rhomb]*data$B29[rhomb]*data$C29[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A29[rhomb])&!is.na(data$B29[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A29[rhomb]*data$B29[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A29[rhomb])&!is.na(data$B29[rhomb]),
                       1/2*data$A29[rhomb]*data$B29[rhomb]*.75*data$B29[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A29[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A29[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell29[cylinder]<-ifelse(!is.na(data$A29[cylinder])&!is.na(data$B29[cylinder]),
                              1/4*pi*data$A29[cylinder]**2*data$B29[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A29[cylinder]),
                                     1/4*pi*data$A29[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell29[elliptic.cyl]<-
  ifelse(!is.na(data$A29[elliptic.cyl])&!is.na(data$B29[elliptic.cyl])&!is.na(data$C29[elliptic.cyl]),
         1/4*pi*data$A29[elliptic.cyl]*data$B29[elliptic.cyl]*data$C29[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A29[elliptic.cyl])&!is.na(data$B29[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A29[elliptic.cyl]*data$B29[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A29[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C29[elliptic.cyl]),
                       1/4*pi*data$A29[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C29[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A29[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A29[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell29[triang.prism]<-
  ifelse(!is.na(data$A29[triang.prism])&!is.na(data$B29[triang.prism])&!is.na(data$C29[triang.prism]),
         1/2*data$A29[triang.prism]*data$B29[triang.prism]*data$C29[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A29[triang.prism])&!is.na(data$B29[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A29[triang.prism]*data$B29[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A29[triang.prism])&!is.na(data$B29[triang.prism]),
                       1/2*data$A29[triang.prism]*data$B29[triang.prism]*.75*data$B29[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A29[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A29[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell29[sphere]<-
  ifelse(!is.na(data$A29[sphere]),
         1/6*pi*data$A29[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell29[spheroid]<-
  ifelse(!is.na(data$A29[spheroid])&!is.na(data$B29[spheroid]),
         1/6*pi*data$A29[spheroid]*data$B29[spheroid]*data$A29[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A29[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A29[spheroid]*data$DimB[spheroid]*data$A29[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell29[ellipsoid]<-
  ifelse(!is.na(data$A29[ellipsoid])&!is.na(data$B29[ellipsoid])&!is.na(data$C29[ellipsoid]),
         1/6*pi*data$A29[ellipsoid]*data$B29[ellipsoid]*data$C29[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A29[ellipsoid])&!is.na(data$B29[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A29[ellipsoid]*data$B29[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A29[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C29[ellipsoid]),
                       1/6*pi*data$A29[ellipsoid]*data$DimB[ellipsoid]*data$C29[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A29[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A29[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell29[cone]<-
  ifelse(!is.na(data$A29[cone])&!is.na(data$B29[cone]),
         1/12*pi*data$A29[cone]**2*(sqrt(data$B29[cone]**2-.25*data$A29[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A29[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A29[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A29[cone]**2))*data$corr.fac[cone],
                NA))

data$cell29[cone.half.sphere]<-
  ifelse(!is.na(data$A29[cone.half.sphere])&!is.na(data$B29[cone.half.sphere]),
         1/12*pi*data$A29[cone.half.sphere]**2*(data$B29[cone.half.sphere]+.5*data$A29[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A29[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A29[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A29[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell29[double.cone]<-
  ifelse(!is.na(data$A29[double.cone])&!is.na(data$B29[double.cone]),
         1/12*pi*data$A29[double.cone]**2*data$B29[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A29[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A29[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell29[pyramid]<-
  ifelse(!is.na(data$A29[pyramid])&!is.na(data$B29[pyramid])&!is.na(data$C29[pyramid]),
         1/3*data$A29[pyramid]*data$B29[pyramid]*data$C29[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A29[pyramid])&!is.na(data$B29[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A29[pyramid]*data$B29[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A29[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C29[pyramid]),
                       1/3*data$A29[pyramid]*data$DimB[pyramid]*data$C29[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A29[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A29[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell29[cuboid]<-
  ifelse(!is.na(data$A29[cuboid])&!is.na(data$B29[cuboid])&!is.na(data$C29[cuboid]),
         data$A29[cuboid]*data$B29[cuboid]*data$C29[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A29[cuboid])&!is.na(data$B29[cuboid])&!is.na(data$DimC[cuboid]),
                data$A29[cuboid]*data$B29[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A29[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C29[cuboid]),
                       data$A29[cuboid]*data$DimB[cuboid]*data$C29[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A29[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A29[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell29[lanceol.cyl]<-
  ifelse(!is.na(data$A29[lanceol.cyl])&!is.na(data$B29[lanceol.cyl])&!is.na(data$C29[lanceol.cyl]),
         2/pi*data$A29[lanceol.cyl]*data$B29[lanceol.cyl]*data$C29[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A29[lanceol.cyl])&!is.na(data$B29[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A29[lanceol.cyl]*data$B29[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A29[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C29[lanceol.cyl]),
                       2/pi*data$A29[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C29[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A29[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A29[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell29[cuboid1]<-
  ifelse(!is.na(data$A29[cuboid1])&!is.na(data$B29[cuboid1]),
         data$A29[cuboid1]*data$B29[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A29[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A29[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))


#A,B,C,Cell
#Cell1

data$cell30[spindle15]<-ifelse(!is.na(data$A30[spindle15])&!is.na(data$B30[spindle15]),
                               2/15*pi*data$A30[spindle15]**2*data$B30[spindle15]*data$corr.fac[spindle15],
                               ifelse(!is.na(data$A30[spindle15]),
                                      2/15*pi*data$A30[spindle15]**2*data$DimB[spindle15]*data$corr.fac[spindle15],
                                      NA))

data$cell30[spindle11]<-
  ifelse(!is.na(data$A30[spindle11])&!is.na(data$B30[spindle11])&!is.na(data$C30[spindle11]),
         2/15*pi*data$A30[spindle11]*data$B30[spindle11]*data$C30[spindle11]*data$corr.fac[spindle11],
         ifelse(!is.na(data$A30[spindle11])&!is.na(data$B30[spindle11])&!is.na(data$DimC[spindle11]),
                2/15*pi*data$A30[spindle11]*data$B30[spindle11]*data$DimC[spindle11]*data$corr.fac[spindle11],
                ifelse(!is.na(data$A30[spindle11])&!is.na(data$B30[spindle11]),
                       2/15*pi*data$A30[spindle11]*data$B30[spindle11]*data$B30[spindle11]*data$corr.fac[spindle11],
                       ifelse(!is.na(data$A30[spindle11])&!is.na(data$DimB[spindle11]),
                              2/15*pi*data$A30[spindle11]*data$DimB[spindle11]*data$DimB[spindle11]*data$corr.fac[spindle11],
                              NA))))

data$cell30[rhomb]<-
  ifelse(!is.na(data$A30[rhomb])&!is.na(data$B30[rhomb])&!is.na(data$C30[rhomb]),
         1/2*data$A30[rhomb]*data$B30[rhomb]*data$C30[rhomb]*data$corr.fac[rhomb],
         ifelse(!is.na(data$A30[rhomb])&!is.na(data$B30[rhomb])&!is.na(data$DimC[rhomb]),
                1/2*data$A30[rhomb]*data$B30[rhomb]*data$DimC[rhomb]*data$corr.fac[rhomb],
                ifelse(!is.na(data$A30[rhomb])&!is.na(data$B30[rhomb]),
                       1/2*data$A30[rhomb]*data$B30[rhomb]*.75*data$B30[rhomb]*data$corr.fac[rhomb],
                       ifelse(!is.na(data$A30[rhomb])&!is.na(data$DimB[rhomb]),
                              1/2*data$A30[rhomb]*data$DimB[rhomb]*.75*data$DimB[rhomb]*data$corr.fac[rhomb],
                              NA))))


data$cell30[cylinder]<-ifelse(!is.na(data$A30[cylinder])&!is.na(data$B30[cylinder]),
                              1/4*pi*data$A30[cylinder]**2*data$B30[cylinder]*data$corr.fac[cylinder],
                              ifelse(!is.na(data$A30[cylinder]),
                                     1/4*pi*data$A30[cylinder]**2*data$DimB[cylinder]*data$corr.fac[cylinder],
                                     NA))

data$cell30[elliptic.cyl]<-
  ifelse(!is.na(data$A30[elliptic.cyl])&!is.na(data$B30[elliptic.cyl])&!is.na(data$C30[elliptic.cyl]),
         1/4*pi*data$A30[elliptic.cyl]*data$B30[elliptic.cyl]*data$C30[elliptic.cyl]*data$corr.fac[elliptic.cyl],
         ifelse(!is.na(data$A30[elliptic.cyl])&!is.na(data$B30[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                1/4*pi*data$A30[elliptic.cyl]*data$B30[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                ifelse(!is.na(data$A30[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$C30[elliptic.cyl]),
                       1/4*pi*data$A30[elliptic.cyl]*data$DimB[elliptic.cyl]*data$C30[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                       ifelse(!is.na(data$A30[elliptic.cyl])&!is.na(data$DimB[elliptic.cyl])&!is.na(data$DimC[elliptic.cyl]),
                              1/4*pi*data$A30[elliptic.cyl]*data$DimB[elliptic.cyl]*data$DimC[elliptic.cyl]*data$corr.fac[elliptic.cyl],
                              NA))))

data$cell30[triang.prism]<-
  ifelse(!is.na(data$A30[triang.prism])&!is.na(data$B30[triang.prism])&!is.na(data$C30[triang.prism]),
         1/2*data$A30[triang.prism]*data$B30[triang.prism]*data$C30[triang.prism]*data$corr.fac[triang.prism],
         ifelse(!is.na(data$A30[triang.prism])&!is.na(data$B30[triang.prism])&!is.na(data$DimC[triang.prism]),
                1/2*data$A30[triang.prism]*data$B30[triang.prism]*data$DimC[triang.prism]*data$corr.fac[triang.prism],
                ifelse(!is.na(data$A30[triang.prism])&!is.na(data$B30[triang.prism]),
                       1/2*data$A30[triang.prism]*data$B30[triang.prism]*.75*data$B30[triang.prism]*data$corr.fac[triang.prism],
                       ifelse(!is.na(data$A30[triang.prism])&!is.na(data$DimB[triang.prism]),
                              1/2*data$A30[triang.prism]*data$DimB[triang.prism]*.75*data$DimB[triang.prism]*data$corr.fac[triang.prism],
                              NA))))

data$cell30[sphere]<-
  ifelse(!is.na(data$A30[sphere]),
         1/6*pi*data$A30[sphere]**3*data$corr.fac[sphere],
         NA)

data$cell30[spheroid]<-
  ifelse(!is.na(data$A30[spheroid])&!is.na(data$B30[spheroid]),
         1/6*pi*data$A30[spheroid]*data$B30[spheroid]*data$A30[spheroid]*data$corr.fac[spheroid],
         ifelse(!is.na(data$A30[spheroid])&!is.na(data$DimB[spheroid]),
                1/6*pi*data$A30[spheroid]*data$DimB[spheroid]*data$A30[spheroid]*data$corr.fac[spheroid],
                NA))

data$cell30[ellipsoid]<-
  ifelse(!is.na(data$A30[ellipsoid])&!is.na(data$B30[ellipsoid])&!is.na(data$C30[ellipsoid]),
         1/6*pi*data$A30[ellipsoid]*data$B30[ellipsoid]*data$C30[ellipsoid]*data$corr.fac[ellipsoid],
         ifelse(!is.na(data$A30[ellipsoid])&!is.na(data$B30[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                1/6*pi*data$A30[ellipsoid]*data$B30[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                ifelse(!is.na(data$A30[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$C30[ellipsoid]),
                       1/6*pi*data$A30[ellipsoid]*data$DimB[ellipsoid]*data$C30[ellipsoid]*data$corr.fac[ellipsoid],
                       ifelse(!is.na(data$A30[ellipsoid])&!is.na(data$DimB[ellipsoid])&!is.na(data$DimC[ellipsoid]),
                              1/6*pi*data$A30[ellipsoid]*data$DimB[ellipsoid]*data$DimC[ellipsoid]*data$corr.fac[ellipsoid],
                              NA))))

data$cell30[cone]<-
  ifelse(!is.na(data$A30[cone])&!is.na(data$B30[cone]),
         1/12*pi*data$A30[cone]**2*(sqrt(data$B30[cone]**2-.25*data$A30[cone]**2))*data$corr.fac[cone],
         ifelse(!is.na(data$A30[cone])&!is.na(data$DimB[cone]),
                1/12*pi*data$A30[cone]**2*(sqrt(data$DimB[cone]**2-.25*data$A30[cone]**2))*data$corr.fac[cone],
                NA))

data$cell30[cone.half.sphere]<-
  ifelse(!is.na(data$A30[cone.half.sphere])&!is.na(data$B30[cone.half.sphere]),
         1/12*pi*data$A30[cone.half.sphere]**2*(data$B30[cone.half.sphere]+.5*data$A30[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
         ifelse(!is.na(data$A30[cone.half.sphere])&!is.na(data$DimB[cone.half.sphere]),
                1/12*pi*data$A30[cone.half.sphere]**2*(data$DimB[cone.half.sphere]+.5*data$A30[cone.half.sphere]**2)*data$corr.fac[cone.half.sphere],
                NA))

data$cell30[double.cone]<-
  ifelse(!is.na(data$A30[double.cone])&!is.na(data$B30[double.cone]),
         1/12*pi*data$A30[double.cone]**2*data$B30[double.cone]**2*data$corr.fac[double.cone],
         ifelse(!is.na(data$A30[double.cone])&!is.na(data$DimB[double.cone]),
                1/12*pi*data$A30[double.cone]**2*data$DimB[double.cone]**2*data$corr.fac[double.cone],
                NA))

data$cell30[pyramid]<-
  ifelse(!is.na(data$A30[pyramid])&!is.na(data$B30[pyramid])&!is.na(data$C30[pyramid]),
         1/3*data$A30[pyramid]*data$B30[pyramid]*data$C30[pyramid]*data$corr.fac[pyramid],
         ifelse(!is.na(data$A30[pyramid])&!is.na(data$B30[pyramid])&!is.na(data$DimC[pyramid]),
                1/3*data$A30[pyramid]*data$B30[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                ifelse(!is.na(data$A30[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$C30[pyramid]),
                       1/3*data$A30[pyramid]*data$DimB[pyramid]*data$C30[pyramid]*data$corr.fac[pyramid],
                       ifelse(!is.na(data$A30[pyramid])&!is.na(data$DimB[pyramid])&!is.na(data$DimC[pyramid]),
                              1/3*data$A30[pyramid]*data$DimB[pyramid]*data$DimC[pyramid]*data$corr.fac[pyramid],
                              NA))))

data$cell30[cuboid]<-
  ifelse(!is.na(data$A30[cuboid])&!is.na(data$B30[cuboid])&!is.na(data$C30[cuboid]),
         data$A30[cuboid]*data$B30[cuboid]*data$C30[cuboid]*data$corr.fac[cuboid],
         ifelse(!is.na(data$A30[cuboid])&!is.na(data$B30[cuboid])&!is.na(data$DimC[cuboid]),
                data$A30[cuboid]*data$B30[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                ifelse(!is.na(data$A30[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$C30[cuboid]),
                       data$A30[cuboid]*data$DimB[cuboid]*data$C30[cuboid]*data$corr.fac[cuboid],
                       ifelse(!is.na(data$A30[cuboid])&!is.na(data$DimB[cuboid])&!is.na(data$DimC[cuboid]),
                              data$A30[cuboid]*data$DimB[cuboid]*data$DimC[cuboid]*data$corr.fac[cuboid],
                              NA))))

data$cell30[lanceol.cyl]<-
  ifelse(!is.na(data$A30[lanceol.cyl])&!is.na(data$B30[lanceol.cyl])&!is.na(data$C30[lanceol.cyl]),
         2/pi*data$A30[lanceol.cyl]*data$B30[lanceol.cyl]*data$C30[lanceol.cyl]*data$corr.fac[lanceol.cyl],
         ifelse(!is.na(data$A30[lanceol.cyl])&!is.na(data$B30[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                2/pi*data$A30[lanceol.cyl]*data$B30[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                ifelse(!is.na(data$A30[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$C30[lanceol.cyl]),
                       2/pi*data$A30[lanceol.cyl]*data$DimB[lanceol.cyl]*data$C30[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                       ifelse(!is.na(data$A30[lanceol.cyl])&!is.na(data$DimB[lanceol.cyl])&!is.na(data$DimC[lanceol.cyl]),
                              2/pi*data$A30[lanceol.cyl]*data$DimB[lanceol.cyl]*data$DimC[lanceol.cyl]*data$corr.fac[lanceol.cyl],
                              NA))))

data$cell30[cuboid1]<-
  ifelse(!is.na(data$A30[cuboid1])&!is.na(data$B30[cuboid1]),
         data$A30[cuboid1]*data$B30[cuboid1]**2*data$corr.fac[cuboid1],
         ifelse(!is.na(data$A30[cuboid1])&!is.na(data$DimB[cuboid1]),
                data$A30[cuboid1]*data$DimB[cuboid1]**2*data$corr.fac[cuboid1],
                NA))



