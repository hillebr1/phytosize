# Phytosize

This analyis of phytoplankton cell size in the German Wadden Sea is associated to the article "Temporal declines in Wadden Sea phytoplankton cell volumes observed within and across species" by Helmut Hillebrand, Josie Antonucci Di Carvalho, Jan-Claas Dajka, Claus-Dieter Dürselen, Onur Kerimoglu, Lucie Kuczynski, Lena Rönn, Alexey Ryabov. When using the data or code, please inform the corresponding author, Helmut Hillebrand, under helmut.hillebrand@uni-oldenburg.de. 

Data are in "data", scripts in "scripts".

## Main analysis
The main analysis is coded in phytosize_main_revised.R, which uses the phytoplankton data (ppall_corr.csv), a set of dates for plotting (dates.csv) and the environmental variables associated to the phytoplankton samples. 

"stationID": unique identifier of sampling station
"date": sampling date
"phylum","class","order","genus","species": taxonomy of the species
"geom_ID","geom_corp": description of the geometric analogue
"corr.fac","HDB","HDC": description of the need to indluce a correction factor and 
"dilution","chamb.vol","count.fac","N","DimA","DimB","DimC","DimD","DimE": information on counting and mean dimensions used in the spreadsheet
"abundance"
"cell.vol"
"biovol"
"cell.C"
"bioC","X","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16","A17","A18","A19","A20","A21","A22","A23","A24","A25","A26","A27","A28","A29","A30","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","B13","B14","B15","B16","B17","B18","B19","B20","B21","B22","B23","B24","B25","B26","B27","B28","B29","B30","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12","C13","C14","C15","C16","C17","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27","C28","C29","C30","X.1","X.2","exclude","year","julian","USI","DimA.needed","DimB.needed","DimC.needed","DimD.needed","DimE.needed","corr.fac.needed","A.count","B.count","C.count","specname","UTI","UTSI","specname.unique","mean.size","cell1","cell2","cell3","cell4","cell5","cell6","cell7","cell8","cell9","cell10","cell11","cell12","cell13","cell14","cell15","cell16","cell17","cell18","cell19","cell20","cell21","cell22","cell23","cell24","cell25","cell26","cell27","cell28","cell29","cell30","equation","hidden","transient"	
![grafik](https://user-images.githubusercontent.com/60977168/142261019-a2b10703-3296-4225-b506-c03bf2188561.png)

## Main analysis

