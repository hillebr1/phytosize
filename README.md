# Phytosize

This repository contains all data and code for the analyis of phytoplankton cell size in the German Wadden Sea, which is associated to the article "Temporal declines in Wadden Sea phytoplankton cell volumes observed within and across species" by Helmut Hillebrand, Josie Antonucci Di Carvalho, Jan-Claas Dajka, Claus-Dieter Dürselen, Onur Kerimoglu, Lucie Kuczynski, Lena Rönn, and Alexey Ryabov. When using the data or code, please cite the DOI of this repository as well as the original manuscript. 

## Overall setup

The phytoplankton data used for the analyses are generated from the original counts which are collated in a single dataset (summary.csv). The script "dataman.R" goes through all steps of data homogenization, including deletion of heterotrophic and parasitic species. The script sources the "biovol.R" script that provides a harmonized calculation of the cell volume of each specimen measured. 

The resulting dataset (ppall_corr.csv) is then analyzed as coded in "phytosize_main_revised.R". To plot dates correctly, we use (dates.csv) and for merging the phytoplankton data with associated environmental variables, we use a larger set of environmental data from a monitoring integration project (MARISCO_pp_wadden_env.csv).

## Metadata

### Phytoplankton data (ppall_corr.csv, summary.csv)
"stationID": unique identifier of sampling station

"date": sampling date in dd.mm.yyyy

"phylum","class","order","genus","species": name and taxonomy of the species

"geom_ID","geom_corp": description of the geometric analogue of the cell shape

"corr.fac","HDB","HDC": description of the need to include a correction factor and the estimation factor for potential hidden dimensions 

"dilution","chamb.vol","count.fac","N": information on counting and mean dimensions used in the spreadsheet

"DimA","DimB","DimC","DimD","DimE": Median estimated for dimensions A to E as depicted by the geometric shapes for each species in each sample

"abundance": Abundance of cells per species per sample

"cell.vol": Median cell volume based on the median dimensions (see above)

"biovol": Biovolume as product of abundance and cell volume

"cell.C": Transformation of cell volume to cell carbon using the methodology of Menden-Deuer et al. 

"bioC": Carbon per species per sample

"A1","A2","A3",[...],"A30": Measured dimension A for speciment 1 to (maximal) 30 of the species per sample

"B1","B2","B3",[...], "B30": Measured dimension B for speciment 1 to (maximal) 30 of the species per sample

"C1","C2","C3",[...],"C30": Measured dimension C for speciment 1 to (maximal) 30 of the species per sample

"exclude": reasons to exclude species from analysis

"year": sampling year

"julian": julian day of sampling

"USI": Unique sample identifier (Station & date)

"DimA.needed","DimB.needed","DimC.needed","DimD.needed","DimE.needed": Logical factor whether nearest geometric shape needs dimension A to E

"corr.fac.needed": Logical factor identifying whether a correction factor is needed or nor

"A.count","B.count","C.count": number of unqiue measured dimensions per species and samples

"specname": Species name, i.e., genus_species

"UTI": unique taxon identifier

"UTSI": unique taxon sample identifier (merges USI and UTI)

"specname.unique": unique species differ, differs from specname by giving numbers to cells 

"mean.size": mean of all cell sizes calculated for a certain species in a certain sample

"cell1","cell2","cell3",[...],"cell30": Volume of each measured cell (µm3) of a species in a sample

"equation" unique name of the equation used in biovol.R

### Environmental data

