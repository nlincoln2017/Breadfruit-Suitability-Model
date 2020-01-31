#################################################
####MAUSIO Et al. BREADFRUIT SUITABILITY MODEL###
#################################################

#ALOHA! 
#This code can be used on climate projection datasets
#as long as the rasters are defined. OUr methodology was an inclusive so
#we used all 17 GLobal Climate Models that had data for year 2070. ENJOY!!
#GCM data for average rainfall and temp can be found on the WorldClim website

#install.packages("googlesheets")

library(googlesheets) #<---intial setup will ask for google login credentials
suppressMessages(library(dplyr))
library(raster)
library(rgdal)
library(ggplot2)
library(rasterVis)
library(RColorBrewer)
library(zoo)

###TO CLEAR TEMP FILES WHEN IT GETS TOO FULL
#tempfile()
dirname(tempdir())

###set values for each row that is marked: #>>>

#>>>set working directory
setwd("x")

#############CONNECT TO SPREADSHEET OF ENV. RANGES################
#>>>copy and paste googlesheet title
#>>>populate with cell ranges (top left corner, bottom right corner)

suit<- gs_title("x")
suitAll<-suit %>% gs_read(ws = "Sheet1", range = cell_limits(c(1, 1), c(4, 21)), col_names = TRUE)
row.names(suitAll)<-suitAll$X1

############CONNECT TO RASTERS OF ENV, VARIABLES OF INTEREST (VOI) and basemap layers##################
#<<<connect each voi to rasters on file 
#(t=temp, r=rainfall, p=ph, s=solar radiation, d=drainage, n=Rnet)

#ACCESS1.0 TCP8.5 2070
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\ACCESS1-0\\ac85bi70\\ac85bi70 ann temp.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\ACCESS1-0\\ac85bi70\\ac85bi7012 precip.tif")

#ACCESS1.0 TCP4.5 2070
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\GFDL_CM3\\gf85bi70\\gf85bi70 ann temp final.tif")
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\GFDL_CM3\\gf85bi70\\gf85bi70 ann temp.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\ACCESS1-0\\ac45bi70\\ac45bi7012 precip.tif")
plot(r)


#GFDL-CM3 TCP8.5 2070
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\GFDL_CM3\\gf85bi70\\gf85bi70 ann temp final.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\GFDL_CM3\\gf85bi70\\gf85bi7012 precip.tif")

#GFDL-CM3 TCP4.5 2070
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\GFDL_CM3\\gf45bi70\\gf45bi70 ann temp.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\GFDL_CM3\\gf45bi70\\gf45bi7012 precip.tif")

#HadGEM2-ES TCP8.5 2070
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\HadGEM2_ES\\he85bi70\\he85bi70 ann temp.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\HadGEM2_ES\\he85bi70\\he85bi7012 precip.tif")

#HadGEM2-ES TCP4.5 2070
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\BCC-CSM1-1\\bc855bi70\\bc85bi70 ann temp final.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\BCC-CSM1-1\\bc85bi70\\bc85bi7012 precip.tif")

#BCC-CSM1-1 TCP8.5 2070
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\BCC_CSM1_1\\bc85bi701 ann temp.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\BCC_CSM1_1\\bc85bi7012.tif")

#BCC-CSM1-1 TCP4.5 2070
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\BCC_CSM1_1\\bc45bi701 ann temp.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\BCC_CSM1_1\\bc45bi7012.tif")

#CCSM4 TCP8.5 2070
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\CCSM4\\cc85bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\CCSM4\\cc85bi7012.tif")

#CCSM4 TCP4.5 2070
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\CCSM4\\cc45bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\CCSM4\\cc45bi7012.tif")

#CNRM-CM5 (#) TCP8.5 
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\CNRM-CM5 (#)\\cn85bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\CNRM-CM5 (#)\\cn85bi7012.tif")

#CNRM-CM5 (#) TCP4.5 
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\CNRM-CM5 (#)\\cn45bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\CNRM-CM5 (#)\\cn45bi7012.tif")

#GISS-E2-R RCP8.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\GISS-E2-R\\gs85bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\GISS-E2-R\\gs85bi7012.tif")

#GISS-E2-R RCP4.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\GISS-E2-R\\gs45bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\GISS-E2-R\\gs45bi7012.tif")

#HadGEM2-AO RCP8.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\HadGEM2-AO\\hd85bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\HadGEM2-AO\\hd85bi7012.tif")

#HadGEM2-AO
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\HadGEM2-AO\\hd45bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\HadGEM2-AO\\hd45bi7012.tif")

#HadGEM2-CC RCP 8.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\HadGEM2-CC\\hg85bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\HadGEM2-CC\\hg85bi7012.tif")

#HadGEM2-CC  RCP4.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\HadGEM2-CC\\hg45bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\HadGEM2-CC\\hg45bi7012.tif")

#INMCM4 RCP 8.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\INMCM4\\in85bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\INMCM4\\in85bi7012.tif")

#INMCM4  RCP4.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\INMCM4\\in45bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\INMCM4\\in45bi7012.tif")


#IPSL-CM5A-LR rcp8.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\IPSL-CM5A-LR\\ip85bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\IPSL-CM5A-LR\\ip85bi7012.tif")

#IPSL-CM5A-LR  RCP4.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\IPSL-CM5A-LR\\ip45bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\IPSL-CM5A-LR\\ip45bi7012.tif")

#MIROC-ESM-CHEM (#) RCP8.5
t<-raster("x\\Raster Layers\\Global Climate Projections\\CMIP5\\WorldClim\\MIROC-ESM-CHEM (#)\\mi85bi701.tif")
r<-raster("x\\Raster Layers\\Global Climate Projections\\CMIP5\\WorldClim\\MIROC-ESM-CHEM (#)\\mi85bi7012.tif")

#MIROC-ESM-CHEM (#)  RCP4.5
t<-raster("x\\Raster Layers\\Global Climate Projections\\CMIP5\\WorldClim\\MIROC-ESM-CHEM (#)\\mi45bi701.tif")
r<-raster("x\\Raster Layers\\Global Climate Projections\\CMIP5\\WorldClim\\MIROC-ESM-CHEM (#)\\mi45bi7012.tif")

#MIROC-ESM (#) rcp8.5
t<-raster("x\\Raster Layers\\Global Climate Projections\\CMIP5\\WorldClim\\MIROC-ESM (#)\\mr85bi701.tif")
r<-raster("x\\Raster Layers\\Global Climate Projections\\CMIP5\\WorldClim\\MIROC-ESM (#)\\mr85bi7012.tif")

#MIROC-ESM (#)  RCP4.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\MIROC-ESM (#)\\mr45bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\MIROC-ESM (#)\\mr45bi7012.tif")

#MIROC5 (#) RCP8.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\MIROC5 (#)\\mc85bi701.tif")
r<-raster("x\\Raster Layers\\Global Climate Projections\\CMIP5\\WorldClim\\MIROC5 (#)\\mc85bi7012.tif")

#MIROC5 (#)  RCP4.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\MIROC5 (#)\\mc45bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\MIROC5 (#)\\mc45bi7012.tif")

#MPI-ESM-LR RCP8.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\MPI-ESM-LR\\mp85bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\MPI-ESM-LR\\mp85bi7012.tif")

#MPI-ESM-LR  RCP4.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\MPI-ESM-LR\\mp45bi701.tif")
r<-raster("x\\Raster Layers\\Global Climate Projections\\CMIP5\\WorldClim\\MPI-ESM-LR\\mp45bi7012.tif")

#MRI-CGCM3 RCP8.5
t<-raster("x\\Raster Layers\\Global Climate Projections\\CMIP5\\WorldClim\\MRI-CGCM3\\mg85bi701.tif")
r<-raster("x\\Raster Layers\\Global Climate Projections\\CMIP5\\WorldClim\\MRI-CGCM3\\mg85bi7012.tif")

#MPI-ESM-LR  RCP4.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\MRI-CGCM3\\mg45bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\MRI-CGCM3\\mg45bi7012.tif")

#NorESM1-M RCP8.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\NorESM1-M\\no85bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\NorESM1-M\\no85bi7012.tif")

#NorESM1-M  RCP4.5
t<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\NorESM1-M\\no45bi701.tif")
r<-raster("x\\Global Climate Projections\\CMIP5\\WorldClim\\NorESM1-M\\no45bi7012.tif")


p<-raster("x.tif")
s<-raster("x.tif")
#n<-raster("x.tif")
d<-raster("x.tif")
#penPET<-raster("x.tif")
#scoast<-shapefile("x.shp")

##################SET UP CODE VARIABLES###########################
##view spreadsheet
suitAll

######################
### temperature ######
######################

#>>>Identify the row of interest (roi): cropname+EcoCrop or cropname+LocalSources
roi<-subset(suitAll, row.names(suitAll) =="Rcode")
roi

#>>>Set variable by replacing with name variable of interest: temp, rain, ph or rad
absMin=roi$tempAbsMin
absMax=roi$tempAbsMax
optMin<-roi$tempOptMin
optMax<-roi$tempOptMax
range1<-optMin-absMin
range2<-absMax-optMax

#>>>Set the raster of the variable of interest (VOI) object
#(t=temp, r=rainfall, p=ph, s=solar radiation, a=aridity)

voi<-t
plot(voi)

#calcualte Absolute range
abs<-voi
abs[voi<absMin]<-NA
abs[voi>absMax]<-NA
plot(abs)

#Calculate Score0
abs0<-abs
abs0[abs!=0]<-NA
#plot(abs0)

#Calculate Score1
abs1<-abs
abs1[abs>optMin]<-NA
plot(abs1)


fun<-function(x) {((x-absMin)/range1)*100}
score1<-calc(abs1, fun) 
plot(score1)

#Calculate Score100 (Optimum Range)
opt<-voi
opt[voi<optMin]<-0
opt[voi>optMax]<-0
opt[opt!=0]<-100
opt[opt!=100]<-NA
plot(opt)

#Calculate Score2
abs2<-abs
abs2[abs<optMax]<-NA
plot(abs2)

fun<-function(x) {(((1-(x-optMax))/range2)*100+100)}
score2<-calc(abs2, fun)
#score2<-abs(score2))
plot(score2)

#>>>final suitability layer = combine score0 (abs0), score1, score100(opt), and score2
final<-merge(abs0, score1, opt, score2 )
plot(final,  main="Temperature suitability of 'Ulu")

plot(coast, add=TRUE) #need to set projections

#>>>assign object name to output
tSuit<-final
#tiff(tSuit, filename = "tSuit.tif")
writeRaster(tSuit, "CropX Global Suitability - Temperature hd4.5 2070 900m.tif", overwrite=TRUE )

#>>>export file, change output title to reflect name of Variable of Interest (voi), and Crop of Interest (coi)
jpeg(file="Temperature suitability of CropX.jpeg")
plot(tSuit, main="Temperature suitability of CropX")
plot(coast, add=TRUE)
dev.off()

######################
###### rainfall ######
######################

#>>>Identify the row of interest (roi): cropname+EcoCrop or cropname+LocalSources
roi<-subset(suitAll, row.names(suitAll) =="Rcode")
roi

#>>>Set variable by replacing with name variable of interest: temp, rain, ph or rad
absMin=roi$rainAbsMin
absMax=roi$rainAbsMax
optMin<-roi$rainOptMin
optMax<-roi$rainOptMax
range1<-optMin-absMin
range2<-absMax-optMax

#>>>Set the raster of the variable of interest (VOI) object
#(t=temp, r=rainfall, p=ph, s=solar radiation, d=drainage class, a=aridity)

#plot(r)
voi<-r
plot(voi)

#calcualte Absolute range
abs<-voi
abs[voi<absMin]<-NA
abs[voi>absMax]<-NA
plot(abs)

#Calculate Score0
abs0<-abs
abs0[abs!=0]<-NA
plot(abs0)

#Calculate Score1
abs1<-abs
abs1[abs>optMin]<-NA
plot(abs1)

fun<-function(x) {((x-absMin)/range1)*100}
score1<-calc(abs1, fun) 
plot(score1)

#Calculate Score100 (Optimum Range)
opt<-voi
opt[voi<optMin]<-0
opt[voi>optMax]<-0
opt[opt!=0]<-100
opt[opt!=100]<-NA
plot(opt)

#Calculate Score2
abs2<-abs
abs2[abs<optMax]<-NA
plot(abs2)

fun<-function(x) {(((1-(x-optMax))/range2)*100)+100}
score2<-calc(abs2, fun)
#score2<-abs(score2)
plot(score2)

#>>>final suitability layer = combine score0 (abs0), score1, score100(opt), and score2
final<-merge(abs0, score1, opt, score2 )
plot(final,  main="Rainfall global suitability of CropX")
plot(coast, add=TRUE) #need to set projections

#>>>assign object name to output
rSuit<-final
#tiff(rSuit, filename = "rSuit.tif")
writeRaster(rSuit, "CropX Global Suitability - Rainfall no4.5 2070 900m.tif", overwrite=TRUE )

#>>>export file, change output title to reflect name of Variable of Interest (voi), and Crop of Interest (coi)
jpeg(file="Rainfall (850mm Absmin) suitability of CropX.jpeg")
plot(rSuit, main="Rainfall (850mm AbsMin) suitability of CropX")
plot(coast, add=TRUE)
dev.off()


######################
## solar radiation ###
######################

#>>>Identify the row of interest (roi): cropname+EcoCrop or cropname+LocalSources
roi<-subset(suitAll, row.names(suitAll) =="Rcode")
roi

#>>>Set variable by replacing with name variable of interest: temp, rain, ph or rad
absMin=roi$radAbsMin
absMax=roi$radAbsMax
optMin<-roi$radOptMin
optMax<-roi$radOptMax
range1<-optMin-absMin
range2<-absMax-optMax

#>>>Set the raster of the variable of interest (VOI) object
#(t=temp, r=rainfall, p=ph, s=solar radiation, a=aridity)

voi<-s
plot(voi)

#calcualte Absolute range
abs<-voi
abs[voi<absMin]<-NA
abs[voi>absMax]<-NA
plot(abs)

#Calculate Score0
abs0<-abs
abs0[abs!=0]<-NA
plot(abs0)

#Calculate Score1
abs1<-abs
abs1[abs>optMin]<-NA
plot(abs1)

fun<-function(x) {((x-absMin)/range1)*100}
score1<-calc(abs1, fun) 
plot(score1)

#Calculate Score100 (Optimum Range)
opt<-voi
opt[voi<optMin]<-0
opt[voi>optMax]<-0
opt[opt!=0]<-100
opt[opt!=100]<-NA
plot(opt)

#Calculate Score2
abs2<-abs
abs2[abs<optMax]<-NA
plot(abs2)

fun<-function(x) {(((1-(x-optMax))/range2)*100)+100}
score2<-calc(abs2, fun)
#score2<-abs(score2)
plot(score2)

#>>>final suitability layer = combine score0 (abs0), score1, score100(opt), and score2
final<-merge(abs0, score1, opt, score2 )
plot(final,  main="Global solar radiation suitability of CropX")
plot(coast, add=TRUE) #need to set projections

#>>>assign object name to output
sSuit<-final
#tiff(sSuit, filename = "sSuit.tif")
writeRaster(sSuit, "CropX Suitability - Global Solar Radiation.tif", overwrite=TRUE )


##################################
##########ANALYSIS################
##################################

setwd("x")

#AC8.5
tSuit<-raster("CropX Global Suitability - Temperature ac8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall ac8.5 2070 900m.tif")

#AC4.5
tSuit<-raster("CropX Global Suitability - Temperature ac4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall ac4.5 2070 900m.tif")

#BC8.5
tSuit<-raster("CropX Global Suitability - Temperature bc8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall bc8.5 2070 900m.tif")


#BC4.5
tSuit<-raster("CropX Global Suitability - Temperature bc4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall bc4.5 2070 900m.tif")


#CC8.5
tSuit<-raster("CropX Global Suitability - Temperature cc8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall cc8.5 2070 900m.tif")

#CC4.5
tSuit<-raster("CropX Global Suitability - Temperature cc4.5 2070 900m.tif")
plot(tSuit)
rSuit<-raster("CropX Global Suitability - Rainfall cc4.5 2070 900m.tif")
plot(rSuit)

##CE8.5
tSuit<-raster("CropX Global Suitability - Temperature ce8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall ce8.5 2070 900m.tif")

##CE4.5
tSuit<-raster("CropX Global Suitability - Temperature ce4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall ce4.5 2070 900m.tif")

#CN8.5
tSuit<-raster("CropX Global Suitability - Temperature cn8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall cn8.5 2070 900m.tif")

#CN4.5
tSuit<-raster("CropX Global Suitability - Temperature cn4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall cn4.5 2070 900m.tif")


#GF8.5
tSuit<-raster("CropX Global Suitability - Temperature gf8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall gf8.5 2070 900m.tif")

#GF4.5
tSuit<-raster("CropX Global Suitability - Temperature gf4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall gf4.5 2070 900m.tif")

##GD8.5
tSuit<-raster("CropX Global Suitability - Temperature gd8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall gd8.5 2070 900m.tif")

##GD4.5
tSuit<-raster("CropX Global Suitability - Temperature gd4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall gd4.5 2070 900m.tif")

#GS8.5
tSuit<-raster("CropX Global Suitability - Temperature gs8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall gs8.5 2070 900m.tif")

#GS4.5
tSuit<-raster("CropX Global Suitability - Temperature gs4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall gs4.5 2070 900m.tif")

#HD8.5
tSuit<-raster("CropX Global Suitability - Temperature hd8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall hd8.5 2070 900m.tif")

#HD4.5
tSuit<-raster("CropX Global Suitability - Temperature hd4.5 2070 900m.tif")
plot(tSuit)
rSuit<-raster("CropX Global Suitability - Rainfall hd4.5 2070 900m.tif")
plot(sSuit)

#HG8.5
tSuit<-raster("CropX Global Suitability - Temperature hg8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall hg8.5 2070 900m.tif")

#HG4.5
tSuit<-raster("CropX Global Suitability - Temperature hg4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall hg4.5 2070 900m.tif")

#HE8.5
tSuit<-raster("CropX Global Suitability - Temperature he8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall he8.5 2070 900m.tif")

#HE4.5
tSuit<-raster("CropX Global Suitability - Temperature he4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall he4.5 2070 900m.tif")

#IN8.5
tSuit<-raster("CropX Global Suitability - Temperature in8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall in8.5 2070 900m.tif")

#IN4.5
tSuit<-raster("CropX Global Suitability - Temperature in4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall in4.5 2070 900m.tif")

#IP8.5
tSuit<-raster("CropX Global Suitability - Temperature ip8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall ip8.5 2070 900m.tif")

#IP4.5
tSuit<-raster("CropX Global Suitability - Temperature ip4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall ip4.5 2070 900m.tif")

#MI8.5
tSuit<-raster("CropX Global Suitability - Temperature mi8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall mi8.5 2070 900m.tif")

#MI4.5
tSuit<-raster("CropX Global Suitability - Temperature mi4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall mi4.5 2070 900m.tif")


#MR8.5
tSuit<-raster("CropX Global Suitability - Temperature mr8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall mr8.5 2070 900m.tif")

#MR4.5
tSuit<-raster("CropX Global Suitability - Temperature mr4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall mr4.5 2070 900m.tif")

#MC8.5
tSuit<-raster("CropX Global Suitability - Temperature mc8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall mc8.5 2070 900m.tif")

#MC4.5
tSuit<-raster("CropX Global Suitability - Temperature mc4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall mc4.5 2070 900m.tif")

#MP8.5
tSuit<-raster("CropX Global Suitability - Temperature mp8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall mp8.5 2070 900m.tif")

#MP4.5
tSuit<-raster("CropX Global Suitability - Temperature mp4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall mp4.5 2070 900m.tif")


#MG8.5
tSuit<-raster("CropX Global Suitability - Temperature mg8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall mg8.5 2070 900m.tif")

#MG4.5
tSuit<-raster("CropX Global Suitability - Temperature mg4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall mg4.5 2070 900m.tif")

#NO8.5
tSuit<-raster("CropX Global Suitability - Temperature no8.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall no8.5 2070 900m.tif")

#NO4.5
tSuit<-raster("CropX Global Suitability - Temperature no4.5 2070 900m.tif")
rSuit<-raster("CropX Global Suitability - Rainfall no4.5 2070 900m.tif")


pSuit<-raster("CropX Global Suitability - ph 30cm.tif")
dSuit<-raster("CropX Suitability - Global Drainage Class.tif")
sSuit<-raster("CropX Suitability - Global Solar Radiation.tif")

#align rasters (match projections) --this will take some time
tSuit<-projectRaster(tSuit, pSuit)
rSuit<-projectRaster(rSuit, pSuit)
sSuit<-projectRaster(sSuit, pSuit)
dSuit<-projectRaster(dSuit, pSuit)

##unify dimensions and resolutions
#tSuit<-resample(tSuit, pSuit)
#rSuit<-resample(rSuit, pSuit)
#stSuit<-resample(sSuit, pSuit)

#stack rasters
sStack<-stack(tSuit, rSuit, pSuit,sSuit, dSuit)
#sStack
#plot(sStack)
#writeRaster(sStack, "uluSStack.asc", overwrite=TRUE )

#find lowest cell values in raster stackn
suitFinal<-min(sStack)
plot(suitFinal, main="CropX Global Suitability - All Projected CC RCP8.5 Env. Variables")
writeRaster(suitFinal, "CropX Global Suitability - All Projected CC RCP8.5 Env. Variables.tif", overwrite=TRUE )



 #### Calculate Averages #####
#########################

RCP8.5
ac85<-raster("CropX Global Suitability - All Projected AC RCP8.5 Env. Variables.tif")
plot(ac85)
bc85<-raster("CropX Global Suitability - All Projected BC RCP8.5 Env. Variables.tif")
plot(bc85)
cc85<-raster("CropX Global Suitability - All Projected CC RCP8.5 Env. Variables.tif")
plot(cc85)
cn85<-raster("CropX Global Suitability - All Projected CN RCP8.5 Env. Variables.tif")
plot(cn85)
gf85<-raster("CropX Global Suitability - All Projected GF RCP8.5 Env. Variables.tif")
plot(gf85)
gs85<-raster("CropX Global Suitability - All Projected GS RCP8.5 Env. Variables.tif")
plot(gs85)
he85<-raster("CropX Global Suitability - All Projected HE RCP8.5 Env. Variables.tif")
plot(he85)
hd85<-raster("CropX Global Suitability - All Projected HD RCP8.5 Env. Variables.tif")
plot(hd85)
hg85<-raster("CropX Global Suitability - All Projected HG RCP8.5 Env. Variables.tif")
plot(hg85)
in85<-raster("CropX Global Suitability - All Projected IN RCP8.5 Env. Variables.tif")
plot(in85)
ip85<-raster("CropX Global Suitability - All Projected IP RCP8.5 Env. Variables.tif")
plot(ip85)
mi85<-raster("CropX Global Suitability - All Projected MI RCP8.5 Env. Variables.tif")
plot(mi85)
mr85<-raster("CropX Global Suitability - All Projected MR RCP8.5 Env. Variables.tif")
plot(mr85)
mc85<-raster("CropX Global Suitability - All Projected MC RCP8.5 Env. Variables.tif")
plot(mc85)
mp85<-raster("CropX Global Suitability - All Projected MP RCP8.5 Env. Variables.tif")
plot(mp85)
mg85<-raster("CropX Global Suitability - All Projected MG RCP8.5 Env. Variables.tif")
plot(mg85)
no85<-raster("CropX Global Suitability - All Projected NO RCP8.5 Env. Variables.tif")
plot(no85)


rcp85<-stack(ac85, bc85, cc85, cn85, gf85, gs85, he85, hd85, hg85, in85, ip85, mi85, mr85, mc85, mp85, mp85, no85)
rcp85 <- calc(rcp85, fun = mean, na.rm = T)
plot(rcp85)

writeRaster(rcp85, "CropX Global Suitability - All Projected RCP8.5 Env. Variables.tif", overwrite=TRUE )
writeRaster(rcp85, "RCP8.5 All Global Env. Variables.tif", overwrite=TRUE )


RCP4.5
ac45<-raster("CropX Global Suitability - All Projected AC RCP4.5 Env. Variables.tif")
plot(ac45)
bc45<-raster("CropX Global Suitability - All Projected BC RCP4.5 Env. Variables.tif")
plot(bc45)
cc45<-raster("CropX Global Suitability - All Projected CC RCP4.5 Env. Variables.tif")
plot(cc45)
cn45<-raster("CropX Global Suitability - All Projected CN RCP4.5 Env. Variables.tif")
plot(cn45)
gf45<-raster("CropX Global Suitability - All Projected GF RCP4.5 Env. Variables.tif")
plot(gf45)
gs45<-raster("CropX Global Suitability - All Projected GS RCP4.5 Env. Variables.tif")
plot(gs45)
he45<-raster("CropX Global Suitability - All Projected HE RCP4.5 Env. Variables.tif")
plot(he45)
hd45<-raster("CropX Global Suitability - All Projected HD RCP4.5 Env. Variables.tif")
plot(hd45)
hg45<-raster("CropX Global Suitability - All Projected HG RCP4.5 Env. Variables.tif")
plot(hg45)
in45<-raster("CropX Global Suitability - All Projected IN RCP4.5 Env. Variables.tif")
plot(in45)
ip45<-raster("CropX Global Suitability - All Projected IP RCP4.5 Env. Variables.tif")
plot(ip45)
mi45<-raster("CropX Global Suitability - All Projected MI RCP4.5 Env. Variables.tif")
plot(mi45)
mr45<-raster("CropX Global Suitability - All Projected MR RCP4.5 Env. Variables.tif")
plot(mr45)
mc45<-raster("CropX Global Suitability - All Projected MC RCP4.5 Env. Variables.tif")
plot(mc45)
mp45<-raster("CropX Global Suitability - All Projected MP RCP4.5 Env. Variables.tif")
plot(mp45)
mg45<-raster("CropX Global Suitability - All Projected MG RCP4.5 Env. Variables.tif")
plot(mg45)
no45<-raster("CropX Global Suitability - All Projected NO RCP4.5 Env. Variables.tif")
plot(no45)

rcp45<-stack(ac45, bc45, cc45, cn45, gf45, gs45, he45, hd45, hg45, in45, ip45, mi45, mr45, mc45, mp45, mp45, no45)

rcp45 <- calc(rcp45, fun = mean, na.rm = T)
plot(rcp45)

writeRaster(rcp45, "CropX Global Suitability - All Projected RCP4.5 Env. Variables.tif", overwrite=TRUE )

######
###CALCULATE DIFF####

#RCP8.5
now<-raster("CropX Global Suitability - All Env. Variables.tif")
plot(now)
now[now<0]<-NA
writeRaster(now, "CropX Global Suitability - All Env. Variables.tif", overwrite=TRUE )


diffNow85<-now-rcp85
plot(diffNow85)

diff85Now<-rcp85-now
writeRaster(diff85Now, "CropX Global Suitability - 50yr Change under RCP8.5.tif", overwrite=TRUE )


#RCP4.5
diff45Now<-rcp45-now
writeRaster(diff45Now, "CropX Global Suitability - 50yr Change under RCP4.5.tif", overwrite=TRUE )

##map rasters

gCoast<- readOGR("X.shp")

##now

mapTheme <- rasterTheme(region=brewer.pal(8,"RdYlGn"))
plt <- levelplot(now, margin=F, par.settings=mapTheme)
plt + layer(sp.lines(gCoast, col="black", lwd=0.5))


###RCP8.5
diff85Now<-raster("CropX Global Suitability - 50yr Change under RCP8.5.tif")
plot(diff85Now, main="2070 Increase/Decrease in 'CropX Suitability under RCP 8.5")
plot(gCoast, add=TRUE)

mapTheme <- rasterTheme(region=brewer.pal(8,"RdBu"))
plt <- levelplot(diff85Now, margin=F, par.settings=mapTheme)
plt + layer(sp.lines(gCoast, col="black", lwd=0.5))


######RCP4.5
diff45Now<-raster("CropX Global Suitability - 50yr Change under RCP4.5.tif")
plot(diff45Now)

mapTheme <- rasterTheme(region=brewer.pal(8,"RdBu"))
plt <- levelplot(diff45Now, margin=F, par.settings=mapTheme)
plt + layer(sp.lines(gCoast, col="black", lwd=0.5))

##Global Current
nnow<-raster("CropX Global Suitability - All Env. Variables.tif")
plot(nnow)
mapTheme <- rasterTheme(region=brewer.pal(8,"RdYlGn"))
plt <- levelplot(nnow, margin=F, par.settings=mapTheme)
plt + layer(sp.lines(gCoast, col="black", lwd=1)) 



##Global RCP8.5
rrcp85<-raster("CropX Global Suitability - All Projected RCP8.5 Env. Variables.tif")
plot(rcp85)
mapTheme <- rasterTheme(region=brewer.pal(8,"RdYlGn"))
plt <- levelplot(rrcp85, margin=F, par.settings=mapTheme)
plt + layer(sp.lines(gCoast, col="black", lwd=1)) 


#Global RCP4.5
rrcp45<-raster("CropX Global Suitability - All Projected RCP4.5 Env. Variables.tif")
plot(rrcp45)
mapTheme <- rasterTheme(region=brewer.pal(8,"RdYlGn"))
plt <- levelplot(rcp45, margin=F, par.settings=mapTheme)
plt + layer(sp.lines(gCoast, col="black", lwd=1)) 



#DENSITY PLOTS
#####stack projection rasters

rcp45<-raster("CropX Global Suitability - All Projected RCP4.5 Env. Variables.tif")
rcp85<-raster("CropX Global Suitability - All Projected RCP8.5 Env. Variables.tif")
now<-raster("CropX Global Suitability - All Env. Variables.tif")
plot(now)

plot(rcp85)
rcp85[rcp85<0]<-NA
writeRaster(rcp85, "CropX Global Suitability - All Projected RCP8.5 Env. Variables.tif", overwrite=TRUE) 

plot(rcp45)
rcp45[rcp45<0]<-NA
writeRaster(rcp45, "CropX Global Suitability - All Projected RCP4.5 Env. Variables.tif", overwrite=TRUE) 


allGlobal<-stack(rcp85, rcp45, now)
allGlobalDf<-as.data.frame(allGlobal)

###generate point files

pt85<-rasterToPoints(rcp85)
pt45<-rasterToPoints(rcp45)
ptNow<-rasterToPoints(now)

head(pt85)

write.csv(pt85, file="CropX RCP8.5 Points.csv")
write.csv(pt45, file="CropX RCP4.5 Points.csv")
write.csv(ptNow, file="CropX Current Points.csv")


###density plots

density(now)
density(rcp45)
density(rcp85)
density(allGlobal)


dNow<-density(now, plot=F)
dRcp45<-density(rcp45, plot=F)
dRcp85<-density(rcp85, plot=F)
#ylim<-range(d.koh$y, d.kon$y, d.kau$y)
#xlim<-range(d.koh$x, d.kon$x, d.kau$x)

plot(dNow, col="#000000", lty=1,
     xlab="", main="")
lines(dRcp45, col="orange", lty=2)
lines(dRcp85, col="#D55E00", lty=3, lwd=2)
legend("topleft", legend=c("Current", "RCP 4.5", "RCP 8.5"),
       lty=1:3, lwd=c(1,1,2), bty="n",
       col=c("#000000","orange","#D55E00"))

