#################################################
####MAUSIO Et al. BREADFRUIT SUITABILITY MODEL###
#################################################

#ALOHA! 
#This code can be used on both global and geographically local datasets
#as long as the rasters are defined as outlined below. ENJOY!!

#install.packages("googlesheets")

library(googlesheets) #<--initial setup will ask for long 
suppressMessages(library(dplyr))
library(raster)
library(rgdal)

###TO CLEAR TEMP FILES WHEN IT GETS TOO FULL
#tempfile()
dirname(tempdir())

###set values for each row that is marked: #>>>

#>>>set working directory
setwd("x") #<---insert working directory

#############CONNECT TO SPREADSHEET OF ENV. RANGES################
#>>>copy and paste googlesheet title
#>>>populate with cell ranges (top left corner, bottom right corner)
#>putting your environmental ranges on a google sheet allows for the easy management of data and development
#of a database of plants/crops, and takes out the need manually change figures in the code itself. 

suit<- gs_title("x") #<----instert google sheet ttle
suitAll<-suit %>% gs_read(ws = "Sheet1", range = cell_limits(c(1, 1), c(4, 21)), col_names = TRUE)
row.names(suitAll)<-suitAll$X1

############CONNECT TO RASTERS OF ENV, VARIABLES OF INTEREST (VOI) and basemap layers##################
#<<<connect each voi to rasters on file 
#(t=temp, r=rainfall, p=ph, s=solar radiation, d=drainage, n=Rnet, d=drainage, penPET=evapotranspiration)
t<-raster("x")
r<-raster("x")
p<-raster("x")
s<-raster("x")
#n<-raster("x")
d<-raster("x")
#penPET<-raster("x")
#scoast<-shapefile("x")

##################SET UP CODE VARIABLES###########################
##view spreadsheet
suitAll

######################
### temperature ######
######################

#>>>Identify the row of interest (roi): cropname+EcoCrop or cropname+LocalSources
roi<-subset(suitAll, row.names(suitAll) =="Rcode") #<- Rcode is the name of the top row in our google datasheet
                                                      #which is pasted in from our chosen crop's row of data. 
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
#plot(voi)

#Assigning the ranges, always double to check that the resulting raster comes out with a range 
#between 0-100. Sometimes you have to +/- a value to the equation to create this output. This is a
#happens when translating true values that vary from raster to raster to a standar fuzzy set (0-100).
#asign NAs to areas considered 'unsuitable'. 

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

fun<-function(x) {(((1-(x-optMax))/range2)*1000)}
score2<-calc(abs2, fun)
#score2<-abs(score2)
plot(score2)

#>>>final suitability layer = combine score0 (abs0), score1, score100(opt), and score2
final<-merge(abs0, score1, opt, score2 )
plot(final,  main="Temperature suitability of 'Ulu")
plot(coast, add=TRUE) #need to set projections

#>>>assign object name to output
tSuit<-final
#tiff(tSuit, filename = "tSuit.tif")
writeRaster(tSuit, "CropX Global Suitability - Temperature.tif", overwrite=TRUE )

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

#Assigning the ranges, always double to check that the resulting raster comes out with a range 
#between 0-100. Sometimes you have to +/- a value to the equation to create this output. This is a
#happens when translating true values that vary from raster to raster to a standar fuzzy set (0-100).
#asign NAs to areas considered 'unsuitable'. 

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
plot(final,  main="Rainfall global suitability of 'Ulu")
plot(coast, add=TRUE) #need to set projections

#>>>assign object name to output
rSuit<-final
#tiff(rSuit, filename = "rSuit.tif")
writeRaster(rSuit, "CropX Global Suitability - Rainfall.tif", overwrite=TRUE )

#>>>export file, change output title to reflect name of Variable of Interest (voi), and Crop of Interest (coi)
jpeg(file="Rainfall (850mm Absmin) suitability of CropX.jpeg")
plot(rSuit, main="Rainfall (850mm AbsMin) suitability of CropX")
plot(coast, add=TRUE)
dev.off()

######################
######## aridity ##########
######################

#We didnt use this in our final model but here is the code to calculate aridity.
#aridity equation is PET/raindfall

#plot(penPET)
#a<-penPET/r
#plot(a)
#writeRaster(a, "aridity.tif", overwrite=TRUE )


######################
######## ph ##########
######################

#>>>Identify the row of interest (roi): cropname+EcoCrop or cropname+LocalSources
roi<-subset(suitAll, row.names(suitAll) =="Rcode")
roi

#>>>Set variable by replacing with name variable of interest: temp, rain, ph or rad
absMin=roi$phAbsMin
absMax=roi$phAbsMax
optMin<-roi$phOptMin
optMax<-roi$phOptMax
range1<-optMin-absMin
range2<-absMax-optMax

#>>>Set the raster of the variable of interest (VOI) object
#(t=temp, r=rainfall, p=ph, s=solar radiation, a=aridity)

voi<-p
#plot(voi)

#Assigning the ranges, always double to check that the resulting raster comes out with a range 
#between 0-100. Sometimes you have to +/- a value to the equation to create this output. This is a
#happens when translating true values that vary from raster to raster to a standar fuzzy set (0-100).
#asign NAs to areas considered 'unsuitable'. 

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

fun<-function(x) {(((1-(x-optMax))/range2)*100+40)}
score2<-calc(abs2, fun)
#score2<-abs(score2)
plot(score2)

#>>>final suitability layer = combine score0 (abs0), score1, score100(opt), and score2
final<-merge(abs0, score1, opt, score2 )
plot(final,  main="ph global suitability for CropX")
plot(coast, add=TRUE) #need to set projections

#>>>assign object name to output
pSuit<-final
writeRaster(pSuit, "CropX Global Suitability - ph.tif", overwrite=TRUE )

#>>>export file, change output title to reflect name of Variable of Interest (voi), and Crop of Interest (coi)
tiff(file="ph suitability for CropX.tif")
plot(pSuit, main="ph suitability for CropX")
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

#Assigning the ranges, always double to check that the resulting raster comes out with a range 
#between 0-100. Sometimes you have to +/- a value to the equation to create this output. This is a
#happens when translating true values that vary from raster to raster to a standar fuzzy set (0-100).
#asign NAs to areas considered 'unsuitable'. 

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

######################
##### drainage #######
######################

#>>>Identify the row of interest (roi): cropname+EcoCrop or cropname+LocalSources
roi<-subset(suitAll, row.names(suitAll) =="Rcode")
roi

#>>>Set variable by replacing with name variable of interest: temp, rain, ph or rad
absMin=roi$drainAbsMin
absMax=roi$drainAbsMax
optMin<-roi$drainOptMin
optMax<-roi$drainOptMax
range1<-optMin-absMin
range2<-absMax-optMax

#>>>Set the raster of the variable of interest (VOI) object
#(t=temp, r=rainfall, p=ph, s=solar radiation, a=aridity)

voi<-d
plot(voi)

#Assigning the ranges, always double to check that the resulting raster comes out with a range 
#between 0-100. Sometimes you have to +/- a value to the equation to create this output. This is a
#happens when translating true values that vary from raster to raster to a standar fuzzy set (0-100).
#asign NAs to areas considered 'unsuitable'. 

#calcualte Absolute range
abs<-voi
#abs[abs==-9]<-1
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

fun<-function(x) {(((1-(x-optMax))/range2)*100)}
score2<-calc(abs2, fun)
#score2<-abs(score2)
plot(score2)

#>>>final suitability layer = combine score0 (abs0), score1, score100(opt), and score2
final<-merge(abs0, score1, opt, score2 )
plot(final,  main="Global drainage class suitability of CropX")
plot(coast, add=TRUE) #need to set projections

#>>>assign object name to output
dSuit<-final
#tiff(dSuit, filename = "dSuit.tif")
writeRaster(dSuit, "CropX Suitability - Global Drainage Class.tif", overwrite=TRUE )

#>>>export file, change output title to reflect name of Variable of Interest (voi), and Crop of Interest (coi)
jpeg(file="Drainage suitability of CropX.jpeg")
plot(pSuit30, main="Drainage suitability of CropX")
plot(coast, add=TRUE)
dev.off()

##################################
##########ANALYSIS################
##################################

setwd("y")

tSuit<-raster("y.tif")
rSuit<-raster("y.tif")
pSuit<-raster("y.tif")
dSuit<-raster("y.tif")
sSuit<-raster("y.tif")

#align rasters (match projections) --this will take some time
tSuit<-projectRaster(tSuit, pSuit)
rSuit<-projectRaster(rSuit, pSuit)
sSuit<-projectRaster(sSuit, pSuit)
dSuit<-projectRaster(dSuit, pSuit)

#stack rasters
sStack<-stack(tSuit, rSuit, pSuit,sSuit, dSuit)
sStack
plot(sStack)
#writeRaster(sStack, "uluSStack.asc", overwrite=TRUE )


#find lowest cell values in raster stack
suitFinal<-min(sStack)
plot(suitFinal, main="CropX Global Suitability - All Env. Variables")
writeRaster(suitFinal, "CropX Global Suitability - All Env. Variables.tif", overwrite=TRUE )

############################
#### WITHOUT RAINFALL ######
### Iriigation Effect ######
############################

#stack temp, pH, solar rad, drainage)
irrStack<-stack(tSuit, pSuit, sSuit, dSuit)
irrStack
plot(irrStack)

#find lowest cell values in raster stack
suitIrrigation<-min(irrStack)
plot(suitIrrigation)
writeRaster(suitIrrigation, "Ulu Suitability - Without rainfall.tif", overwrite=TRUE )

############################
####### WITHOUT pH #########
### Fertilization Effect ###
############################

#stack temp, rainfall, solar rad, drainage)
fertStack<-stack(tSuit, rSuit, sSuit, dSuit)
fertStack
plot(fertStack)

#find lowest cell values in raster stack
suitFertilisation<-min(fertStack)
plot(suitFertilisation)
writeRaster(suitFertilisation, "CropX Suitability - Without soil pH.tif", overwrite=TRUE )

########################################
####### WITHOUT raingfall + pH #########
### Irrigation + Fertilization Effect ##
########################################


#stack temp, solar rad, drainage)
irrFertStack<-stack(tSuit, sSuit, dSuit)
irrFertStack
plot(irrFertStack)

#find lowest cell values in raster stack
suitIrrFert<-min(irrFertStack)
plot(suitIrrFert)
writeRaster(suitIrrFert, "CropX Suitability - Without rainfall + soil pH.tif", overwrite=TRUE )

