#-------------      Loading of data packages   ---------------------#
# install.packages("remotes")
remotes::install_github("paleolimbot/qgisprocess")

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(tmap)
library(sf)
library(terra)
library(qgisprocess)
library(Rsagacmd)
library(rgrass7)
library(gdistance)
library(smoothr) #Delete this package
library(rmapshaper)
library("tools")
library(tidyr)
library(ggplot2)

#Simulated example


#Simulate cases for years 2016,2016,2017,2018,2019

#data$cases*
seq2015 = seq(from = 600, to = 1600, by = 3)
data$cases2015 = sample(seq2015, size =535, replace = TRUE)

seq2016 = seq(from = 600, to = 1600, by = 3)
data$cases2016 = sample(seq2016, size =535, replace = TRUE)

seq2017 = seq(from = 600, to = 1600, by = 3)
data$cases2017 = sample(seq2017, size =535, replace = TRUE)

seq2018 = seq(from = 600, to = 1600, by = 3)
data$cases2018 = sample(seq2018, size =535, replace = TRUE)

seq2019 = seq(from = 600, to = 1600, by = 3)
data$cases2019 = sample(seq2019, size =535, replace = TRUE)

seqE = seq(from = 600, to = 1600, by = 3)
data$expected = sample(seqE, size =535, replace = TRUE)

seqpop = seq(from = 500, to = 60000, by = 1)
data$population = sample(seqpop, size =535, replace = TRUE)

seqsir = seq(from = 0, to = 1, by = 0.001)
data$SIR = sample(seqsir, size =535, replace = TRUE)





#-------------      Setting up a working directory -----------------#

setwd("~/Documents/GISFolder/Institutions/MLW/Malaria/iTPA") ## iTPA Working directory on Patrick's computer

#-------------        Reading the data files         ---------------#

rdt <- read_csv("delimited/rdt.csv") # RDT data from DHIS2 
microscopy <- read_csv("delimited/microscopy.csv") #Microscopy data from DHIS2
moh <- read_csv("delimited/moh.csv") #The file was obtained from Donnie. The file was modified to add '-' for two facilities namely kayelekera and Grace 
malawi <- st_read("Vector/malawi/mwi_admbnda_adm0_nso_20181016.shp")
districts = st_read("Vector/malawi/mwi_admbnda_adm2_nso_20181016.shp")

#----TO BE REMOVED LATER
new_cat = st_read("Vector/catchments/catchments.shp")
old_cat = st_read("Vector/catchments/old/old_cat.shp")

## Cleaning of DHIS2 Data




#----------- convert moh file to shapefile
moh = na.omit(moh)
moh <- st_as_sf(moh, coords = c("X", "Y"), crs = 4326)
names(moh)[names(moh) == 'Name'] <- "facility"#Rename the facilities names to match DHIS2

#st_write(moh, "Vector/facilities/moh/facilities.shp")

#Join moh facilities with DHIS2 data

facilities.cases = right_join(moh, DHIS2, by ="facility")

#Plot facilities that we have on a map (include their districts)

tmap_mode("view")

tm_shape(districts)+
  tm_borders(col = "red")+
  tm_shape(facilities.cases) +
  tm_dots()+
  tm_shape(new_cat)+
  tm_borders()+
  tm_shape(old_cat)+
  tm_borders(col = "blue")



tm_shape(new_cat)+
  tm_borders(col = "red")




#---------   Remove some facilities from facilities.cases ------------------#

#Remove private hospitals i.e. those that do not report in DHIS2
#--- Lilongwe
facilities.cases = filter(facilities.cases, facility != "Partners In Hope Clinic Moyo Clinic (public)") 
facilities.cases = filter(facilities.cases, facility != "Nkhalango Private Clinic") 
facilities.cases = filter(facilities.cases, facility != "Mlodza SDA Dispensary") 
facilities.cases = filter(facilities.cases, facility != "Dr David Livingstone Memorial Clinic") 
facilities.cases = filter(facilities.cases, facility != "City Assembly Dispensary")
facilities.cases = filter(facilities.cases, facility != "Blessings Hospital")


#---- Blantyre

facilities.cases = filter(facilities.cases, facility != "Mtengoumodzi Private Hospital") 
facilities.cases = filter(facilities.cases, facility != "Nyambadwe Private Hospital") 
facilities.cases = filter(facilities.cases, facility != "Malamulo Day Clinic") 
facilities.cases = filter(facilities.cases, facility != "Kanjedza Police Clinic") 
facilities.cases = filter(facilities.cases, facility != "Polytechnic Clinic")
facilities.cases = filter(facilities.cases, facility != "Chichiri Prison Clinic")
facilities.cases = filter(facilities.cases, facility != "Blantyre Civic Centre Dispensary")
facilities.cases = filter(facilities.cases, facility != "Blantyre City Assembly Clinic")
facilities.cases = filter(facilities.cases, facility != "Chichiri Escom Clinic")
facilities.cases = filter(facilities.cases, facility != "Blantyre Adventist Hospital")
facilities.cases = filter(facilities.cases, facility != "Shifa Private Clinic")
facilities.cases = filter(facilities.cases, facility != "Mtengoumodzi Private Hospital")
facilities.cases = filter(facilities.cases, facility != "Mwaiwathu Private Hospital")

# Zomba

facilities.cases = filter(facilities.cases, facility != "Ahi Private Clinic")
facilities.cases = filter(facilities.cases, facility != "Chinamwali Private Clinic")


#Mzuzu

#Convert the cases from wide to long
#--- Transform the cases from wide to long
facilities.cases.long = facilities.cases
facilities.cases.long = gather(facilities.cases.long, key = "date", value = "cases", 5:88)

#------ for the case of modelling, lets only extract one date 

facilities.cases.long.december = filter(facilities.cases.long, date == "Dec_2020_01")

#Lets begin by creating a continous surface through simple interpolation 
#library(devtools)
#install.packages("~/Documents/GISFolder/Resources/tools/r/packages/gstat_1.1-6.tgz")

#library(gstat)
#g = gstat(formula =  cases~ 1, data = facilities.cases.long.december)
library(spatstat)

b = facilities.cases.long.december
write_sf(b, "outputs/december2.shp")

december <- b %>%
  dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                latitude = sf::st_coordinates(.)[,2])

december_malaria<-ppp(december$longitude,december$latitude,
                 marks=december$cases,window=obs_window)

#Plot facilities that we have on a map (include their catchment areas)
tmap_mode("view")
tm_shape(facilities.cases.long) +
  tm_dots()#+


#LIMITATIONS


#-------------------------------------------------------------------------------#
#                       Model fitting                                           #
#-------------------------------------------------------------------------------#

#We estimate the risk of malaria (2021) across Malawi health facilities using R-INLA package

# Data manipulation, transformation and visualisation
library(tidyverse)
# Nice tables
library(kableExtra)
# Simple features (a standardised way to encode vector data ie. points, lines, polygons)
library(sf) 
# Spatial objects conversion
library(sp) 
# Thematic maps
library(tmap) 
# Nice colour schemes
library(viridis) 
# Obtain correlation coefficients
library(corrplot)
# Highlight data on plots
library(gghighlight)
# Analysing spatio-temporal data
#library(STRbook)
library(spacetime)
# Date parsing and manipulation
library(lubridate)
# Applied statistics
library(MASS)
# Statistical tests for linear regression models
library(lmtest)

#-----Installations----#
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(devtools)
install_github("andrewzm/FRK", dependencies = FALSE, build_vignettes = TRUE)

# Fit spatial random effects models
library(FRK)
# Exportable regression tables
library(jtools)



#------------ Prepare data for modelling
#--------------------------------------------------#
#------ generate catchment areas------------------#


FacilityCatchment <- function (frictionsurface, points, malawi, sabela) {
  
  transition.matrix.exists.flag <- 0 # if the geo-corrected graph has already been made, this can save time.  Uses the same T.GC.filename as specified using the T.GC.filename variable.
  # Output Files
  T.filename <- '~\\outputs\\study.area.T.rds'
  T.GC.filename <- '~\\outputs\\study.area.T.GC.rds'
  output.filename <- '~\\outputs\\study.area.accessibility2.tif'
  
  
  # Fetch the number of points
  temp <- dim(points)
  n.points <- temp[1]
  
  # Make the graph and the geocorrected version of the graph (or read in the latter).
  if (transition.matrix.exists.flag == 1) {
    # Read in the transition matrix object if it has been pre-computed
    T.GC <- readRDS(T.GC.filename)
  } else {
    # Make and geocorrect the transition matrix (i.e., the graph)
    T <- transition(frictionsurface, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
    saveRDS(T, T.filename)
    T.GC <- geoCorrection(T)                    
    saveRDS(T.GC, T.GC.filename)
  }
  
  # Convert the points into a matrix
  xy.data.frame <- data.frame()
  xy.data.frame[1:n.points,1] <- points[,1]
  xy.data.frame[1:n.points,2] <- points[,2]
  xy.matrix <- as.matrix(xy.data.frame)
  
  # Run the accumulated cost algorithm to make the final output map. This can be quite slow (potentially hours).
  temp.raster <- accCost(T.GC, xy.matrix)
  cost <- temp.raster
  #Disaggregate raster cells for cost
  #cost <- disagg(cost, fact = 4, method ="bilinear")
  
  #---Output
  #final = #'Vector/facilities2/test3.tif'
  
  catchments = qgis_run_algorithm("grass7:r.cost", input = cost, start_points = sabela)
  #                           tool = 10, threshold = 25000)
  
  a1 = qgis_as_terra(catchments$nearest)
  a2 = qgis_as_terra(catchments$outdir)
  a3 = qgis_as_terra(catchments$output)
  
  b=plot(a1)
  
  
  # Mask and Clip Malawi
  
  #v <- project(malawi,a1)
  final = mask(crop(a1,malawi),malawi)
  
  #Vectorize
  catchmentsHosp = as.polygons(final) %>% 
    st_as_sf()
  
  #Make ths shapefile smooth
  #library(smoothr)
  #library(rmapshaper)
  
  catchmentsHosp_simp2 = ms_simplify(catchmentsHosp, keep = 0.1)
  
  #Convert it to a format that can enable ploting with tmap
  a = vect(catchmentsHosp_simp2)
  a = st_as_sf(a)
  #a = plot(a)
  #Assign the generated catchments names
  a = st_join(a,sabela)
  a <- subset(a, select = -min)
  
  
  
  return(a)
}

#---- Data for this task

a =facilities.cases

b = as.data.frame(st_coordinates(facilities.cases))

b = rename(b, X_COORD = X, Y_COORD = Y)

write.csv(b, file = "delimited/points.csv", row.names = FALSE)

sabela =facilities.cases[,1:4]

frictionsurface <- raster("Raster/traveldistance/frictionsurface/frictionsurface.tiff") #Read Friction surface
# change resolution of raster to 100 meters 0.0009009009

#disaggregate from 40x40 resolution to 10x10 (factor = 9)
frictionsurface <- disaggregate(frictionsurface, fact=2)
res(frictionsurface)


points <- read.csv("delimited/points.csv") # Read in the points table
sabela =facilities.cases[,1:4]
catchmentsHosp_simp2 <- FacilityCatchment(frictionsurface, points, malawi, sabela)
catchments = catchmentsHosp_simp2[,-c(2,3)]

#Extract human population for the catchments
pop.2020 = raster("raster/population/mwi_ppp_2020.tif")

test<- terra::extract(pop.2020, catchments)

#Extract Elevation 
elevation = raster("raster/elevation/s15_e033_1arc_v3.tif")

#Extract average temperature
library(ncdf4) # package for netcdf manipulation


#Temperature data
temp.data <- nc_open('raster/temperature/climatology-tas-monthly-mean_cru_monthly_cru-ts4.06-climatology_mean_1991-2020.nc')
# Save the print(nc) dump to a text file
{
  sink('climatology-tas-monthly-mean_cru_monthly_cru-ts4_metadata.txt')
  print(temp.data)
  sink()
}


#Altenatively, we can consult MET departnment and intepolate temperature data for each and every month for the country



#Change from wide to long
# separate date variable month and year variables
facilities.cases$day <- day(covid19$date)
facilities.cases$month <- month(facilities.cases)
facilities.cases$year <- year(facilities.cases$date)




data = final
data <- st_drop_geometry(data)
data.final= left_join(data, combined.annual, by='facility') 
  
#Monthlky rainfall 
#---------- Start with one month (ignoring temporal aspect)
#Filter cases for 2021-12-01
december = filter(malaria_cases, date == "2021-12-01")
sabela = left_join(catchments, december, by='facility') 
december.data =sabela

#-----------Calculating Standard Incidence ratio (SIR)

#Interprating

#----------- Fitting BEsag-York Mollie (BYM) model to obtain relative risk estimates and quatify the effects of variables




#-----------Calculating S

#QGIS
write_sf(sabela, "temp/sabela.shp")
#create centroids
test = read_sf("temp/centroids.shp")

#Extract lat and long
test <- test %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])

library(INLA)

coo <- cbind(test$lon, test$lat)
mesh <- inla.mesh.2d(
  loc = coo, max.edge = c(0.1, 5),
  cutoff = 0.01
)

#----------      Possible explanatory variables for differences in transmissions
#1.	Altitude
#-------- USGS

#2.	Temperature
#------- Bioclim

#3.	Humidity
#------- Bioclim

#4.	Rainfall patterns
#------- Bioclim (precipitation)

#5.	Proximity to water bodies

#6.	Land use
#-------- ESA landcover maps

#7.	Vector distribution
#8.	Social demographic characteristics
#9.	Access to anti malaria treatment
#10.	Implementation of vector control programs

## THIS SCRIPT ESTIMATES HUMAN POPULATION IN THE CATCHMENTS AND CALCULATE ANNUAL CASES PER 1000


#This script add all covariates 


#Modelling
#This script perfom statistical modelling on the dataset



test <- read.csv("delimited/test_district.csv") 
test2 = spread(test, key = year, value = TOTAL)
write.csv(test2,"delimited/cases.csv")

