#Full

## STEP ONE

#----- This script helps cleaning of DHIS 2 data to be ready for joining with spatial data for health facilities across Malawi 

#-------------      Loading of data packages   ---------------------#

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




#-------------      Setting up a working directory -----------------#

setwd("~/Documents/GISFolder/Institutions/MLW/Malaria/iTPA") ## iTPA Working directory on Patrick's computer

#-------------        Reading the data files         ---------------#

rdt <- read_csv("delimited/rdt.csv") #DHS RDT 
microscopy <- read_csv("delimited/microscopy.csv") #DHS Microscopy

rdt2 <- rdt

## Cleaning of DHIS2 Data


#-----------------------------------------------------------------------------------------------------------#
#----------------     PROCEDURE FOR CLEANING DHIS2 DATA   --------------------------------------------------#
#-----------------------------------------------------------------------------------------------------------

## First, looking at the data from DHIS2. Facility names have a prefix indicating the outcome being presented. This prefix has to be removed 


DHIS2cleaning <- function (outcome) {
  #Remove unnecessary columns for dates
  outcome <- outcome[, !(colnames(outcome) %in% c("periodid", "periodcode", "perioddescription"))]
  #Rename prefix
  outcome <- outcome %>% 
    rename_all(~stringr::str_replace(.,"NMCP OPD Confirmed Malaria Cases Through RDT ",""))%>%
    rename_all(~stringr::str_replace(.,"NMCP OPD Confirmed Malaria Cases Through Microscopy ",""))
  
  #rename period
  names(outcome)[1] <- "period"
  #Transform the data from wide to long
  outcome <- outcome %>% gather(facility, cases, -period)
  
  #Format facilities as a factor, not a character
  #outcome[,'facility'] <- as.factor(outcome[,'facility'])
  #Split time properly
  
  # Split period column into month and year
  outcome[c('month', 'year')] <- str_split_fixed(outcome$period, '-', 2)
  
  #drop the column period
  outcome <- within(outcome, rm(period))
  
  #Add 20 to reflect the year correctly
  outcome$pref <- "20"
  outcome$year <- paste(outcome$pref, outcome$year, sep ="")
  
  #drop prefix
  outcome <- within(outcome, rm(pref))
  
  #Combinbe date 
  outcome <- unite(outcome, date, c(month,year))
  
  #Make it a date
  outcome$date <- as.Date(paste0(outcome$date, '_1'), '%b_%Y_%d')
  outcome$date <-format(outcome$date, "%h_%Y_%d")
  
  return(outcome)
}

#----- applying the function on DHIS2 data

rdt <- DHIS2cleaning(rdt2)  #--------RDT confirmed cases
microscopy <- DHIS2cleaning(microscopy)  #--------Microscopy confirmed cases

#------------- rename cases column to reflect the outcome measure
names(rdt)[names(rdt) == 'cases'] <- "rdt"
names(microscopy)[names(microscopy) == 'cases'] <- "microscopy"

#-------------Combining RDT and microscopy data
#In order to get the total number of cases per facility 
#we have to combine RDT to microscopy dataset

malaria_data <- rdt %>%
  inner_join(microscopy, by=c("facility","date"))%>%
  mutate(cases = rowSums(across(c(rdt, microscopy)))) 

malaria_data = dplyr::select (malaria_data,-c(microscopy,rdt))

malaria2 = malaria_data #To be replaced by combined dataset RDT and microsocopy
malaria2$facility=as.factor(malaria2$facility)
#
malaria2 =spread(malaria2, date, cases)
list(malaria2$facility)
#Get a list of facilities on DHIS 2

list(malaria2$facility)

head(select(malaria2, -facility))

#Get a list of facilities that have no data (NA's)

p = as.vector(colnames(malaria2))
p = p[p!= "facility"]

#--------- Facilities that do not report data on malaria cases

data_na <- malaria2 %>% 
  filter_at(vars(p),all_vars(is.na(.)))

#---------- Facilities that have reported data on malaria cases

data_no_na <- malaria2 %>% 
  filter_at(vars(p),any_vars(!is.na(.)))

# This CSV is to be used for joining and other operations

write.csv(data_no_na,"MLW_iTPA/delimited/malaria_wide.csv")





## STEP TWO

#----- This script uses latest ministry of health location data and assess if we have location data for all health facilities that reports data on DHIS 2 
#The output of the script is a shapefile with locations for majority of the facilities on DHIS2
# Missing facilities have been sourced from other sources


#-------------      Loading of data packages   ---------------------#
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


setwd("~/Documents/GISFolder/Institutions/MLW/Malaria/iTPA/MLW_iTPA") ## iTPA





#---- data files

DHIS2 <- read_csv("delimited/malaria_wide.csv") #DHIS2
moh <- read_csv("delimited/moh.csv") #The file was obtained from Donnie. The file was modified to add '-' for two facilities namely kayelekera and Grace 
malawi <- st_read("Vector/malawi/mwi_admbnda_adm0_nso_20181016.shp")
districts = st_read("Vector/malawi/mwi_admbnda_adm2_nso_20181016.shp")


#----TO BE REMOVED LATER
new_cat = st_read("Vector/catchments/catchments.shp")
old_cat = st_read("Vector/catchments/old/old_cat.shp")




##facilities <- st_read("Vector/facilities/health_facilities.shp") #Chifuniro shapefile
##facilities_rev <- st_read("Vector/facilities/extra_facilities/extra_facilities.shp") #Donnies shapefile


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

#Locate the ones without coordinates
zilibe <- facilities.cases[is.na(facilities.cases$Geometry),]
#The cordinates of such facilities will be located outside R environment
#Google Search has been perfomed
#They have been checked against existing facility from MSF

zilibe = subset(zilibe, select = facility )
zilibe = st_set_geometry(zilibe, NULL)
write.csv(zilibe, file = "delimited/missing_facilities.csv")

#There are 68 facilities without coordinates

#----- Write outputs needed for catchment area estimation

#Delete files before writting
#Add a logical operator here

#files  = list.files("Vector/facilities/latest", full.names = TRUE, recursive = TRUE)
#file.remove(files)

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
#--NONE






#---------------------------------------#
a =facilities.cases

b = as.data.frame(st_coordinates(facilities.cases))

b = rename(b, X_COORD = X, Y_COORD = Y)

write.csv(b, file = "delimited/points.csv", row.names = FALSE)


## STEP THREE

#----- This script uses location data from step 2 to support generation of proxy catchments


#-------------        Reading the data files         ---------------#


frictionsurface <- raster("Raster/traveldistance/frictionsurface/frictionsurface.tiff") #Read Friction surface
# change resolution of raster to 100 meters 0.0009009009

#disaggregate from 40x40 resolution to 10x10 (factor = 9)
frictionsurface <- disaggregate(frictionsurface, fact=2)
res(frictionsurface)


points <- read.csv("delimited/points.csv") # Read in the points table
sabela = facilities.cases
#sabela <- st_read("../test/Vector/facilities2/facilities2.shp") #malaria fac
#The above file will be updated as new files are added



library(tmap)
library(rmapshaper)

#Plot facilities that we have on a map (include their catchment areas)
tmap_mode("view")
tm_shape(sabela) +
  tm_dots()#+



## Generation of Proxy facility catchment areas



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

catchmentsHosp_simp2 <- FacilityCatchment(frictionsurface, points, malawi, sabela)


#Plot facilities that we have on a map (include their catchment areas)
tmap_mode("view")
tm_shape(catchmentsHosp_simp2) +
  tm_polygons()



#Improve catchment area estimation for urban areas


tm_shape(districts)+
  tm_borders(col = "red")+
  tm_shape(facilities.cases) +
  tm_dots()+
  tm_shape(catchmentsHosp_simp2)+
  tm_borders()+
  tm_shape(old_cat)+
  tm_borders(col = "blue")

catchmentsHosp_simp2 <- st_collection_extract(catchmentsHosp_simp2, "POLYGON")
catchmentsHosp_simp2<- st_zm(catchmentsHosp_simp2, drop=T, what='ZM')


st_write(catchmentsHosp_simp2, "vector/catchments/catchment_Oct_26/catchment.gpkg")




## STEP 4

#------Subset output of step two into annual malaria cases and geographical data
malaria_cases = catchmentsHosp_simp2 
malaria_cases = st_set_geometry(malaria_cases, NULL) #drop geometry (maintain cases and facility name)
malaria_cases  = subset(malaria_cases , select = -c(ID,Geometry,...1 ) )


catchments  = subset(catchmentsHosp_simp2, select = facility )


#------------------- annual malaria cases -------------------------------------#

#--- Transform the cases from wide to long
malaria_cases = gather(malaria_cases, key = "date", value = "cases", 2:85)

#Convert date to date format
#malaria_cases$date = lubridate::ymd(malaria_cases$date)

#Make it a date
malaria_cases$date <- as.Date(malaria_cases$date, '%b_%Y_%d')

malaria_cases <- malaria_cases %>%
  mutate(year = year(malaria_cases$date))
malaria_cases.year  = subset(malaria_cases , select = -date)

#Subset cases for each year
cases.2015 = filter(malaria_cases.year, year == 2015)
cases.2016 = filter(malaria_cases.year, year == 2016)
cases.2017 = filter(malaria_cases.year, year == 2017)
cases.2018 = filter(malaria_cases.year, year == 2018)
cases.2019 = filter(malaria_cases.year, year == 2019)
cases.2020 = filter(malaria_cases.year, year == 2020)

#Aggregate annual cases per facility
library(dplyr)

#remove nas
cases.2015 = cases.2015[!(is.na(cases.2015$cases)),]
cases.2016 = cases.2016[!(is.na(cases.2016$cases)),]
cases.2017 = cases.2017[!(is.na(cases.2017$cases)),]
cases.2018 = cases.2018[!(is.na(cases.2018$cases)),]
cases.2019 = cases.2019[!(is.na(cases.2019$cases)),]
cases.2020 = cases.2020[!(is.na(cases.2020$cases)),]

sum.2015 = cases.2015 %>% 
  group_by(facility) %>% 
  summarise(total_2015 = sum(cases))

sum.2016 = cases.2016 %>% 
  group_by(facility) %>% 
  summarise(total_2016 = sum(cases))

sum.2017 = cases.2017 %>% 
  group_by(facility) %>% 
  summarise(total_2017 = sum(cases))

sum.2018 = cases.2018 %>% 
  group_by(facility) %>% 
  summarise(total_2018 = sum(cases))

sum.2019 = cases.2019 %>% 
  group_by(facility) %>% 
  summarise(total_2019 = sum(cases))

sum.2020 = cases.2020 %>% 
  group_by(facility) %>% 
  summarise(total_2020 = sum(cases))


#combine the dataset for all the years 


library(tidyverse)
#list
years_list <- list(sum.2015, sum.2016, sum.2017, sum.2018, sum.2019)

#merge all data frames in list
combined.annual = left_join(sum.2020, sum.2019, by='facility') 
combined.annual = left_join(combined.annual, sum.2018, by='facility') 
combined.annual = left_join(combined.annual, sum.2017, by='facility') 
combined.annual = left_join(combined.annual, sum.2016, by='facility') 
combined.annual = left_join(combined.annual, sum.2015, by='facility') 

#--------- summarize malaria cases per facility

#Population estimation was done in QGIS and it will be done here

catchments <- st_read("Vector/catch/pop2015.shp")

#----- Combine data ---------------------#

final = left_join(catchments, combined.annual, by='facility') 

#---- Compute cases per 1000
#divide cases with population
final$case2015 = final$total_2015/final$pop2015
final$case2015 = final$case2015*1000
final$case2016 = 1000*final$total_2016/final$pop2016
final$case2017 = 1000*final$total_2017/final$pop2017
final$case2018 = 1000*final$total_2018/final$pop2018
final$case2019 = 1000*final$total_2019/final$pop2019
final$case2020 = final$total_2020/final$pop2020
final$case2020 = final$case2020*1000

#--------     Visualize cases per 1000 on a map

malawi.districts = tm_shape(districts) +
  tm_borders(col ="blue")

map.2015 = tm_shape(final) +
  tm_fill("case2020",title="WHO Scheme",breaks=c(0,100,250,450,1000),palette="Reds")  +
  tm_borders() +
  tm_layout(title = "Malaria cases in 2015", title.position = c("right","bottom"))+
  malawi.districts

map.2016 = tm_shape(final) +
  tm_fill("case2016",title="WHO Scheme",breaks=c(0,100,250,450,1000),palette="Reds")  +
  tm_borders() +
  tm_layout(title = "Malaria cases in 2016", title.position = c("right","bottom"))+
  malawi.districts

map.2017 = tm_shape(final) +
  tm_fill("case2017",title="WHO Scheme",breaks=c(0,100,250,450,1000),palette="Reds")  +
  tm_borders() +
  tm_layout(title = "Malaria cases in 2017", title.position = c("right","bottom"))+
  malawi.districts

map.2018 = tm_shape(final) +
  tm_fill("case2018",title="WHO Scheme",breaks=c(0,100,250,450,1000),palette="Reds")  +
  tm_borders() +
  tm_layout(title = "Malaria cases in 2018", title.position = c("right","bottom"))+
  malawi.districts

map.2019 = tm_shape(final) +
  tm_fill("case2019",title="WHO Scheme",breaks=c(0,100,250,450,1000),palette="Reds")  +
  tm_borders() +
  tm_layout(title = "Malaria cases in 2019", title.position = c("right","bottom"))+
  malawi.districts

map.2020 = tm_shape(final) +
  tm_fill("case2020",title="WHO Scheme",breaks=c(0,100,250,450,1000),palette="Reds")  +
  tm_borders() +
  tm_layout(title = "Malaria cases in 2020", title.position = c("right","bottom"))+
  malawi.districts

map.2015
map.2016
map.2017
map.2018
map.2019
map.2020

#LIMITATIONS

# Make functions that will be used in future work
#Make all work reproducible (population estimation should be done in R using Raster or Terra package)
#Stratify cases at monthly level (WHO scheme will be too big)
#Locate coordinates for missing facilities
#Improve population estimation for cities 
#leess people than caserd

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

