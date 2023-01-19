#----- This script helps cleaning of DHIS 2 data to be ready for joining with spatial data for health facilities across Malawi 

#-------------------------------------------------------------------------------#
#                       Data Cleaning                                           #
#-------------------------------------------------------------------------------#

#----   Loading of data packages  

pacman::p_load(readr,               # Spatial files
               lubridate,        # File locator
               dplyr,    # data management + ggplot2 graphics
               SpatialEpi,      # handle time series datasets
               ggplot2,       # for calculating moving averages
               caret,     # for filling in missing values
               tictoc,       # for time series decomposition and autocorrelation
               varImp,     # fit sin and cosin terms to data (note: must load after feasts)
               mlbench,     # fit and assess models 
               tidymodels,    # for getting geocoordinates (lon/lat) based on place names
               gghighlight,       # for interacting with copernicus sateliate CDS API
               plotly,
               cvms,
               dplyr,
               klaR
               
)


#-------------      Loading of data packages   ---------------------#

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library("tools")
library(tidyr)
library(ggplot2)

#-------------      Setting up a working directory -----------------#

#setwd("~/Documents/GISFolder/Institutions/MLW/Malaria/iTPA") ## iTPA Working directory on Patrick's computer

#-------------        Reading the data files         ---------------#

rdt <- read_csv("inputs/DHIS2/rdt.csv") #DHS RDT 
microscopy <- read_csv("inputs/DHIS2/microscopy.csv")#DHS Microscopy

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

rdt <- DHIS2cleaning(rdt)  #--------RDT confirmed cases
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

#Create a nested function for the process above
#-------------
## ensure the date column is in the appropriate format
#malaria_data$date <- as.Date(malaria_data$date)
library(tidyverse)
# Convert to class date
malaria_data <- malaria_data %>% 
  mutate(date.final = as.Date(date, format = "%b_%Y_%d"))

#remove na's

malaria.final = na.omit(malaria_data)
malaria.final =subset(malaria.final, select = -date)

#renamne date

colnames(malaria.final)[colnames(malaria.final) == "date.final"] = "date"


#Catchments should be computed before this step
#The process was done already
library(readr)
population <- read_csv("inputs/population/population.csv")

#--- join population to DHIS2 data

malaria.data = malaria.final %>%
  inner_join(population, by = "facility")

#----- Computing expected cases

library(SpatialEpi)
n.strata <- 1
malaria.data$E <- expected(
  population = malaria.data$pop2020,
  cases = malaria.data$cases,
  n.strata = n.strata
)

#The expected number of cases was expected to vary with time, the code below allows that.
#More rows are created and this is making join operations to be challenging
#Get uniqie dates and facilities
#ndates <- length(unique(malaria.data$date))
#facilitiesE <- rep(unique(malaria.data$facility),
 #                each = ndates)

#nfacilities <- length(unique(malaria.data$facility))
#datesE <- rep(unique(malaria.data$date),
 #             times = nfacilities)

#izo <- data.frame(facility = facilitiesE, date = datesE, E = E)

#head(dE)

#----- calculating SIR

malaria.data$SIR = malaria.data$cases/malaria.data$E

#--- Preparing the data for mapping

#Facility + date + E

malaria.data.E = subset(malaria.data, select = c(facility, date, E))
malaria.data.EW = spread(malaria.data.E, date, E)
colnames(malaria.data.EW) <-paste("E",colnames(malaria.data.EW),sep=".")
names(malaria.data.EW)[names(malaria.data.EW) == 'E.facility'] <- "facility"

#Facility + date + SIR

malaria.data.SIR = subset(malaria.data, select = c(facility, date, SIR))
malaria.data.SIRW = spread(malaria.data.SIR, date, SIR)
colnames(malaria.data.SIRW) <-paste("SIR",colnames(malaria.data.SIRW),sep=".")
names(malaria.data.SIRW)[names(malaria.data.SIRW) == 'SIR.facility'] <- "facility"

#Facility + date + cases
malaria.data.cases = subset(malaria.data, select = c(facility, date, cases))
malaria.data.casesW = spread(malaria.data.cases, date, cases)
colnames(malaria.data.casesW) <-paste("cases",colnames(malaria.data.casesW),sep=".")
names(malaria.data.casesW)[names(malaria.data.casesW) == 'cases.facility'] <- "facility"

#Facility + date + pop

malaria.data.pop = subset(malaria.data, select = c(facility, date, pop2020))
malaria.data.popW = spread(malaria.data.pop, date, pop2020)
colnames(malaria.data.popW) <-paste("pop",colnames(malaria.data.popW),sep=".")
names(malaria.data.popW)[names(malaria.data.popW) == 'pop.facility'] <- "facility"

#Combine the columns
malaria.mapping = merge(malaria.data.EW,malaria.data.SIRW, by="facility")
malaria.mapping = merge(malaria.mapping,malaria.data.casesW, by="facility")
malaria.mapping = merge(malaria.mapping,malaria.data.popW, by="facility")

write_csv(malaria.mapping, file = "outputs/delimited/dhis_wide.csv")
write_csv(malaria.data, file = "outputs/delimited/malariadata.csv")

#----- Temporal changes
g <- ggplot(malaria.data, aes(x = date, y = SIR, 
                   group = facility, color = facility)) +
  geom_line() + geom_point(size = 2) + theme_bw()
g

g <- g + theme(legend.position = "none")
g

#Comparing Kasungu district hospital to the rest

library(gghighlight)
g + gghighlight(facility == "Kasungu District Hospital")

#--- Interactive map

library(plotly)
ggplotly(g)

######-------------------------   End ----------------------##############

#--- Spread doesnt work
malaria.data %>% 
  group_by(ID) %>% 
  mutate(Visit = 1:n())

malaria.data.wide = reshape(malaria.data,
                            timevar = "date",
                            idvar = "facility",
                            direction = "wide")

malaria.data.wide[1:2, ]
malaria.data.wide2 = spread(malaria.data, date, cases)

#summarize by date

temp_data <- malaria_data2 %>% 
  group_by(date.final) %>% 
  summarise(cases = as.numeric(sum(cases)))

## plot a line graph of cases by week
ggplot(temp_data, aes(x = date.final, y = cases)) + 
  geom_line()

# ----------- Restructuring the data for spatial operations

malaria2 = malaria_data2 #To be replaced by combined dataset RDT and microsocopy
malaria2$facility=as.factor(malaria2$facility)

#Nested function should be created for the process above

malaria2 =spread(malaria2, date.final, cases)

#Get a list of facilities on DHIS 2
list(malaria2$facility)

#Get a list of facilities that have no data (NA's)

p = as.vector(colnames(malaria2))
p = p[p!= "facility"]

#--------- Facilities that do not report data on malaria cases

data_na <- malaria2 %>% 
  filter_at(vars(p),all_vars(is.na(.)))

#---------- Facilities that have reported data on malaria cases

data_no_na <- malaria2 %>% 
  filter_at(vars(p),any_vars(!is.na(.)))

# This file CSV is to be used for joining and other operations
DHIS2  = data_no_na
write.csv(DHIS2, "outputs/delimited/cleaned.csv")

#Produces a wide csv that will be joined to the catchment areas
