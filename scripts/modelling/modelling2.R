#-------------------------------------------------------------------------------#
#                       Model fitting                                           #
#-------------------------------------------------------------------------------#

#----   Loading of data packages  

pacman::p_load(sf,               # Spatial files
               lubridate,        # File locator
               tmap,    # data management + ggplot2 graphics
               SpatialEpi,      # handle time series datasets
               ggplot2,       # for calculating moving averages
               rgdal,     # for filling in missing values
               gganimate,       # for time series decomposition and autocorrelation
               INLA,     # fit sin and cosin terms to data (note: must load after feasts)
               spdep,     # fit and assess models 
               tidymodels,    # for getting geocoordinates (lon/lat) based on place names
               readr,       # for interacting with copernicus sateliate CDS API
               utils,
               cvms,
               dplyr,
               klaR
               
)

#---- Load data files 


#data = st_read("outputs/catchments/catchments.shp")

malawi = st_read("inputs/catchments/catchments.shp")
malaria = read_csv("outputs/delimited/dhis_wide.csv")
d = read_csv("outputs/delimited/malariadata.csv")


#---- Reformat malria data, replace "-" with "_"

malaria <- malaria %>% 
  rename_all(~stringr::str_replace(.,"-","_"))
malaria <- malaria %>% 
  rename_all(~stringr::str_replace(.,"-","_"))

plot(map)

# Merge malaria mapping to the shapefile

map <- merge(map, malaria, by.x = "facility")

map@data[1:2, ]

# Mapping SIR
map_sf <- st_as_sf(map)
#map_sf = map_sf[,86:169]
map_sf2 = map_sf[, c(1,86:169)]
#map_sf = map_sf[,86:505]

#(map_sf, select = c(facility, date, E))


library(tidyr)

#map_sf <- gather(map_sf, year, SIR, paste0("SIR.", 84))

#map_sf <- gather(map_sf, date,SIR, 1:84)
map_sf2 <- gather(map_sf2, date,SIR, 2:85)

#-- replace SIR

#map_sf$date = gsub(pattern = "SIR.", "", map_sf$date)
map_sf2$date = gsub(pattern = "SIR.", "", map_sf2$date)

#map_sf$date <- as.Date(paste0(outcome$date, '_1'), '%b_%Y_%d')
#map_sf$date <-format(map_sf$date, "%h_%Y_%d")

#--- make SIR a decimal number
#map_sf$SIR = as.numeric(map_sf$SIR)
#map_sf = na.omit(map_sf)

map_sf2$SIR = as.numeric(map_sf2$SIR)
map_sf2 = na.omit(map_sf2)

map_sf2$date = gsub("_","-",map_sf2$date)

map_sf2$date = lubridate::ymd(map_sf2$date)



#Plot SIR

library(ggplot2)
ggplot(map_sf) + geom_sf(aes(fill = SIR)) +
  facet_wrap(~date, dir = "h", ncol = 7) +
  ggtitle("SIR") + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red"
  )

#---- Modelling starts here
library(INLA)
library(spdep)

#---- Neighborhood matrix to define spatial random effect

nb <- poly2nb(map)
head(nb)

nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")

#--- Matrix of facilities and months to be used for specifying teh random effects

d$idarea <- as.numeric(as.factor(d$facility))
d$idarea1 <- d$idarea
d$idate <- 1 + d$date - min(d$date)

#------- Bernadinelli model

formula <- cases ~ f(idarea, model = "bym", graph = g) +
  f(idarea1, idate, model = "iid") + idate

res <- inla(formula,
            family = "poisson", data = d, E = E,verbose = T,
            control.predictor = list(compute = TRUE)
)

#----- INLA Keep on crashing


#-------- Mapping relative risk

d$RR <- res$summary.fitted.values[, "mean"]
d$LL <- res$summary.fitted.values[, "0.025quant"]
d$UL <- res$summary.fitted.values[, "0.975quant"]

d2 =d
map_sf2 =map_sf

map_sf22 <- left_join(
  map_sf2, d,
  by = c("facility", "date")
)


ggplot(map_sf22) + geom_sf(aes(fill = RR)) +
  facet_wrap(~date, dir = "h", ncol = 7) +
  ggtitle("RR") + theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red"
  )


library(gganimate)
ggplot(map_sf) + geom_sf(aes(fill = RR)) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red"
  ) +
  transition_time(idate) +
  labs(title = "Year: {round(frame_time, 0)}")
