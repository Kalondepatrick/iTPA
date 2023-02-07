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
               klaR,
               tidyr
               
)

#---- Load data files 


#data = st_read("outputs/catchments/catchments.shp")

map = readOGR("inputs/catchments/catchments.shp")
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

# Mapping cases

map_sf <- st_as_sf(map)
map_sf2 = map_sf[, c(1,170:253, 320)]

library(tidyr)

map_sf2 <- gather(map_sf2, date,cases, 2:85)

#---- edit date values

map_sf2$date = gsub(pattern = "cases.", "", map_sf2$date)

map_sf2$date = gsub("_","-",map_sf2$date)

map_sf2$date = lubridate::ymd(map_sf2$date)

# map_sf2 = na.omit(map_sf2)
 
 

#---- Modelling starts here

#--- Start with basic GLM

logit_base = glm(cases ~pop2020,
                 data =d, family = 'poisson')

# --- INLA MODEL

#---- Neighborhood matrix to define spatial random effect

nb <- poly2nb(map)
malawi_graph = nb2mat(nb, style = 'B', zero.policy = TRUE)

#--- Matrix of facilities and months to be used for specifying teh random effects

map_sf2$idarea <- as.numeric(as.factor(map_sf2$facility))
map_sf2$idate <- as.numeric(as.factor(map_sf2$date))
#map_sf2$idate <- 1 + map_sf2$date - min(map_sf2$date)

#--- model fitting
map_sf2 = na.omit(map_sf2)
map_sf2$pop.2020_07_01 = round(map_sf2$pop.2020_07_01)

res <- inla(cases ~ pop.2020_07_01 +
              f(idate, model = 'iid') +
              f(idarea, model ='besag', graph = malawi_graph),
            family = "poisson", data = map_sf2, verbose = TRUE)

#This one works

res <- inla(cases ~  
              f(idate, model = 'iid') +
              f(idarea, model ='besag', graph = malawi_graph, scale.model =  TRUE),
            E = pop.2020_07_01, family = "poisson", data = map_sf2, verbose = TRUE)

summary(res)


#----- INLA Keep on crashing





#Error in nb2listw(neighbours, glist = glist, style = style, zero.policy = zero.policy) : 
#Empty neighbour sets found
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


#res <- inla(cases ~ pop2020,
 #           f(id)
   #         family = "poisson", data = d, E = E,verbose = T,
  #          control.predictor = list(compute = TRUE)
#)

#----- INLA Keep on crashing


res <- inla(formula,
            family = "poisson", data = d, E = E,verbose = T,
            control.predictor = list(compute = TRUE)
)

#-------- Mapping relative risk

map_sf2$RR <- res$summary.fitted.values[, "mean"]
map_sf2$LL <- res$summary.fitted.values[, "0.025quant"]
map_sf2$UL <- res$summary.fitted.values[, "0.975quant"]

#d2 =d
#map_sf2 =map_sf

#map_sf22 <- left_join(
 # map_sf2, d,
  #by = c("facility", "date")
#)


ggplot(map_sf2) + geom_sf(aes(fill = RR)) +
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

#--- Visualizing changes in RR with time -----------#

library(gganimate)
b = ggplot(map_sf2) + geom_sf(aes(fill = RR)) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red"
  ) +
  transition_time(date) +
  labs(title = "Month: {round(frame_time, 0)}")
b


#--- Visualizing changes in cases with time -----------#

library(gganimate)
c = ggplot(map_sf2) + geom_sf(aes(fill = cases)) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red"
  ) +
  transition_time(date) +
  labs(title = "Month: {round(frame_time, 0)}")
c


#---- Visualizing tempral

g <- ggplot(map_sf2, aes(x = date, y = cases, 
                   group = facility, color = facility)) +
  geom_line() + geom_point(size = 2) + theme_bw()
g

g <- g + theme(legend.position = "none")
g

library(gghighlight)
g + gghighlight(facility == "Kasungu District Hospital")

#---- Areas of improvement
#Add a basemap
#Check modelling framework
#Add covariates