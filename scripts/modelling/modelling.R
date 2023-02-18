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
               tidyr,
               gghighlight,
               rio,            #  import/export
               naniar,         #  assess and visualize missingness
               mice           #missing data imputation
               
)

#---- Load data files 


#data = st_read("outputs/catchments/catchments.shp")

map = readOGR("inputs/catchments/catchments.shp")
malaria = read_csv("outputs/delimited/dhis_wide.csv")
d = read_csv("outputs/delimited/malariadata.csv")

#---- Data preparation


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

# --- Preparation of data for INLA

#---- Neighborhood matrix to define spatial random effect

nb <- poly2nb(map)
malawi_graph = nb2mat(nb, style = 'B', zero.policy = TRUE)

#--- Matrix of facilities and months to be used for specifying teh random effects

map_sf2$idarea <- as.numeric(as.factor(map_sf2$facility))
map_sf2$idate <- as.numeric(as.factor(map_sf2$date))
#map_sf2$idate <- 1 + map_sf2$date - min(map_sf2$date)

#---- Here is how the data looks

#--- Across space

map_space = map_sf2
map_space = na.omit(map_space)

map_space2 = map_space %>%
  group_by(facility) %>%
  summarize(
    total = sum(cases)
  )

plot(map_space2)

# create leaflet plot
library(tmap)
tmap_mode("view")
facetmaps <- tm_shape(map_space2) +
  tm_borders()+
  tm_fill(col ='total', palette = "OrRd")
facetmaps

#Find a way to visualize my tmaps interactively on github
tmap_save(facetmaps, "scripts/modelling/graphics/interactivemaps/my_map.html")

#Plot the cases since 2015 for all the facilities

#--- Across time

#---- Visualizing temporal

g <- ggplot(map_sf2, aes(x = date, y = cases, 
                         group = facility, color = facility)) +
  geom_line() + geom_point(size = 2) + theme_bw()
g

g <- g + theme(legend.position = "none")
g

library(gghighlight)
g + gghighlight(facility == "Kasungu District Hospital")

# animate the top 20 facilities and how the cases change with time

save = g + transition_reveal(date)
#anim_save("scripts/modelling/graphics/cases_time.gif", save)
#--- Top ten
top_10 = c("Salima District Hospital", "Nkhotakota District Hospital","	
Kasungu District Hospital","Nkhata Bay District Hospital", "Balaka District Hospital", "Mulanje District Hospital", "Mwanza District Hospital", "Machinga District Hospital", "Mponela Rural Hospital", "Chintheche Rural Hospital")

#--- Visualizing changes in cases with time -----------#

#top10_data = unique(map_sf2, by = top_10)
top10_data = map_sf2 %>% 
 subset(facility %in% top_10)

#--- Plotting

t <- ggplot(top10_data, aes(x = date, y = cases, 
                         group = facility, color = facility)) +
  geom_line() + geom_point(size = 2) + theme_bw()
t

#t <- t + theme(legend.position = "none")
t

library(gghighlight)
t + gghighlight(facility == "Kasungu District Hospital")

# animate the top 20 facilities and how the cases change with time

tm = t + transition_reveal(date)
tm

#Cumulative cases with time visualized as a a verical bar chart 


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

#anim_save("scripts/modelling/graphics/cases_space_time.gif", c)

#----  Adding covariates  ----#


#----- Adding covariates

library(sf)
library(tidyverse)

facilities.districts = read_sf("inputs/facilities/joined/fac_distict.shp")
# Rename columns
# Syntax rename with condition
colnames(facilities.districts)[colnames(facilities.districts) == "ADM2_EN"] ="Districts"
ITN = read_csv("inputs/Co-variates/ITN/ITN_2018.csv")

# replace values 
#ITN[ITN == "Mzimba S"] <- "Mzimba"
#ITN[ITN == "Mzimba N"] <- "Mzimba"
#ITN[ITN == "Nkhatabay"] <- "Nkhata Bay"
library(dplyr)

ITN_PJ2 = left_join(facilities.districts, ITN, by ="Districts")
colnames(ITN_PJ2)[colnames(ITN_PJ2) == "Total"] ="ITN"
colnames(ITN_PJ2)[colnames(ITN_PJ2) == "Name"] ="facility"
library(tidyverse)
ITN_data = ITN_PJ2 %>%
  dplyr::select('facility','ITN')

ITN_data = st_set_geometry(ITN_data, NULL)
#Remove rows with empty ITN 
ITN_data = ITN_data %>% drop_na(ITN) 

# Join ITN data to orignal dataset
map_sf_test = left_join(map_sf2, ITN_data, by ="facility")

#Check facilities where join was not sucessful
map_sf_test2 = anti_join(map_sf2, ITN_data, by ="facility")
#This will be fixed later | Find out the source of the problem

#---- Elevation
#Read elevation rasters
elevation =raster::raster("../../../Resources/Data/malawi/elevation/combined/elevation_mw.tif")

library(stars)
library(sf)

#Change format of catchment areas
#catch = as(map_sf, "SPatial")
elevation_mask = mask(elevation, mask = map_sf)

#crop is also the same
elevation_cropped = crop(elevation_mask, map_sf)

# Extracting values

vals = extract(elevation_cropped, map_sf,fun = mean)


#---- Modelling fitting ----#

#--- Start with basic GLM

logit_base = glm(cases ~pop2020,
                 data =d, family = 'poisson')

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




#This one works

res <- inla(cases ~  
              f(idate, model = 'iid') +
              f(idarea, model ='besag', graph = malawi_graph, scale.model =  TRUE),
            E = pop.2020_07_01, family = "poisson", data = map_sf2, verbose = TRUE)



#----- Checking results -----#
summary(res)
res$summary.fixed[, c("mean", "0.025quant", "0.975quant")] ## Fixed effects
res$summary.fitted.values[, "mean"] ## Fitted values
res$summary.hyperpar[, c("mean", "0.025quant", "0.975quant")] ##  Hyperparameters
res$summary.random$field ##  Random effects
res$marginals.fixed ## Posterior marginal distributions for predictors
res$marginals.hyperpar ## Posterior marginal distributions for hyperparameters
plot.mar.fixed(res) ## Custom function to plot marginal distributions of fixed effects
plot.mar.hyper(res) ## Custom function to plot marginal distributions of hyperparameters

## Build tables with all effects
tab1 <- res$summary.fixed[, c("mean", "0.025quant", "0.975quant")] %>%
  rename(mean_2018 = 'mean', Lci_2018 = '0.025quant', Uci_2018 = '0.975quant')



#Extension
##significacncia betas
round(res$summary.fixed,6)

##densidad betas

library(reshape2)
mf.B<-melt(res$marginals.fixed)
cf.B <- spread(mf.B,Var2,value)
names(cf.B)[2] <- "parameter"
library(ggplot2)
x11()
ggplot(cf.B,aes(x=x,y=y)) + geom_line()+facet_wrap(~ parameter,
                                                   scales="free") + geom_vline(xintercept=0) + ylab("density")


##interpretacion betas
prob.malaria <- inla.tmarginal(function(x) exp(x)/(1+exp(x)),
                               res$marginals.fixed[[1]])
inla.zmarginal(prob.malaria)

bin1<-inla.emarginal(exp, res$marginals.fixed$valor)
bin2<-inla.emarginal(exp, res$marginals.fixed$valor_1)
bin3<-inla.emarginal(exp, res$marginals.fixed$deng_total)
bin4<-inla.emarginal(exp, res$marginals.fixed$chic_total)
bin5<-inla.emarginal(exp, res$marginals.fixed$zika_total)
bin6<-inla.emarginal(exp, res$marginals.fixed$bosque_std)
bin7<-inla.emarginal(exp, res$marginals.fixed$preci_std)
bin8<-inla.emarginal(exp, res$marginals.fixed$alt_med_std)
round((c(bin1,bin2,bin3,bin4,bin5,bin6,bin7,bin8)-1)*100,6)
print(c(bin1,bin2,bin3,bin4,bin5,bin6,bin7,bin8))

Binomial.NI<-round(res$summary.fixed,6)$mean*100
###




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


#------- Visualizing missing values ------# 
# import the linelist
inde = map_sf2
drop = "pop.2020_07_01"
inde2 = inde[,!(names(inde)%in%drop)]
inde3 = spread(inde2, facility, cases)
gg_miss_fct(inde3, date)

gg_miss_span(inde3,
             var = date,
             span_every = 1500)

#---- Areas of improvement
#Add a basemap
#Check modelling framework
#Add covariates

#When I predict malaria cases using these variables, do I accurately predict the cases?