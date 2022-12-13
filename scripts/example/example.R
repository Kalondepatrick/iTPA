library(SpatialEpi)
data(scotland)


head(scotland$data)


map <- scotland$spatial.polygon
plot(map)


codes <- rgdal::make_EPSG()
codes[which(codes$code == "27700"), ]


proj4string(map) <- "+proj=tmerc +lat_0=49 +lon_0=-2
+k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36
+units=km +no_defs"


map <- spTransform(map,
                   CRS("+proj=longlat +datum=WGS84 +no_defs"))


d <- scotland$data[,c("county.names", "cases", "expected", "AFF")]
names(d) <- c("county", "Y", "E", "AFF")

d$SIR <- d$Y / d$E


sapply(slot(map, "polygons"), function(x){slot(x, "ID")})


library(sp)
rownames(d) <- d$county
map <- SpatialPolygonsDataFrame(map, d, match.ID = TRUE)



#Mapping

library(leaflet)
l <- leaflet(map) %>% addTiles()

pal <- colorNumeric(palette = "YlOrRd", domain = map$SIR)

l %>%
  addPolygons(
    color = "grey", weight = 1,
    fillColor = ~ pal(SIR), fillOpacity = 0.5
  ) %>%
  addLegend(
    pal = pal, values = ~SIR, opacity = 0.5,
    title = "SIR", position = "bottomright"
  )


labels <- sprintf("<strong> %s </strong> <br/>
  Observed: %s <br/> Expected: %s <br/>
  AFF: %s <br/> SIR: %s",
                  map$county, map$Y, round(map$E, 2),
                  map$AFF, round(map$SIR, 2)) %>%
  lapply(htmltools::HTML)

l %>%
  addPolygons(
    color = "grey", weight = 1,
    fillColor = ~ pal(SIR), fillOpacity = 0.5,
    highlightOptions = highlightOptions(weight = 4),
    label = labels,
    labelOptions = labelOptions(
      style = list(
        "font-weight" = "normal",
        padding = "3px 8px"
      ),
      textsize = "15px", direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal, values = ~SIR, opacity = 0.5,
    title = "SIR", position = "bottomright"
  )



#----- Modelling


library(spdep)
library(INLA)
nb <- poly2nb(map)
head(nb)



nb2INLA("map.adj", nb)
g <- inla.read.graph(filename = "map.adj")


map$idareau <- 1:nrow(map@data)
map$idareav <- 1:nrow(map@data)


formula <- Y ~ AFF +
  f(idareau, model = "besag", graph = g, scale.model = TRUE) +
  f(idareav, model = "iid")


res <- inla(formula,
            family = "poisson", data = map@data,
            E = E, control.predictor = list(compute = TRUE)
)

summary(res)


library(ggplot2)
marginal <- inla.smarginal(res$marginals.fixed$AFF)
marginal <- data.frame(marginal)
ggplot(marginal, aes(x = x, y = y)) + geom_line() +
  labs(x = expression(beta[1]), y = "Density") +
  geom_vline(xintercept = 0, col = "black") + theme_bw()



head(res$summary.fitted.values)



map$RR <- res$summary.fitted.values[, "mean"]
map$LL <- res$summary.fitted.values[, "0.025quant"]
map$UL <- res$summary.fitted.values[, "0.975quant"]


pal <- colorNumeric(palette = "YlOrRd", domain = map$RR)

labels <- sprintf("<strong> %s </strong> <br/>
  Observed: %s <br/> Expected: %s <br/>
  AFF: %s <br/> SIR: %s <br/> RR: %s (%s, %s)",
                  map$county, map$Y, round(map$E, 2),
                  map$AFF, round(map$SIR, 2), round(map$RR, 2),
                  round(map$LL, 2), round(map$UL, 2)) %>% lapply(htmltools::HTML)

lRR <- leaflet(map) %>%
  addTiles() %>%
  addPolygons(
    color = "grey", weight = 1, fillColor = ~ pal(RR),
    fillOpacity = 0.5,
    highlightOptions = highlightOptions(weight = 4),
    label = labels,
    labelOptions = labelOptions(
      style =
        list(
          "font-weight" = "normal",
          padding = "3px 8px"
        ),
      textsize = "15px", direction = "auto"
    )
  ) %>%
  addLegend(
    pal = pal, values = ~RR, opacity = 0.5, title = "RR",
    position = "bottomright"
  )
lRR



#Exceedance probabilities

1 - inla.pmarginal(q = c, marginal = marg)
