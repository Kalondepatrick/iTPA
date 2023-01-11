
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