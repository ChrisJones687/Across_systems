### Data preparation for West Nile epidemic data at the county level in California ###
library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(rinat)
library(ebirdst)
library(auk)
library(sf)
wnv <- read.csv("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/West Nile Virus/west_nile_california.csv")
wnv[is.na(wnv)] <- 0

### California counties
## read in data
counties <- readOGR("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/West Nile Virus/us_lower_48_counties.shp")
CAcounties <- counties[counties$STATE_NAME == 'California',]
names(wnv)[2] <- "NAME"
## merge WNV data with county shapefiles
CAcounties18 <- merge(CAcounties, wnv[wnv$year==2018,], by = "NAME")
CAcounties17 <- merge(CAcounties, wnv[wnv$year==2017,], by = "NAME")
CAcounties16 <- merge(CAcounties, wnv[wnv$year==2016,], by = "NAME")
CAcounties15 <- merge(CAcounties, wnv[wnv$year==2015,], by = "NAME")
CAcounties14 <- merge(CAcounties, wnv[wnv$year==2014,], by = "NAME")
CAcounties13 <- merge(CAcounties, wnv[wnv$year==2013,], by = "NAME")
## clip NLCD data to California extent
nlcd2016 <- raster("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/NLCD_2016_Land_Cover_L48_20190424.img")
states <- readOGR("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/Foot and Mouth Disease/us_lower_48_states.shp")
california <- states[states$STATE_NAME == 'California',]
california <- spTransform(california, CRSobj = crs(nlcd2016))
california_nlcd <- crop(nlcd2016, california)
california_nlcd <- mask(california_nlcd, california)
## aggregate NLCD data to a lower resolution
california_nlcd_900m <- aggregate(california_nlcd, fact = 30, fun = 'sum')
california_nlcd_3000m <- aggregate(california_nlcd, fact = 100, fun = 'sum')
#CAcounties <- spTransform(CAcounties18, CRSobj = crs(california_nlcd_3000m))
## transform county data to NLCD data
CAcounties18 <- spTransform(CAcounties18, CRSobj = crs(california_nlcd_3000m))
CAcounties17 <- spTransform(CAcounties17, CRSobj = crs(california_nlcd_3000m))
CAcounties16 <- spTransform(CAcounties16, CRSobj = crs(california_nlcd_3000m))
CAcounties15 <- spTransform(CAcounties15, CRSobj = crs(california_nlcd_3000m))
CAcounties14 <- spTransform(CAcounties14, CRSobj = crs(california_nlcd_3000m))
CAcounties13 <- spTransform(CAcounties13, CRSobj = crs(california_nlcd_3000m))

###############
# HUMANS
###############

## HUMANS (infected)
## call CA county data for humans infected by year
CAcounties18_humans <- CAcounties18[,names(CAcounties18)[9]]
CAcounties17_humans <- CAcounties17[,names(CAcounties17)[9]]
CAcounties16_humans <- CAcounties16[,names(CAcounties16)[9]]
CAcounties15_humans <- CAcounties15[,names(CAcounties15)[9]]
CAcounties14_humans <- CAcounties14[,names(CAcounties14)[9]]
CAcounties13_humans <- CAcounties13[,names(CAcounties13)[9]]
## rasterize CA county data for humans infected by year
humans18 <- rasterize(CAcounties18_humans, california_nlcd_3000m, field = "human", fun = 'last')
humans17 <- rasterize(CAcounties17_humans, california_nlcd_3000m, field = "human", fun = 'last')
humans16 <- rasterize(CAcounties16_humans, california_nlcd_3000m, field = "human", fun = 'last')
humans15 <- rasterize(CAcounties15_humans, california_nlcd_3000m, field = "human", fun = 'last')
humans14 <- rasterize(CAcounties14_humans, california_nlcd_3000m, field = "human", fun = 'last')
humans13 <- rasterize(CAcounties13_humans, california_nlcd_3000m, field = "human", fun = 'last')
## randomly assign infected humans to a given number of cells in a county
for (i in 1:nrow(CAcounties)) {
  ex <- extract(humans18, CAcounties[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  humans18[ex] <- 0
  humans18[samples] <- ceiling(CAcounties18_humans$human[i]/3)
}
plot(humans18)
plot(CAcounties, add = TRUE)

## HUMANS (total population)
total_humans <- read.csv("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/West Nile Virus/humanpopulationcalifornia.csv")
total_humans <- total_humans[,1:2]
total_humans$NAME <- as.character(total_humans$NAME)
CAcounties$NAME <- as.character(CAcounties$NAME)
total_humans$Population <- as.numeric(total_humans$Population)
CAcounties <- CAcounties[order(CAcounties$NAME),]
total_humans <- total_humans[order(total_humans$NAME),]
#humans_CAcounties <- merge(CAcounties, total_humans$Population, by = "NAME")
CAcounties$Population <- total_humans$Population
## human total population raster
total_humans_raster <- rasterize(CAcounties, california_nlcd_3000m, field = "Population", fun = 'last')
## randomly assign total humans to a given number of cells in a county
for (i in 1:nrow(CAcounties)) {
  ex <- extract(total_humans_raster, CAcounties[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  total_humans_raster[ex] <- ceiling(CAcounties$Population[i]/length(ex))
}
total_humans_raster <- mask(total_humans_raster, california)
plot(total_humans_raster)

###############
# MOSQUITOES
###############

## MOSQUITOES (infected)
## call CA county data for mosquitoes infected by year
CAcounties18_mosquitoes <- CAcounties18[,names(CAcounties18)[12]]
CAcounties17_mosquitoes <- CAcounties17[,names(CAcounties17)[12]]
CAcounties16_mosquitoes <- CAcounties16[,names(CAcounties16)[12]]
CAcounties15_mosquitoes <- CAcounties15[,names(CAcounties15)[12]]
CAcounties14_mosquitoes <- CAcounties14[,names(CAcounties14)[12]]
CAcounties13_mosquitoes <- CAcounties13[,names(CAcounties13)[12]]
## rasterize CA county data for mosquitoes infected by year
mosquitoes18 <- rasterize(CAcounties18_mosquitoes, california_nlcd_3000m, field = "mosquitoes", fun = 'last')
mosquitoes17 <- rasterize(CAcounties17_mosquitoes, california_nlcd_3000m, field = "mosquitoes", fun = 'last')
mosquitoes16 <- rasterize(CAcounties16_mosquitoes, california_nlcd_3000m, field = "mosquitoes", fun = 'last')
mosquitoes15 <- rasterize(CAcounties15_mosquitoes, california_nlcd_3000m, field = "mosquitoes", fun = 'last')
mosquitoes14 <- rasterize(CAcounties14_mosquitoes, california_nlcd_3000m, field = "mosquitoes", fun = 'last')
mosquitoes13 <- rasterize(CAcounties13_mosquitoes, california_nlcd_3000m, field = "mosquitoes", fun = 'last')
## randomly assign infected mosquitoes to a given number of cells in a county
for (i in 1:nrow(CAcounties)) {
  ex <- extract(mosquitoes18, CAcounties[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  mosquitoes18[ex] <- 0
  mosquitoes18[samples] <- ceiling(CAcounties18_mosquitoes$mosquitoes[i]/3)
}
plot(mosquitoes18)
plot(CAcounties, add = TRUE)

## MOSQUITOES (total population)
species_of_interest <- "Mosquitoes"
extent <- c(32.534156, -124.409591, 42.009518, -114.131211)
inat_mosquitoes <- get_inat_obs(query = species_of_interest, maxresults = 30000, geo = TRUE, bounds = extent, quality = "research")
## extract points for mosquito sightings and plot
mosquitoes_crs <- '{"type": "name",
"properties": {
"name": "urn:ogc:def:crs:OGC:1.3:CRS84"
}}'
mosquitoes <- SpatialPointsDataFrame(inat_mosquitoes[,5:6],inat_mosquitoes)
mosquitoes <- crs(mosquitoes_crs)
plot(mosquitoes)

###############
# BIRDS
###############

## BIRDS (dead -> proxy for infected)
## call CA county data for birds infected by year
CAcounties18_birds <- CAcounties18[,names(CAcounties18)[11]]
CAcounties17_birds <- CAcounties17[,names(CAcounties17)[11]]
CAcounties16_birds <- CAcounties16[,names(CAcounties16)[11]]
CAcounties15_birds <- CAcounties15[,names(CAcounties15)[11]]
CAcounties14_birds <- CAcounties14[,names(CAcounties14)[11]]
CAcounties13_birds <- CAcounties13[,names(CAcounties13)[11]]
## rasterize CA county data for birds infected by year
birds18 <- rasterize(CAcounties18_birds, california_nlcd_3000m, field = "birds", fun = 'last')
birds17 <- rasterize(CAcounties17_birds, california_nlcd_3000m, field = "birds", fun = 'last')
birds16 <- rasterize(CAcounties16_birds, california_nlcd_3000m, field = "birds", fun = 'last')
birds15 <- rasterize(CAcounties15_birds, california_nlcd_3000m, field = "birds", fun = 'last')
birds14 <- rasterize(CAcounties14_birds, california_nlcd_3000m, field = "birds", fun = 'last')
birds13 <- rasterize(CAcounties13_birds, california_nlcd_3000m, field = "birds", fun = 'last')
## randomly assign infected birds to a given number of cells in a county
for (i in 1:nrow(CAcounties)) {
  ex <- extract(birds18, CAcounties[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  birds18[ex] <- 0
  birds18[samples] <- ceiling(CAcounties18_birds$birds[i]/3)
}
plot(birds18)
plot(CAcounties, add = TRUE)

## BIRDS (total population)
## based on iNaturalist
# species_of_interest <- "American Robin"
# extent <- c(32.534156, -124.409591, 42.009518, -114.131211)
# inat_birds <- get_inat_obs(query = species_of_interest, maxresults = 30000, geo = TRUE, bounds = extent, quality = "research")
# ## extract points for American robin sightings and plot
# birds_crs <- '{"type": "name",
# "properties": {
# "name": "urn:ogc:def:crs:OGC:1.3:CRS84"
# }}'
# birds <- SpatialPointsDataFrame(inat_birds[,5:6],inat_birds)
# birds <- crs(birds_crs)
# plot(birds)

## BIRDS (total population)
## based on eBird
# birds <- ebirdst_download(species = "amerob",  path = rappdirs::user_data_dir("ebirdst"),
#                  tifs_only = TRUE, force = TRUE)
# birds_abund <- load_raster("abundance_umean", path = birds)
# birds_abund <- crop(birds_abund, california_nlcd_3000m)
# birds_abund <- mask(birds_abund, california_nlcd_3000m)
# plot(birds_abund)
# birds_abund
# #project_extent(birds_abund, crs = california_nlcd_3000m)
# #birds_abund <- spTransform(birds_abund, CRSobj = crs(california_nlcd_3000m))
# plot(birds_abund)
# ebirdst_extent(CAcounties)

birds <- read.csv("/Users/rachellantz/Google Drive File Stream/My Drive/EEID/West Nile Virus/ebd_US-CA_amerob_201301_201812_relJul-2019.csv")
## extract eBird data by year
CA18_birds <- birds[birds$YEAR==2018,]
CA17_birds <- birds[birds$YEAR==2017,]
CA16_birds <- birds[birds$YEAR==2016,]
CA15_birds <- birds[birds$YEAR==2015,]
CA14_birds <- birds[birds$YEAR==2014,]
CA13_birds <- birds[birds$YEAR==2013,]

CA18_birds$LATITUDE <- as.numeric(CA18_birds$LATITUDE)
CA18_birds$LONGITUDE <- as.numeric(CA18_birds$LONGITUDE)
CA18_birds$OBSERVATION.COUNT <- as.numeric(CA18_birds$OBSERVATION.COUNT)

## create spatial dataframe for bird total population in CA
CA18_birds_lat_lon <- cbind(CA18_birds$LATITUDE, CA18_birds$LONGITUDE)
CA18_birds_coordinates <- CA18_birds_lat_lon
coordinates(CA18_birds_coordinates)=~long+lat
#proj4string(CA18_birds_coordinates) <- CRS("+proj=longlat +datum=WGS84")
#LLcoor<-spTransform(CA18_birds_coordinates,CRS("+proj=longlat"))
raster::shapefile(CA18_birds_coordinates, "CA18_total_birds.shp")
## rasterize the spatial dataframe
CA18_birds2 <- rasterize(CA18_birds_lat_lon, california_nlcd_3000m, field = CA18_birds$OBSERVATION.COUNT, fun = 'sum')
CA18_birds2

# CA18_birds_counts = data.frame(CA18_birds$OBSERVATION.COUNT)
# SpatialPointsDataFrame(CA18_birds_lat_lon, CA18_birds_counts)
# 
# ## then transform the spatial dataframe into a raster file
# california_nlcd_3000m <- aggregate(california_nlcd, fact = 100, fun = 'sum')
# CA18_birds <- st_as_sf(CA18_birds, coords = c("LATITUDE", "LONGITUDE"), crs = california_nlcd_3000m)
# CA18_birds2 <- rasterize(CA18_birds_lat_lon, california_nlcd_3000m, field = CA18_birds$OBSERVATION.COUNT, fun = 'sum')
# CA18_birds2
# plot(CA18_birds2)

# ## call CA county data for birds infected by year
# CA18_birds <- CA18_birds[,names(CA18_birds)[11]]
# CA17_birds <- CA17_birds[,names(CA17_birds)[11]]
# CA16_birds <- CA16_birds[,names(CA16_birds)[11]]
# CA15_birds <- CA15_birds[,names(CA15_birds)[11]]
# CA14_birds <- CA14_birds[,names(CA14_birds)[11]]
# CA13_birds <- CA13_birds[,names(CA13_birds)[11]]
# ## rasterize CA county data for birds infected by year
# birds18 <- rasterize(CAcounties18_birds, california_nlcd_3000m, field = "birds", fun = 'last')
# birds17 <- rasterize(CAcounties17_birds, california_nlcd_3000m, field = "birds", fun = 'last')
# birds16 <- rasterize(CAcounties16_birds, california_nlcd_3000m, field = "birds", fun = 'last')
# birds15 <- rasterize(CAcounties15_birds, california_nlcd_3000m, field = "birds", fun = 'last')
# birds14 <- rasterize(CAcounties14_birds, california_nlcd_3000m, field = "birds", fun = 'last')
# birds13 <- rasterize(CAcounties13_birds, california_nlcd_3000m, field = "birds", fun = 'last')

## Write out the following raster files: infected_humans, total_humans, infected_birds,
## total_birds, infected_mosquitoes, total_mosquitoes
writeRaster(infected_humans, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/West Nile Virus/infected_humans.tif")
writeRaster(total_humans, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/West Nile Virus/total_humans.tif")
writeRaster(infected_mosquitoes, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/West Nile Virus/infected_mosquitoes.tif")
writeRaster(total_mosquitoes, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/West Nile Virus/total_mosquitoes.tif")
writeRaster(infected_birds, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/West Nile Virus/infected_birds.tif")
writeRaster(total_birds, "/Users/rachellantz/Google Drive File Stream/My Drive/EEID/West Nile Virus/total_birds.tif")

