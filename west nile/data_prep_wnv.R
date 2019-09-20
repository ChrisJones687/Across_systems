### Data preparation for West Nile epidemic data at the county level in California ###
library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(rinat)
library(ebirdst)
library(auk)
library(sf)
library(naniar)
library(plotKML)
wnv <- read.csv("G:/My Drive/EEID/West Nile Virus/west_nile_california.csv")
wnv[is.na(wnv)] <- 0

### California counties
## read in data
counties <- readOGR("G:/My Drive/EEID/West Nile Virus/us_lower_48_counties.shp")
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
nlcd2016 <- raster("G:/My Drive/EEID/Foot and Mouth Disease/NLCD_2016_Land_Cover_L48_20190424.img")
states <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/us_lower_48_states.shp")
california <- states[states$STATE_NAME == 'California',]
california <- spTransform(california, CRSobj = crs(nlcd2016))
# california_nlcd <- crop(nlcd2016, california)
# california_nlcd <- mask(california_nlcd, california)

## load California NLCD raster files
## aggregate NLCD data to a lower resolution
# california_nlcd_900m <- aggregate(california_nlcd, fact = 30, fun = 'sum')
# california_nlcd_3000m <- aggregate(california_nlcd, fact = 100, fun = 'sum')
# writeRaster(california_nlcd_900m, "G:/My Drive/EEID/West Nile Virus/california_nlcd_900m.img")
# #writeRaster(california_nlcd_3000m, "G:/My Drive/EEID/West Nile Virus/california_nlcd_3000m.img")
california_nlcd_900m <- raster("G:/My Drive/EEID/West Nile Virus/california_nlcd_900m.img")
california_nlcd_3000m <- raster("G:/My Drive/EEID/West Nile Virus/california_nlcd_3000m.img")

#CAcounties <- spTransform(CAcounties18, CRSobj = crs(california_nlcd_3000m))
## transform county data to NLCD data
CAcounties18 <- spTransform(CAcounties18, CRSobj = crs(california_nlcd_3000m))

###############
# HUMANS
###############

## HUMANS (infected)
## call CA county data for humans infected by year
CAcounties18_humans <- CAcounties18[,names(CAcounties18)[9]]

## rasterize CA county data for humans infected by year
humans18 <- rasterize(CAcounties18_humans, california_nlcd_3000m, field = "human", fun = 'last')

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
total_humans <- read.csv("G:/My Drive/EEID/West Nile Virus/humanpopulationcalifornia.csv")
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
## rasterize CA county data for mosquitoes infected by year
mosquitoes18 <- rasterize(CAcounties18_mosquitoes, california_nlcd_3000m, field = "mosquitoes", fun = 'last')
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
CA18_mosquitoes <- inat_mosquitoes[inat_mosquitoes$YEAR==2018,]
lon <- CA18_mosquitoes$LONGITUDE
lat <- CA18_mosquitoes$LATITUDE
count <- CA18_mosquitoes$count
CA18_mosquitoes_data <- cbind.data.frame(count)
CA18_mosquitoes_lon_lat <- cbind.data.frame(lon, lat)


## extract points for mosquito sightings and plot
mosquitoes_crs <- '{"type": "name",
"properties": {
"name": "urn:ogc:def:crs:OGC:1.3:CRS84"
}}'
CA18_total_mosquitoes <- SpatialPointsDataFrame(CA18_mosquitoes_lon_lat, CA18_mosquitoes_data)

mosquitoes <- SpatialPointsDataFrame(inat_mosquitoes[,5:6],inat_mosquitoes)
mosquitoes <- crs(mosquitoes_crs)
total_mosquitoes <- rasterize() 
plot(mosquitoes)



CA18_birds <- birds[birds$YEAR==2018,]
lon <- CA18_birds$LONGITUDE
lat <- CA18_birds$LATITUDE
count <- CA18_birds$count
CA18_data_frame <- cbind.data.frame(count)
CA18_birds_lon_lat <- cbind.data.frame(lon, lat)
CA18_total_birds <- SpatialPointsDataFrame(CA18_birds_lon_lat, CA18_birds_data, proj4string = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
total_birds18 <- rasterize(CA18_total_birds, california_nlcd_3000m, field = "count", fun = 'sum', mask = TRUE, background = 0)

###############
# BIRDS
###############

## BIRDS (dead -> proxy for infected)
## call CA county data for birds infected by year
CAcounties18_birds <- CAcounties18[,names(CAcounties18)[11]]
## rasterize CA county data for birds infected by year
infected_birds18 <- rasterize(CAcounties18_birds, california_nlcd_3000m, field = "birds", fun = 'last')
## randomly assign infected birds to a given number of cells in a county 2018
for (i in 1:nrow(CAcounties18)) {
  ex <- extract(infected_birds18, CAcounties18[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  infected_birds18[ex] <- 0
  infected_birds18[samples] <- ceiling(CAcounties18_birds$birds[i]/3)
}

## BIRDS (total population)
## set up bird data
birds <- read.csv("G:/My Drive/EEID/West Nile Virus/american_robin_CA_2013_2018.csv")
birds$count <- as.character(birds$count)
birds$count[is.na(birds$count)] <- "0"
birds$count <- as.numeric(birds$count)
## create rasters for total birds 2013-2018
## 2018
CA18_birds <- birds[birds$YEAR==2018,]
lon <- CA18_birds$LONGITUDE
lat <- CA18_birds$LATITUDE
count <- CA18_birds$count
CA18_birds_data <- cbind.data.frame(count)
CA18_birds_lon_lat <- cbind.data.frame(lon, lat)
CA18_total_birds <- SpatialPointsDataFrame(CA18_birds_lon_lat, CA18_birds_data, proj4string = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
total_birds18 <- rasterize(CA18_total_birds, california_nlcd_3000m, field = "count", fun = 'sum', mask = TRUE, background = 0)

## Write out the following raster files: infected_humans, total_humans, infected_birds,
## total_birds, infected_mosquitoes, total_mosquitoes
writeRaster(infected_humans, "G:/My Drive/EEID/West Nile Virus/infected_humans.tif")
writeRaster(total_humans, "G:/My Drive/EEID/West Nile Virus/total_humans.tif")
writeRaster(infected_mosquitoes, "G:/My Drive/EEID/West Nile Virus/infected_mosquitoes.tif")
writeRaster(total_mosquitoes, "G:/My Drive/EEID/West Nile Virus/total_mosquitoes.tif")
writeRaster(infected_birds, "G:/My Drive/EEID/West Nile Virus/infected_birds.tif")
## BIRDS (total population) 2013-2018
writeRaster(total_birds18, "G:/My Drive/EEID/West Nile Virus/total_birds18.tif", overwrite = TRUE)


