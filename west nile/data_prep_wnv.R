### Data preparation for West Nile epidemic data at the county level in California ###
library(raster)
library(sp)
library(rgdal)
library(dplyr)
library(rinat)
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
CAcounties <- spTransform(CAcounties18, CRSobj = crs(california_nlcd_3000m))
## transform county data to NLCD data
CAcounties18 <- spTransform(CAcounties18, CRSobj = crs(california_nlcd_3000m))
CAcounties17 <- spTransform(CAcounties17, CRSobj = crs(california_nlcd_3000m))
CAcounties16 <- spTransform(CAcounties16, CRSobj = crs(california_nlcd_3000m))
CAcounties15 <- spTransform(CAcounties15, CRSobj = crs(california_nlcd_3000m))
CAcounties14 <- spTransform(CAcounties14, CRSobj = crs(california_nlcd_3000m))
CAcounties13 <- spTransform(CAcounties13, CRSobj = crs(california_nlcd_3000m))

## HUMANS

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

## BIRDS (total population)
species_of_interest <- "American Robin"
extent <- c(32.534156, -124.409591, 42.009518, -114.131211)
inat_birds <- get_inat_obs(query = species_of_interest, maxresults = 30000, geo = TRUE, bounds = extent, quality = "research")
## extract points for American robin sightings and plot
birds_crs <- '{"type": "name",
"properties": {
"name": "urn:ogc:def:crs:OGC:1.3:CRS84"
}}'
birds <- SpatialPointsDataFrame(inat_birds[,5:6],inat_birds)
birds <- crs(birds_crs)
plot(birds)
