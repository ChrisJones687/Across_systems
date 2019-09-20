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
library(lubridate)
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
# ## aggregate NLCD data to a lower resolution
# california_nlcd_900m <- aggregate(california_nlcd, fact = 30, fun = 'sum')
# #california_nlcd_3000m <- aggregate(california_nlcd, fact = 100, fun = 'sum')
# writeRaster(california_nlcd_900m, "G:/My Drive/EEID/West Nile Virus/california_nlcd_900m.img")
# #writeRaster(california_nlcd_3000m, "G:/My Drive/EEID/West Nile Virus/california_nlcd_3000m.img")
california_nlcd_900m <- raster("G:/My Drive/EEID/West Nile Virus/california_nlcd_900m.img")
california_nlcd_3000m <- raster("G:/My Drive/EEID/West Nile Virus/california_nlcd_3000m.img")

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
## randomly assign infected humans to a given number of cells in a county in 2018
for (i in 1:nrow(CAcounties18)) {
  ex <- extract(humans18, CAcounties18[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  humans18[ex] <- 0
  humans18[samples] <- ceiling(CAcounties18_humans$human[i]/3)
}
infected_humans18 <- humans18
## randomly assign infected humans to a given number of cells in a county in 2017
for (i in 1:nrow(CAcounties17)) {
  ex <- extract(humans17, CAcounties17[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  humans17[ex] <- 0
  humans17[samples] <- ceiling(CAcounties17_humans$human[i]/3)
}
infected_humans17 <- humans17
## randomly assign infected humans to a given number of cells in a county in 2016
for (i in 1:nrow(CAcounties16)) {
  ex <- extract(humans16, CAcounties16[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  humans16[ex] <- 0
  humans16[samples] <- ceiling(CAcounties16_humans$human[i]/3)
}
infected_humans16 <- humans16
## randomly assign infected humans to a given number of cells in a county in 2015
for (i in 1:nrow(CAcounties15)) {
  ex <- extract(humans15, CAcounties15[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  humans15[ex] <- 0
  humans15[samples] <- ceiling(CAcounties15_humans$human[i]/3)
}
infected_humans15 <- humans15
## randomly assign infected humans to a given number of cells in a county in 2014
for (i in 1:nrow(CAcounties14)) {
  ex <- extract(humans14, CAcounties14[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  humans14[ex] <- 0
  humans14[samples] <- ceiling(CAcounties14_humans$human[i]/3)
}
infected_humans14 <- humans14
## randomly assign infected humans to a given number of cells in a county in 2013
for (i in 1:nrow(CAcounties13)) {
  ex <- extract(humans13, CAcounties13[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  humans13[ex] <- 0
  humans13[samples] <- ceiling(CAcounties13_humans$human[i]/3)
}
infected_humans13 <- humans13

## HUMANS (total population)
total_humans <- read.csv("G:/My Drive/EEID/West Nile Virus/humanpopulationcalifornia.csv")
#total_humans <- total_humans[,1:2]
total_humans$NAME <- as.character(total_humans$NAME)
CAcounties18$NAME <- as.character(CAcounties18$NAME)
total_humans$Population <- as.numeric(total_humans$Population)
CAcounties18 <- CAcounties18[order(CAcounties18$NAME),]
total_humans <- total_humans[order(total_humans$NAME),]
#humans_CAcounties <- merge(CAcounties, total_humans$Population, by = "NAME")
CAcounties18$Population <- total_humans$Population
## human total population raster
total_humans18 <- rasterize(CAcounties18, california_nlcd_3000m, field = "2018", fun = 'sum')
## randomly assign total humans to a given number of cells in a county
for (i in 1:nrow(CAcounties18)) {
  ex <- extract(total_humans, CAcounties18[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  total_humans[ex] <- ceiling(CAcounties18$Population[i]/length(ex))
}
total_humans <- mask(total_humans, california)

total_humans_CA18 <- total_humans$2018


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
infected_mosquitoes18 <- rasterize(CAcounties18_mosquitoes, california_nlcd_3000m, field = "mosquitoes", fun = 'last')
infected_mosquitoes17 <- rasterize(CAcounties17_mosquitoes, california_nlcd_3000m, field = "mosquitoes", fun = 'last')
infected_mosquitoes16 <- rasterize(CAcounties16_mosquitoes, california_nlcd_3000m, field = "mosquitoes", fun = 'last')
infected_mosquitoes15 <- rasterize(CAcounties15_mosquitoes, california_nlcd_3000m, field = "mosquitoes", fun = 'last')
infected_mosquitoes14 <- rasterize(CAcounties14_mosquitoes, california_nlcd_3000m, field = "mosquitoes", fun = 'last')
infected_mosquitoes13 <- rasterize(CAcounties13_mosquitoes, california_nlcd_3000m, field = "mosquitoes", fun = 'last')
## randomly assign infected mosquitoes to a given number of cells in a county in 2018
for (i in 1:nrow(CAcounties18)) {
  ex <- extract(infected_mosquitoes18, CAcounties18[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  infected_mosquitoes18[ex] <- 0
  infected_mosquitoes18[samples] <- ceiling(CAcounties18_mosquitoes$mosquitoes[i]/3)
}
infected_mosquitoes18 <- mask(infected_mosquitoes18, california)
## randomly assign infected mosquitoes to a given number of cells in a county in 2017
for (i in 1:nrow(CAcounties17)) {
  ex <- extract(infected_mosquitoes17, CAcounties17[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  infected_mosquitoes17[ex] <- 0
  infected_mosquitoes17[samples] <- ceiling(CAcounties17_mosquitoes$mosquitoes[i]/3)
}
infected_mosquitoes17 <- mask(infected_mosquitoes17, california)
## randomly assign infected mosquitoes to a given number of cells in a county in 2016
for (i in 1:nrow(CAcounties16)) {
  ex <- extract(infected_mosquitoes16, CAcounties16[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  infected_mosquitoes16[ex] <- 0
  infected_mosquitoes16[samples] <- ceiling(CAcounties16_mosquitoes$mosquitoes[i]/3)
}
infected_mosquitoes16 <- mask(infected_mosquitoes16, california)
## randomly assign infected mosquitoes to a given number of cells in a county in 2015
for (i in 1:nrow(CAcounties15)) {
  ex <- extract(infected_mosquitoes15, CAcounties15[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  infected_mosquitoes15[ex] <- 0
  infected_mosquitoes15[samples] <- ceiling(CAcounties15_mosquitoes$mosquitoes[i]/3)
}
infected_mosquitoes15 <- mask(infected_mosquitoes15, california)
## randomly assign infected mosquitoes to a given number of cells in a county in 2014
for (i in 1:nrow(CAcounties14)) {
  ex <- extract(infected_mosquitoes14, CAcounties14[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  infected_mosquitoes14[ex] <- 0
  infected_mosquitoes14[samples] <- ceiling(CAcounties14_mosquitoes$mosquitoes[i]/3)
}
infected_mosquitoes14 <- mask(infected_mosquitoes14, california)
## randomly assign infected mosquitoes to a given number of cells in a county in 2013
for (i in 1:nrow(CAcounties13)) {
  ex <- extract(infected_mosquitoes13, CAcounties13[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  infected_mosquitoes13[ex] <- 0
  infected_mosquitoes13[samples] <- ceiling(CAcounties13_mosquitoes$mosquitoes[i]/3)
}
infected_mosquitoes13 <- mask(infected_mosquitoes13, california)

## MOSQUITOES (total population)
species_of_interest <- "Mosquitoes"
extent <- c(32.534156, -124.409591, 42.009518, -114.131211)
inat_mosquitoes <- get_inat_obs(query = species_of_interest, maxresults = 30000, geo = TRUE, bounds = extent, quality = "research")
inat_mosquitoes$YEAR <- year(inat_mosquitoes$datetime)
inat_mosquitoes$MONTH <- month(inat_mosquitoes$datetime)
## 2018 by month
## january 2018
total_mosquitoes_CA18 <- inat_mosquitoes[inat_mosquitoes$YEAR==2018,]
total_mosquitoes_CA18_1 <- total_mosquitoes_CA18[total_mosquitoes_CA18$MONTH==1,]
count <- total_mosquitoes_CA18_1$num_identification_agreements
lon <- total_mosquitoes_CA18_1$longitude
lat <- total_mosquitoes_CA18_1$latitude
CA18_1_mosquitoes_lon_lat <- cbind.data.frame(lon, lat)
CA18_1_mosquitoes_data <- SpatialPointsDataFrame(CA18_1_mosquitoes_lon_lat, total_mosquitoes_CA18_1)
CA18_1_total_mosquitoes <- rasterize(CA18_1_mosquitoes_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_1_total_mosquitoes <- mask(CA18_1_total_mosquitoes, california)
## february 2018
total_mosquitoes_CA18 <- inat_mosquitoes[inat_mosquitoes$YEAR==2018,]
total_mosquitoes_CA18_2 <- total_mosquitoes_CA18[total_mosquitoes_CA18$MONTH==2,]
count <- total_mosquitoes_CA18_2$num_identification_agreements
lon <- total_mosquitoes_CA18_2$longitude
lat <- total_mosquitoes_CA18_2$latitude
CA18_2_mosquitoes_lon_lat <- cbind.data.frame(lon, lat)
CA18_2_mosquitoes_data <- SpatialPointsDataFrame(CA18_2_mosquitoes_lon_lat, total_mosquitoes_CA18_2)
CA18_2_total_mosquitoes <- rasterize(CA18_2_mosquitoes_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_2_total_mosquitoes <- mask(CA18_2_total_mosquitoes, california)
## march 2018
total_mosquitoes_CA18 <- inat_mosquitoes[inat_mosquitoes$YEAR==2018,]
total_mosquitoes_CA18_3 <- total_mosquitoes_CA18[total_mosquitoes_CA18$MONTH==3,]
count <- total_mosquitoes_CA18_3$num_identification_agreements
lon <- total_mosquitoes_CA18_3$longitude
lat <- total_mosquitoes_CA18_3$latitude
CA18_3_mosquitoes_lon_lat <- cbind.data.frame(lon, lat)
CA18_3_mosquitoes_data <- SpatialPointsDataFrame(CA18_3_mosquitoes_lon_lat, total_mosquitoes_CA18_3)
CA18_3_total_mosquitoes <- rasterize(CA18_3_mosquitoes_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_3_total_mosquitoes <- mask(CA18_3_total_mosquitoes, california)
## april 2018
total_mosquitoes_CA18 <- inat_mosquitoes[inat_mosquitoes$YEAR==2018,]
total_mosquitoes_CA18_4 <- total_mosquitoes_CA18[total_mosquitoes_CA18$MONTH==4,]
count <- total_mosquitoes_CA18_4$num_identification_agreements
lon <- total_mosquitoes_CA18_4$longitude
lat <- total_mosquitoes_CA18_4$latitude
CA18_4_mosquitoes_lon_lat <- cbind.data.frame(lon, lat)
CA18_4_mosquitoes_data <- SpatialPointsDataFrame(CA18_4_mosquitoes_lon_lat, total_mosquitoes_CA18_4)
CA18_4_total_mosquitoes <- rasterize(CA18_4_mosquitoes_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_4_total_mosquitoes <- mask(CA18_4_total_mosquitoes, california)
## may 2018
total_mosquitoes_CA18 <- inat_mosquitoes[inat_mosquitoes$YEAR==2018,]
total_mosquitoes_CA18_5 <- total_mosquitoes_CA18[total_mosquitoes_CA18$MONTH==5,]
count <- total_mosquitoes_CA18_5$num_identification_agreements
lon <- total_mosquitoes_CA18_5$longitude
lat <- total_mosquitoes_CA18_5$latitude
CA18_5_mosquitoes_lon_lat <- cbind.data.frame(lon, lat)
CA18_5_mosquitoes_data <- SpatialPointsDataFrame(CA18_5_mosquitoes_lon_lat, total_mosquitoes_CA18_5)
CA18_5_total_mosquitoes <- rasterize(CA18_5_mosquitoes_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_5_total_mosquitoes <- mask(CA18_5_total_mosquitoes, california)
## june 2018
total_mosquitoes_CA18 <- inat_mosquitoes[inat_mosquitoes$YEAR==2018,]
total_mosquitoes_CA18_6 <- total_mosquitoes_CA18[total_mosquitoes_CA18$MONTH==6,]
count <- total_mosquitoes_CA18_6$num_identification_agreements
lon <- total_mosquitoes_CA18_6$longitude
lat <- total_mosquitoes_CA18_6$latitude
CA18_6_mosquitoes_lon_lat <- cbind.data.frame(lon, lat)
CA18_6_mosquitoes_data <- SpatialPointsDataFrame(CA18_6_mosquitoes_lon_lat, total_mosquitoes_CA18_6)
CA18_6_total_mosquitoes <- rasterize(CA18_6_mosquitoes_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_6_total_mosquitoes <- mask(CA18_6_total_mosquitoes, california)
## july 2018
total_mosquitoes_CA18 <- inat_mosquitoes[inat_mosquitoes$YEAR==2018,]
total_mosquitoes_CA18_7 <- total_mosquitoes_CA18[total_mosquitoes_CA18$MONTH==7,]
count <- total_mosquitoes_CA18_7$num_identification_agreements
lon <- total_mosquitoes_CA18_7$longitude
lat <- total_mosquitoes_CA18_7$latitude
CA18_7_mosquitoes_lon_lat <- cbind.data.frame(lon, lat)
CA18_7_mosquitoes_data <- SpatialPointsDataFrame(CA18_7_mosquitoes_lon_lat, total_mosquitoes_CA18_7)
CA18_7_total_mosquitoes <- rasterize(CA18_7_mosquitoes_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_7_total_mosquitoes <- mask(CA18_7_total_mosquitoes, california)
## august 2018
total_mosquitoes_CA18 <- inat_mosquitoes[inat_mosquitoes$YEAR==2018,]
total_mosquitoes_CA18_8 <- total_mosquitoes_CA18[total_mosquitoes_CA18$MONTH==8,]
count <- total_mosquitoes_CA18_8$num_identification_agreements
lon <- total_mosquitoes_CA18_8$longitude
lat <- total_mosquitoes_CA18_8$latitude
CA18_8_mosquitoes_lon_lat <- cbind.data.frame(lon, lat)
CA18_8_mosquitoes_data <- SpatialPointsDataFrame(CA18_8_mosquitoes_lon_lat, total_mosquitoes_CA18_8)
CA18_8_total_mosquitoes <- rasterize(CA18_8_mosquitoes_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_8_total_mosquitoes <- mask(CA18_8_total_mosquitoes, california)
## september 2018
total_mosquitoes_CA18 <- inat_mosquitoes[inat_mosquitoes$YEAR==2018,]
total_mosquitoes_CA18_9 <- total_mosquitoes_CA18[total_mosquitoes_CA18$MONTH==9,]
count <- total_mosquitoes_CA18_9$num_identification_agreements
lon <- total_mosquitoes_CA18_9$longitude
lat <- total_mosquitoes_CA18_9$latitude
CA18_9_mosquitoes_lon_lat <- cbind.data.frame(lon, lat)
CA18_9_mosquitoes_data <- SpatialPointsDataFrame(CA18_9_mosquitoes_lon_lat, total_mosquitoes_CA18_9)
CA18_9_total_mosquitoes <- rasterize(CA18_9_mosquitoes_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_9_total_mosquitoes <- mask(CA18_9_total_mosquitoes, california)
## october 2018
total_mosquitoes_CA18 <- inat_mosquitoes[inat_mosquitoes$YEAR==2018,]
total_mosquitoes_CA18_10 <- total_mosquitoes_CA18[total_mosquitoes_CA18$MONTH==10,]
count <- total_mosquitoes_CA18_10$num_identification_agreements
lon <- total_mosquitoes_CA18_10$longitude
lat <- total_mosquitoes_CA18_10$latitude
CA18_10_mosquitoes_lon_lat <- cbind.data.frame(lon, lat)
CA18_10_mosquitoes_data <- SpatialPointsDataFrame(CA18_10_mosquitoes_lon_lat, total_mosquitoes_CA18_10)
CA18_10_total_mosquitoes <- rasterize(CA18_10_mosquitoes_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_10_total_mosquitoes <- mask(CA18_10_total_mosquitoes, california)
## november 2018
total_mosquitoes_CA18 <- inat_mosquitoes[inat_mosquitoes$YEAR==2018,]
total_mosquitoes_CA18_11 <- total_mosquitoes_CA18[total_mosquitoes_CA18$MONTH==11,]
count <- total_mosquitoes_CA18_11$num_identification_agreements
lon <- total_mosquitoes_CA18_11$longitude
lat <- total_mosquitoes_CA18_11$latitude
CA18_11_mosquitoes_lon_lat <- cbind.data.frame(lon, lat)
CA18_11_mosquitoes_data <- SpatialPointsDataFrame(CA18_11_mosquitoes_lon_lat, total_mosquitoes_CA18_11)
CA18_11_total_mosquitoes <- rasterize(CA18_11_mosquitoes_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_11_total_mosquitoes <- mask(CA18_11_total_mosquitoes, california)
## december 2018
total_mosquitoes_CA18 <- inat_mosquitoes[inat_mosquitoes$YEAR==2018,]
total_mosquitoes_CA18_12 <- total_mosquitoes_CA18[total_mosquitoes_CA18$MONTH==12,]
count <- total_mosquitoes_CA18_12$num_identification_agreements
lon <- total_mosquitoes_CA18_12$longitude
lat <- total_mosquitoes_CA18_12$latitude
CA18_12_mosquitoes_lon_lat <- cbind.data.frame(lon, lat)
CA18_12_mosquitoes_data <- SpatialPointsDataFrame(CA18_12_mosquitoes_lon_lat, total_mosquitoes_CA18_12)
CA18_12_total_mosquitoes <- rasterize(CA18_12_mosquitoes_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_12_total_mosquitoes <- mask(CA18_12_total_mosquitoes, california)

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
infected_birds18 <- rasterize(CAcounties18_birds, california_nlcd_3000m, field = "birds", fun = 'last')
infected_birds17 <- rasterize(CAcounties17_birds, california_nlcd_3000m, field = "birds", fun = 'last')
infected_birds16 <- rasterize(CAcounties16_birds, california_nlcd_3000m, field = "birds", fun = 'last')
infected_birds15 <- rasterize(CAcounties15_birds, california_nlcd_3000m, field = "birds", fun = 'last')
infected_birds14 <- rasterize(CAcounties14_birds, california_nlcd_3000m, field = "birds", fun = 'last')
infected_birds13 <- rasterize(CAcounties13_birds, california_nlcd_3000m, field = "birds", fun = 'last')
## randomly assign infected birds to a given number of cells in a county 2018
for (i in 1:nrow(CAcounties18)) {
  ex <- extract(infected_birds18, CAcounties18[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  infected_birds18[ex] <- 0
  infected_birds18[samples] <- ceiling(CAcounties18_birds$birds[i]/3)
}
infected_birds18 <- mask(infected_birds18, california)
## randomly assign infected birds to a given number of cells in a county 2017
for (i in 1:nrow(CAcounties17)) {
  ex <- extract(infected_birds17, CAcounties17[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  infected_birds17[ex] <- 0
  infected_birds17[samples] <- ceiling(CAcounties17_birds$birds[i]/3)
}
infected_birds17 <- mask(infected_birds17, california)
## randomly assign infected birds to a given number of cells in a county 2016
for (i in 1:nrow(CAcounties16)) {
  ex <- extract(infected_birds16, CAcounties16[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  infected_birds16[ex] <- 0
  infected_birds16[samples] <- ceiling(CAcounties16_birds$birds[i]/3)
}
infected_birds16 <- mask(infected_birds16, california)
## randomly assign infected birds to a given number of cells in a county 2015
for (i in 1:nrow(CAcounties15)) {
  ex <- extract(infected_birds15, CAcounties15[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  infected_birds15[ex] <- 0
  infected_birds15[samples] <- ceiling(CAcounties15_birds$birds[i]/3)
}
infected_birds15 <- mask(infected_birds15, california)
## randomly assign infected birds to a given number of cells in a county 2014
for (i in 1:nrow(CAcounties14)) {
  ex <- extract(infected_birds14, CAcounties14[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  infected_birds14[ex] <- 0
  infected_birds14[samples] <- ceiling(CAcounties14_birds$birds[i]/3)
}
infected_birds14 <- mask(infected_birds14, california)
## randomly assign infected birds to a given number of cells in a county 2013
for (i in 1:nrow(CAcounties13)) {
  ex <- extract(infected_birds13, CAcounties13[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  infected_birds13[ex] <- 0
  infected_birds13[samples] <- ceiling(CAcounties13_birds$birds[i]/3)
}
infected_birds13 <- mask(infected_birds13, california)


## BIRDS (total population)
birds <- read.csv("G:/My Drive/EEID/West Nile Virus/american_robin_CA_2013_2018.csv")
birds$count <- as.character(birds$count)
birds$count[is.na(birds$count)] <- "0"
birds$count <- as.numeric(birds$count)
## 2018 by month
## january 2018
total_birds_CA18 <- birds[birds$YEAR==2018,]
total_birds_CA18_1 <- total_birds_CA18[total_birds_CA18$MONTH==1,]
count <- total_birds_CA18_1$count
lon <- total_birds_CA18_1$LONGITUDE
lat <- total_birds_CA18_1$LATITUDE
california_crs <- st_crs(california_nlcd_3000m)
CA18_1_birds_lon_lat <- cbind.data.frame(lon, lat)
CA18_1_birds_data <- cbind.data.frame(count)
CA18_1_birds_data <- SpatialPointsDataFrame(CA18_1_birds_lon_lat, CA18_1_birds_data, proj4string = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
CA18_1_total_birds <- rasterize(CA18_1_birds_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_1_total_birds <- mask(CA18_1_total_birds, california)
## february 2018
total_birds_CA18 <- birds[birds$YEAR==2018,]
total_birds_CA18_2 <- total_birds_CA18[total_birds_CA18$MONTH==2,]
count <- total_birds_CA18_2$count
lon <- total_birds_CA18_2$LONGITUDE
lat <- total_birds_CA18_2$LATITUDE
CA18_2_birds_lon_lat <- cbind.data.frame(lon, lat)
CA18_2_birds_data <- SpatialPointsDataFrame(CA18_2_birds_lon_lat, total_birds_CA18_2)
CA18_2_total_birds <- rasterize(CA18_2_birds_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= FALSE, background = 0) 
CA18_2_total_birds <- mask(CA18_2_total_birds, california)
## march 2018
total_birds_CA18 <- birds[birds$YEAR==2018,]
total_birds_CA18_3 <- total_birds_CA18[total_birds_CA18$MONTH==3,]
count <- total_birds_CA18_3$count
lon <- total_birds_CA18_3$LONGITUDE
lat <- total_birds_CA18_3$LATITUDE
CA18_3_birds_lon_lat <- cbind.data.frame(lon, lat)
CA18_3_birds_data <- SpatialPointsDataFrame(CA18_3_birds_lon_lat, total_birds_CA18_3)
CA18_3_total_birds <- rasterize(CA18_3_birds_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_3_total_birds <- mask(CA18_3_total_birds, california)
## february 2018
total_birds_CA18 <- birds[birds$YEAR==2018,]
total_birds_CA18_2 <- total_birds_CA18[total_birds_CA18$MONTH==2,]
count <- total_birds_CA18_2$count
lon <- total_birds_CA18_2$LONGITUDE
lat <- total_birds_CA18_2$LATITUDE
CA18_2_birds_lon_lat <- cbind.data.frame(lon, lat)
CA18_2_birds_data <- SpatialPointsDataFrame(CA18_2_birds_lon_lat, total_birds_CA18_2)
CA18_2_total_birds <- rasterize(CA18_2_birds_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_2_total_birds <- mask(CA18_2_total_birds, california)
## february 2018
total_birds_CA18 <- birds[birds$YEAR==2018,]
total_birds_CA18_2 <- total_birds_CA18[total_birds_CA18$MONTH==2,]
count <- total_birds_CA18_2$count
lon <- total_birds_CA18_2$LONGITUDE
lat <- total_birds_CA18_2$LATITUDE
CA18_2_birds_lon_lat <- cbind.data.frame(lon, lat)
CA18_2_birds_data <- SpatialPointsDataFrame(CA18_2_birds_lon_lat, total_birds_CA18_2)
CA18_2_total_birds <- rasterize(CA18_2_birds_data, california_nlcd_3000m, field = "count", fun = 'sum', mask= TRUE, background = 0) 
CA18_2_total_birds <- mask(CA18_2_total_birds, california)

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
CA18_data_frame <- cbind.data.frame(count)
CA18_birds_lon_lat <- cbind.data.frame(lon, lat)
california_crs <- st_crs(california_nlcd_3000m)
CA18_total_birds <- SpatialPointsDataFrame(CA18_birds_lon_lat, CA18_data_frame, proj4string = CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
total_birds18 <- rasterize(CA18_total_birds, california_nlcd_3000m, field = "count", fun = 'sum', mask = TRUE, background = 0)
total_birds18 <- mask(total_birds18, california)

## Write out the following raster files: infected_humans, total_humans, infected_birds,
## total_birds, infected_mosquitoes, total_mosquitoes
## HUMANS (infected) 2013-2018
writeRaster(infected_humans18, "G:/My Drive/EEID/West Nile Virus/infected_humans18.tif")
writeRaster(infected_humans18, "G:/My Drive/EEID/West Nile Virus/infected_humans17.tif")
writeRaster(infected_humans18, "G:/My Drive/EEID/West Nile Virus/infected_humans16.tif")
writeRaster(infected_humans18, "G:/My Drive/EEID/West Nile Virus/infected_humans15.tif")
writeRaster(infected_humans18, "G:/My Drive/EEID/West Nile Virus/infected_humans14.tif")
writeRaster(infected_humans18, "G:/My Drive/EEID/West Nile Virus/infected_humans13.tif")
writeRaster(total_humans, "G:/My Drive/EEID/West Nile Virus/total_humans.tif")
writeRaster(infected_mosquitoes, "G:/My Drive/EEID/West Nile Virus/infected_mosquitoes.tif")
## MOSQUITOES (total population) 2013-2018
writeRaster(CA18_1_total_mosquitoes, "G:/My Drive/EEID/West Nile Virus/CA18_1_total_mosquitoes.tif")
writeRaster(CA18_2_total_mosquitoes, "G:/My Drive/EEID/West Nile Virus/CA18_2_total_mosquitoes.tif")
writeRaster(CA18_3_total_mosquitoes, "G:/My Drive/EEID/West Nile Virus/CA18_3_total_mosquitoes.tif")
writeRaster(CA18_4_total_mosquitoes, "G:/My Drive/EEID/West Nile Virus/CA18_4_total_mosquitoes.tif")
writeRaster(CA18_5_total_mosquitoes, "G:/My Drive/EEID/West Nile Virus/CA18_5_total_mosquitoes.tif")
writeRaster(CA18_6_total_mosquitoes, "G:/My Drive/EEID/West Nile Virus/CA18_6_total_mosquitoes.tif")
writeRaster(CA18_7_total_mosquitoes, "G:/My Drive/EEID/West Nile Virus/CA18_7_total_mosquitoes.tif")
writeRaster(CA18_8_total_mosquitoes, "G:/My Drive/EEID/West Nile Virus/CA18_8_total_mosquitoes.tif")
writeRaster(CA18_9_total_mosquitoes, "G:/My Drive/EEID/West Nile Virus/CA18_9_total_mosquitoes.tif")
writeRaster(CA18_10_total_mosquitoes, "G:/My Drive/EEID/West Nile Virus/CA18_10_total_mosquitoes.tif")
writeRaster(CA18_11_total_mosquitoes, "G:/My Drive/EEID/West Nile Virus/CA18_11_total_mosquitoes.tif")
writeRaster(CA18_12_total_mosquitoes, "G:/My Drive/EEID/West Nile Virus/CA18_12_total_mosquitoes.tif")


writeRaster(infected_birds, "G:/My Drive/EEID/West Nile Virus/infected_birds.tif")
## BIRDS (total population) 2013-2018
writeRaster(total_birds18, "G:/My Drive/EEID/West Nile Virus/total_birds18.tif", overwrite = TRUE)
writeRaster(total_birds17, "G:/My Drive/EEID/West Nile Virus/total_birds17.tif")
writeRaster(total_birds16, "G:/My Drive/EEID/West Nile Virus/total_birds16.tif")
writeRaster(total_birds15, "G:/My Drive/EEID/West Nile Virus/total_birds15.tif")
writeRaster(total_birds14, "G:/My Drive/EEID/West Nile Virus/total_birds14.tif")
writeRaster(total_birds13, "G:/My Drive/EEID/West Nile Virus/total_birds13.tif")


