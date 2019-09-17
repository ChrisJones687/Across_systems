### Data preparation for West Nile epidemic data at the county level in California ###
library(raster)
library(sp)
library(rgdal)
library(dplyr)
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
## call CA county data for humans infected by year
CAcounties18_human <- CAcounties18[,names(CAcounties18)[9]]
CAcounties17_human <- CAcounties17[,names(CAcounties17)[9]]
CAcounties16_human <- CAcounties16[,names(CAcounties16)[9]]
CAcounties15_human <- CAcounties15[,names(CAcounties15)[9]]
CAcounties14_human <- CAcounties14[,names(CAcounties14)[9]]
CAcounties13_human <- CAcounties13[,names(CAcounties13)[9]]
## rasterize CA county data for humans infected by year
humans18 <- rasterize(CAcounties18_human, california_nlcd_3000m, field = "human", fun = 'last')
humans17 <- rasterize(CAcounties17_human, california_nlcd_3000m, field = "human", fun = 'last')
humans16 <- rasterize(CAcounties16_human, california_nlcd_3000m, field = "human", fun = 'last')
humans15 <- rasterize(CAcounties15_human, california_nlcd_3000m, field = "human", fun = 'last')
humans14 <- rasterize(CAcounties14_human, california_nlcd_3000m, field = "human", fun = 'last')
humans13 <- rasterize(CAcounties13_human, california_nlcd_3000m, field = "human", fun = 'last')

## randomly assign infected humans to a given number of cells in a county
for (i in 1:nrow(CAcounties)) {
  ex <- extract(humans18, CAcounties[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  samples <- sample(ex, 3, replace = FALSE)
  humans18[ex] <- 0
  humans18[samples] <- ceiling(CAcounties18_human$human[i]/3)
}
plot(humans18)
plot(CAcounties, add = TRUE)
