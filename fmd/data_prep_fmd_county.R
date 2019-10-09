### Aggregate zip code cattle to county level for FMD
### Colorado (counties)
counties <- readOGR("G:/My Drive/EEID/Foot and Mouth Disease/us_lower_48_counties.shp")
VA_counties <- counties[counties$STATE_NAME == 'Virginia',]
virginia_nlcd_3000m <- raster("G:/My Drive/EEID/Foot and Mouth Disease/states/VA_nlcd_3000m.img")
VA_counties <- spTransform(VA_counties, CRSobj = crs(virginia_nlcd_3000m))
VA_counties$area_of_pasture <- extract(virginia_nlcd_3000m, VA_counties, fun = sum)
# COcounties_pasture <- extract(colorado_nlcd_300m, COcounties, fun = sum)
writeOGR(COcounties, "G:/My Drive/EEID/Foot and Mouth Disease/virginia_counties_pasturecattle.shp")
virginia_cattle <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/virginia_county_cattle.csv")
names(virginia_cattle)[2] <- "NAME"
VA_counties <- VA_counties[order(VA_counties$NAME),]
VA_counties$cattle <- virginia_cattle$cattle
VA_counties$NAME <- as.character(VA_counties$NAME)
virginia_cattle$NAME <- as.character(virginia_cattle$NAME)
VA_counties2 <- left_join(VA_counties@data, virginia_cattle, by = 'NAME')

# Distribute cattle across the county area
VA_counties$cattle <- as.numeric(VA_counties$cattle)
VA_counties$area_of_pasture <- as.numeric(VA_counties$area_of_pasture)
VA_counties$cattleperarea <- VA_counties$cattle/VA_counties$area_of_pasture
# Average number of cattle in 30 x 30 m of pasture/grassland in the county
VA_counties$cattleperarea
plot(virginia_nlcd_3000m)
plot(VA_counties, add = TRUE)

# Extract the values for the number of cattle for each county
for (i in 1:64) {
  ex <- extract(virginia_nlcd_3000m, VA_counties[i,], cellnumbers = TRUE)
  ex <- data.frame(ex)
  ex <- ex$cell
  virginia_nlcd_300m[ex] <- ceiling(virginia_nlcd_300m[ex]*VA_counties$cattleperarea[i]*100)
}
colorado_nlcd_300m[colorado_nlcd_300m==0] <- NA
colorado_nlcd_300m <- mask(colorado_nlcd_300m, colorado)
plot(colorado_nlcd_300m)