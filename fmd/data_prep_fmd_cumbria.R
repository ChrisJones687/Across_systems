### 2001 UK epidemic model in Cumbria
library(raster)
library(sf)

# Read in data
cumbria_farms <- read.csv("G:/My Drive/EEID/Foot and Mouth Disease/cumbria_uk_cattle.csv")
uk_counties <- readOGR("C:/Users/lantzra/Downloads/Counties_and_Unitary_Authorities_December_2016_Full_Clipped_Boundaries_in_England_and_Wales.kml")
uk_raster <- raster("C:/Users/lantzra/Downloads/")

cumbria_cattle <- rasterFromXYZ(cumbria_farms, res = c(NA,NA), crs = NA, digits = 5)
cumbria_cattle
plot(cumbria_cattle)

# Create raster of cattle across UK
great_britain <- readOGR("C:/Users/lantzra/Downloads/Great_Britain_shapefile/gb_10km.shp")
plot(great_britain)

# Convert UK counties kml to shapefile
uk_counties <- st_read("C:/Users/lantzra/Downloads/Counties_and_Unitary_Authorities_December_2016_Full_Clipped_Boundaries_in_England_and_Wales.kml")
uk_counties <- rbind(uk_counties)
st_write(uk_counties, "C:/Users/lantzra/Downloads/uk_counties.shp")
uk_counties <- readOGR("C:/Users/lantzra/Downloads/uk_counties.shp")