library("raster")
library("sp")
library(readxl)


num_cols <- 45
num_rows <- 3
resolution <- 2.5
infected <- matrix(0, nrow = num_rows, ncol = num_cols)

infected <- raster(infected, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
infected[2,23]<-100
plot(infected)

total_plants<-infected
writeRaster(infected, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/infected.tif")


#####Mixture

num_cols <- 96
num_rows <- 32
resolution <- 2.5
host <- matrix(25, nrow = num_rows, ncol = num_cols)
host <- raster(host, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(host)
writeRaster(host, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/mix_host.tif")

num_cols <- 96
num_rows <- 32
resolution <- 2.5
total <- matrix(100, nrow = num_rows, ncol = num_cols)
total <- raster(total, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(total)
writeRaster(total, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/mix_total.tif")


num_cols <- 96
num_rows <- 32
resolution <- 2.5
infected <- matrix(0, nrow = num_rows, ncol = num_cols)
infected <- raster(infected, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
infected[c(16,17),c(8,9)]<-100
plot(infected)
writeRaster(infected, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/mix_infected.tif")

#####Pure

num_cols <- 96
num_rows <- 32
resolution <- 2.5
host <- matrix(100, nrow = num_rows, ncol = num_cols)
host <- raster(host, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(host)
writeRaster(host, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEI/Across_systems/wheat stripe rust/Pure_host.tif")

num_cols <- 96
num_rows <- 32
resolution <- 2.5
total <- matrix(100, nrow = num_rows, ncol = num_cols)
total <- raster(total, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(total)
writeRaster(total, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/Pure_total.tif")


num_cols <- 96
num_rows <- 32
resolution <- 2.5
infected <- matrix(0, nrow = num_rows, ncol = num_cols)
infected <- raster(infected, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
infected[16,8]<-100
plot(infected)
writeRaster(infected, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/Pure_infected.tif")

## set for actual infections
num_cols <- 96
num_rows <- 32
resolution <- 2.5
infected_w1 <- matrix(0, nrow = num_rows, ncol = num_cols)
infected_w1 <- raster(infected_w1, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(infected_w1)
infected_w2 <- infected_w3 <- infected_w4 <- infected_w5 <- infected_w1
real_wsr_infections <- read_excel('Mixture_Interpolation.xlsx', sheet= 'week5')


for (i in 8:(num_cols-7)){
  infected_w5[seq(1,nrow(infected),1), i]<- real_wsr_infections$Disease[i]
}
plot(infected_w5)
infected_w5[seq(1,nrow(infected),1), c(1:7)]<- 0
infected_w5[seq(1,nrow(infected),1), c((num_cols-7):num_cols)]<- 0
plot(infected_w5)
infected_weeks <- stack(infected_w1, infected_w2, infected_w3, infected_w4, infected_w5)

writeRaster(infected_weeks, "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/Across_systems/wheat stripe rust/real_infected.tif")

















## set for actual infections
num_cols <- 96
num_rows <- 32
resolution <- 2.5
infected_w1 <- matrix(0, nrow = num_rows, ncol = num_cols)
infected_w1 <- raster(infected_w1, xmn = 0, ymn = 0, xmx = num_cols*resolution, ymx = num_rows*resolution)
plot(infected_w1)
infected_w2 <- infected_w3 <- infected_w4 <- infected_w5 <- infected_w1
real_wsr_infections <- read_excel('Mixture_Interpolation.xlsx', sheet= 'week1')


for (i in 8:(num_cols-7)){
  infected_w1[seq(1,nrow(infected),1), i]<- real_wsr_infections$Disease[i]
}
plot(infected_w1)
infected_w1[seq(1,nrow(infected),1), c(1:7)]<- 0
infected_w1[seq(1,nrow(infected),1), c((num_cols-7):num_cols)]<- 0
plot(infected_w1)
infected_w1[1,num_cols-9]
infected_weeks <- stack(infected_w1)


s <- seq(8, num_cols-8, 1)

