library(PoPS)
library(raster)

infected_file <- "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/infected.tif"
host_file <- "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/host.tif"
total_plants_file <- "C:/Users/bhimc/OneDrive/Desktop/Oregon State University/Modeling works_EEID/total.tif"
temperature_file <- ""
temperature_coefficient_file <- ""
precipitation_coefficient_file <-""
use_lethal_temperature <- FALSE
temp <- FALSE
precip <- FALSE
season_month_start <- 5
season_month_end <- 6
time_step <- "week"
start_time <- 2019
end_time <- 2019
lethal_temperature <- 0
lethal_temperature_month <- 1
random_seed <- 42
reproductive_rate <- 1.50
treatments_file <- ""
treatment_years <- c(0)
treatment_month <- 12
treatment_method <- "ratio"
management <- FALSE
mortality_on <- FALSE
mortality_rate <- 0
mortality_time_lag <- 0
percent_natural_dispersal <- 1.0
natural_kernel_type <- "cauchy"
anthropogenic_kernel_type <- "cauchy"
natural_distance_scale <- 10
anthropogenic_distance_scale <- 0.0
natural_dir <- "NONE"
natural_kappa <- 0
anthropogenic_dir <- "NONE"
anthropogenic_kappa <- 0

data <- PoPS::pops(infected_file, host_file, total_plants_file, 
                   temp, temperature_coefficient_file, 
                   precip, precipitation_coefficient_file, 
                   time_step, reproductive_rate,
                   season_month_start, season_month_end, 
                   start_time, end_time, 
                   use_lethal_temperature, temperature_file,
                   lethal_temperature, lethal_temperature_month,
                   mortality_on, mortality_rate, mortality_time_lag, 
                   management, treatment_years, treatments_file,
                   treatment_method, treatment_month,
                   percent_natural_dispersal,
                   natural_kernel_type, anthropogenic_kernel_type,
                   natural_distance_scale, anthropogenic_distance_scale,
                   natural_dir, natural_kappa, 
                   anthropogenic_dir, anthropogenic_kappa,
                   random_seed = NULL)


head (data)
plot(raster(data$infected[[1]]))
plot(raster(host_file))
inf <- raster(host_file)
inf[] <- data$infected[[1]]
plot(inf)


write.csv(data$infected[[1]], "infected.csv")


data$infected
data$infected[1]
data$infected[[1]]





tabulate<-data.frame(matrix(NA,3,45))
colnames(tabulate)<-c('distance', 'disease')
tabulate$distance<-c(seq(0, 112.5, by=2.5))
tabulate$disease<-data$infected[[1]]
tabulate

seq(0, 110, by = 2.5) 

Disease_prevalence<-c()

for(i in 1:ncol(data)){
  Disease_prevalence[i]<-(inf[])
}

store<-rep(NA,nrow(start_to_end_data))

for (i in 1:nrow(start_to_end_data)){
  store[i] <- list(specificDate(start_d[i], end_d[i]))
}

store_df <- data.frame(matrix(unlist(store), nrow=nrow(start_to_end_data), byrow=T)) %>%
  `colnames<-`(c("timePeriod", "whichColumn", "Average"))

head(store_df)

write.table (store_df,sep = ',', col.names = T, row.names = FALSE, 'Average.csv' )

