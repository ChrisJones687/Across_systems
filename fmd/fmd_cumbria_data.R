#devtools::install_github("ncsu-landscape-dynamics/rpops")
library(PoPS)
library(raster)

infected_file <- "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/infected.tif"
host_file <- "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/host.tif"
total_plants_file <- "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/total_hosts.tif"
temperature_file <- ""
temperature_coefficient_file <- ""
precipitation_coefficient_file <-""
use_lethal_temperature <- FALSE
temp <- FALSE
precip <- FALSE
season_month_start <- 2
season_month_end <- 9
time_step <- "day"
start_time <- 2001
end_time <- 2001
lethal_temperature <- -35
lethal_temperature_month <- 1
random_seed <- 42
reproductive_rate <- 500
treatments_file <- ""
treatment_years <- c(0)
treatment_month <- 12
treatment_method <- "ratio"
management <- FALSE
mortality_on <- TRUE
mortality_rate <- 0.95
mortality_time_lag <- 9
percent_natural_dispersal <- 1.0
natural_kernel_type <- "cauchy"
anthropogenic_kernel_type <- "cauchy"
natural_distance_scale <- 300
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

plot(raster(data$infected[[1]]))

infected_years_file <- "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/infected_2001.tif"
num_iterations <- 30
start_reproductive_rate <- 450
start_natural_distance_scale <- 350
sd_reproductive_rate <- 0.2
sd_natural_distance_scale <- 4
number_of_cores <- 6

infected_file <- "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/infected.tif"
host_file <- "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/host.tif"
total_plants_file <- "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/total_hosts.tif"
temperature_file <- ""
temperature_coefficient_file <- ""



mask = NULL
success_metric = "quantity"

params_slf <- calibrate(infected_years_file, num_iterations, start_reproductive_rate, number_of_cores,
                        start_natural_distance_scale, sd_reproductive_rate, sd_natural_distance_scale,
                        infected_file, host_file, total_plants_file,
                        temp, temperature_coefficient_file,
                        precip = FALSE, precipitation_coefficient_file,
                        time_step, reproductive_rate,
                        season_month_start, season_month_end,
                        start_time, end_time,
                        use_lethal_temperature, temperature_file,
                        lethal_temperature, lethal_temperature_month,
                        mortality_on, mortality_rate, mortality_time_lag,
                        management, treatment_years, treatments_file,
                        treatment_method, treatment_month,
                        percent_natural_dispersal = 1.0,
                        natural_kernel_type = "cauchy", anthropogenic_kernel_type = "cauchy",
                        natural_distance_scale = 21, anthropogenic_distance_scale = 0.0,
                        natural_dir = "NONE", natural_kappa = 0,
                        anthropogenic_dir = "NONE", anthropogenic_kappa = 0,
                        mask, success_metric)
params_slf

table(params_slf$reproductive_rate)/nrow(params_slf)*100
table(params_slf$natural_distance_scale)/nrow(params_slf)*100


infected_years_file <- "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/infected_2001.tif"
num_iterations <- 30

infected_file <- "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/infected.tif"
host_file <- "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/host.tif"
total_plants_file <- "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/total_hosts.tif"
temperature_file <- ""
temperature_coefficient_file <- ""
precipitation_coefficient_file <-""
use_lethal_temperature <- FALSE
temp <- FALSE
precip <- FALSE
season_month_start <- 2
season_month_end <- 9
time_step <- "day"
start_time <- 2001
end_time <- 2001
lethal_temperature <- -35
lethal_temperature_month <- 1
random_seed <- 42
reproductive_rate <- 20.45
treatments_file <- ""
treatment_years <- c(0)
treatment_month <- 12
treatment_method <- "ratio"
management <- FALSE
mortality_on <- TRUE
mortality_rate <- 0.75
mortality_time_lag <- 9
percent_natural_dispersal <- 1.0
natural_kernel_type <- "cauchy"
anthropogenic_kernel_type <- "cauchy"
natural_distance_scale <- 41
anthropogenic_distance_scale <- 0.0
natural_dir <- "NONE"
natural_kappa <- 0
anthropogenic_dir <- "NONE"
anthropogenic_kappa <- 0

validate_slf <- validate(infected_years_file, num_iterations, number_of_cores,
                        infected_file, host_file, total_plants_file,
                        temp, temperature_coefficient_file,
                        precip = FALSE, precipitation_coefficient_file,
                        time_step, reproductive_rate,
                        season_month_start, season_month_end,
                        start_time, end_time,
                        use_lethal_temperature, temperature_file,
                        lethal_temperature, lethal_temperature_month,
                        mortality_on, mortality_rate, mortality_time_lag,
                        management, treatment_years, treatments_file,
                        treatment_method, treatment_month,
                        percent_natural_dispersal = 1.0,
                        natural_kernel_type = "cauchy", anthropogenic_kernel_type = "cauchy",
                        natural_distance_scale = 21, anthropogenic_distance_scale = 0.0,
                        natural_dir = "NONE", natural_kappa = 0,
                        anthropogenic_dir = "NONE", anthropogenic_kappa = 0,
                        mask, success_metric)
validate_slf

# weather_2005 <- stack("")
# weather_2005 <- weather_2005[[157:nlayers(weather_2005)]]
# 
# cum_inf_2002_2004 <- stack(raster("G:/My Drive/EEID/Foot and Mouth Disease/cumbria/infected_2001.tif"))
# 
# writeRaster(cum_inf_2002_2004, "G:/My Drive/EEID/Foot and Mouth Disease/cumbria/infected_2001.tif")
#writeRaster(weather_2005, "")

