library(PoPS)
library(raster)

infected_file <- "H:/My Drive/EEID/Sudden Oak Death/inf_2001.tif"
host_file <- "H:/My Drive/EEID/Sudden Oak Death/lide_300m_median_2001.tif"
total_plants_file <- "H:/My Drive/EEID/Sudden Oak Death/lemma_max300m.tif"
temperature_file <- ""
temperature_coefficient_file <- "H:/My Drive/EEID/Sudden Oak Death/weather_coef_2002_2005.tif"
precipitation_coefficient_file <-""
use_lethal_temperature <- FALSE
temp <- TRUE
precip <- FALSE
season_month_start <- 5
season_month_end <- 11
time_step <- "week"
start_time <- 2002
end_time <- 2003
lethal_temperature <- -35
lethal_temperature_month <- 1
random_seed <- 42
reproductive_rate <- 2.1
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
natural_distance_scale <- 29
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

infected_years_file <- "H:/My Drive/EEID/Sudden Oak Death/cum_inf_2002_2004.tif"
num_iterations <- 10
start_reproductive_rate <- 2.0
start_natural_distance_scale <- 40
sd_reproductive_rate <- 0.2
sd_natural_distance_scale <- 4
number_of_cores <- 11

infected_file <- "H:/My Drive/EEID/Sudden Oak Death/inf_2001.tif"
host_file <- "H:/My Drive/EEID/Sudden Oak Death/inf_2001.tif"
total_plants_file <- "H:/My Drive/EEID/Sudden Oak Death/lemma_max300m.tif"
temperature_file <- ""
temperature_coefficient_file <- "H:/My Drive/EEID/Sudden Oak Death/weather_coef_2002_2005.tif"



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

infected_years_file <- "H:/My Drive/EEID/Sudden Oak Death/cum_inf_2005.tif"
num_iterations <- 10

infected_file <- "H:/My Drive/EEID/Sudden Oak Death/cum_inf_2004.tif"
host_file <- "H:/My Drive/EEID/Sudden Oak Death/lide_300m_median_2001.tif"
total_plants_file <- "H:/My Drive/EEID/Sudden Oak Death/lemma_max300m.tif"
temperature_file <- ""
temperature_coefficient_file <- "H:/My Drive/EEID/Sudden Oak Death/weather_2005.tif"
precipitation_coefficient_file <-""
use_lethal_temperature <- TRUE
temp <- TRUE
precip <- FALSE
season_month_start <- 5
season_month_end <- 11
time_step <- "month"
start_time <- 2018
end_time <- 2018
lethal_temperature <- -35
lethal_temperature_month <- 1
random_seed <- 42
reproductive_rate <- 3.5
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
natural_distance_scale <- 21
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

weather_2005 <- stack("H:/My Drive/EEID/Sudden Oak Death/weather_coef_2002_2005.tif")
weather_2005 <- weather_2005[[157:nlayers(weather_2005)]]

cum_inf_2002_2004 <- stack(raster("H:/My Drive/EEID/Sudden Oak Death/cum_inf_2002.tif"), raster("H:/My Drive/EEID/Sudden Oak Death/cum_inf_2003.tif"),raster("H:/My Drive/EEID/Sudden Oak Death/cum_inf_2004.tif"))

writeRaster(cum_inf_2002_2004, "H:/My Drive/EEID/Sudden Oak Death/cum_inf_2002_2004.tif")
writeRaster(weather_2005, "H:/My Drive/EEID/Sudden Oak Death/weather_2005.tif")
