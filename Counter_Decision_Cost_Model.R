# Counter-Cost-and-Decision-Model
# Created: April 08, 2015 for Marine Scotland Science
# Last edited: December 11, 2015
# Author: Douglas Braun - InStream Fisheries Research
# Email: dbraun@instream.net

# Load libraries.
library(plyr)
library(dplyr)

# Set working directory.
setwd()

##################################

# Import site data and counter options.
counter_option   <- read.csv("Counter Option.csv", stringsAsFactors = FALSE)
counter_option$site <- site_d$site

site_d     		<- read.csv("Site Data.csv", stringsAsFactors = FALSE)[2,]
counter_acc 	<- read.csv("Counter Accuracy.csv", stringsAsFactors = FALSE)
validation_d 	<- read.csv("Validation Data.csv", stringsAsFactors = FALSE)

######################################

# Set parameters for day rates in GBP.
# Privide currency in description.
eng_day_rate 	<- 500
bio_day_rate 	<- 300
tech_day_rate <- 200

######################################

################################################################################
# The first section of the Cost-and-Decision-Model eliminates counter options 
# that are not suitable for the site characterisitics. For example, if 
# conductivity is too low (i.e., <20 uS) then resistivity counters can not be 
# used. Other considerations include if there is mains power or will the counter
# setup be powered by batteries.
################################################################################

counter_cost_model <- function(site_d, counter_option, counter_acc, validation_d){
  
  # Decision 1 - what is the site turbidity (NTU)?
  # This could be max turb, mean turb, prob should be max.
  # NEED to define.
  # x is the site data.
  # y is the counter options.
  
  node_1 <- function(x, y){
    
    if(x$turbidity[1] > 90){
      counter_opt_1 <- dplyr::filter_(counter_option, ~technology != "optical beam")
    }
    
    else
      counter_opt_1 <- y
  }
  
  counter_opt_1 <- node_1(site_d, counter_option)
  
  ##################################
  
  # Decision 2 - what is the site conductivity (ucms)?
  # This could be max cond, mean cond, prob should be max.
  # NEED to define.
  # x is the site data.
  # y is the counter options.
  
  node_2 <- function(x, y){
    
    if(x$conductivity[1] < 20){
      counter_opt_2 <- dplyr::filter_(y, ~technology != "resistivity")
    }
    
    else
      counter_opt_2 <- y
  }
  
  counter_opt_2 <- node_2(site_d, counter_opt_1)
  
  ##################################
  
  # Decision 3 - what is the channel type? (possible answers: fishway or river).
  # x is the site data.
  # y is the counter options.
  
  node_3 <- function(x, y){
    
    if(x$channel_type[1] %in% c("fish pass")){
      counter_opt_3 <- dplyr::filter_(y, ~channel_type == "fish pass")
    }
    
    else if(x$channel_type[1] %in% c("river")){
      counter_opt_3 <- dplyr::filter_(y, ~channel_type == "river")
    }
  }
  
  counter_opt_3 <- node_3(site_d, counter_opt_2)
  
  ##################################
  
  # Decision 4 - what is the existing power type? (possible answers: mains, none).
  # If there is no power type, what is the prefered power type? (possible answers, mains, battery, either).
  # Either keeps both mains and battery in so that they are both priced out.
  # x is the site data.
  # y is the counter options.
  
  node_4 <- function(x, y){
    
    if(x$existing_power_type[1] == "mains"){
      counter_opt_4 <- dplyr::filter_(y, ~power_type == "mains")
    }
    
    else if(x$existing_power_type[1] == "none" && site_d$preferred_power_type[1] == "battery"){
      counter_opt_4 <- dplyr::filter_(y, ~power_type == "battery")
    }		
    
    else if(x$existing_power_type[1] == "none" && x$preferred_power_type[1] == "mains"){
      counter_opt_4 <- dplyr::filter_(y, ~power_type == "mains")		
    }
  }
  
  counter_opt_4 <- node_4(site_d, counter_opt_3)
  
  ##################################
  
  # Decision 5 - If the water depth is greater than 100 cm get rid of flat pad sensors
  # x is the site data
  # y is the counter options
  
  node_5 <- function(x, y){
    
    if(x$max_depth > 100){
      counter_opt_5 <- dplyr::filter_(y, ~sensor != "flat pad")
    }
    
    else
      counter_opt_5 <- y
  }
  
  counter_opt_5 <- node_5(site_d, counter_opt_4)
  
  ##################################
  
  # Decision 6 - Is the water wadeable at the time of year a counter needs to be installed? 
  # If not wadeable then get rid of picket fence and floating fence structures, and flat pad sensors.
  # x is the site data
  # y is the counter options
  
  node_6 <- function(x, y){
    
    if(x$wadeable == "no"){
      counter_opt_6 <- dplyr::filter_(y, ~!structure %in% c("picket fence") & !sensor %in% c("flat pad"))
    }
    
    else if(x$wadeable == "yes"){
      counter_opt_6 <- y
    }
  }
  
  counter_opt_6 <- node_6(site_d, counter_opt_5)
  
  ##################################
  
  # Decision 7 - Is the water wadeable at the time of year a counter needs to be installed? 
  # If not wadeable then get rid of picket fence and floating fence structures, and flat pad sensors.
  # x is the site data
  # y is the counter options
  
  node_7 <- function(x, y){
    
    if(x$min_depth < 90){
      counter_opt_7 <- dplyr::filter_(y, ~!technology %in% c("hydroacoustic"))
    }
    
    else 
      counter_opt_7 <- y
    
  }
  
  counter_opt_7 <- node_7(site_d, counter_opt_6)
  
  node_8 <- function(x, y){
    
    if(x$turbidity > 30){
      counter_opt_8 <- dplyr::filter_(y, ~!technology %in% c("video"))
    }
    
    else
      counter_opt_8 <- y
  }
  
  counter_opt_8 <- node_8(site_d, counter_opt_7)
  
  ##################################
  
  # Decision 8 - Is the water wadeable at the time of year a counter needs to be installed? 
  # If not wadeable then remove of picket fence and floating fence structures, and flat pad sensors.
  # x is the site data
  # y is the counter options
  
  counter_opt_9 <- dplyr::filter_(counter_opt_8, ~!maximum_ww < site_d$ww)	
  
  ################################################################################
  # The second section of the cstool determines the cost of each counter. This is 
  # broken down into the upfront captial costs (e.g., purchasing the counter) and
  # the annual costs (e.g., maintenance). 
  ################################################################################
  
  counter_opt <- ddply(counter_opt_9, c("site",
                                        "option", 
                                        "technology", 
                                        "company", 
                                        "settings", 
                                        "counter", 
                                        "no_counter", 
                                        "company_risk", 
                                        "structure_risk", 
                                        "validation_risk", 
                                        "structure", 
                                        "structure_pad", 
                                        "counter_accuracy", 
                                        "power_type", 
                                        "counting_software", 
                                        "validation_type"), 
                       function(x){
                         
                         #################
                         # COUNTER COSTS #
                         #################
                         
                         # Captial costs of the counter.
                         if(site_d$counter_backup == "yes"){
                           counter_capital_cost <- x$no_counter * x$counter_cost + x$counter_backup_cost
                         }
                         
                         if(site_d$counter_backup == "no"){
                           counter_capital_cost <- x$no_counter * x$counter_cost
                         }
                         
                         ###################
                         # STRUCTURE COSTS #
                         ###################
                         
                         # Captial costs of the structure.
                         # Most optical beam and resistivity counters require structures other than fences.
                         if(x$structure == "fishway insert" && x$sensor == "optical beam"){  
                           structure_cost <- x$structure_cost_fixed + x$structure_cost_m
                         }
                         
                         if(x$structure == "fishway insert" && x$sensor == "crump weir"){  
                           structure_cost <- x$structure_cost_fixed + x$structure_cost_m
                         }
                         
                         if(x$structure == "fishway insert" && x$sensor =="tubes"){  
                           structure_cost <- x$structure_cost_fixed + x$structure_cost_m
                         }		
                         
                         if(x$structure == "fishway insert" && x$sensor =="flume"){  
                           structure_cost <- x$structure_cost_fixed + x$structure_cost_m
                         }
                         
                         # this is a linear cost function from North West Hydraulics in GBP.
                         if(x$structure == "picket fence" && x$sensor == "optical beam"){  
                           structure_cost <- x$structure_cost_fixed + x$structure_cost_m * site_d$ww[1]
                         }	
                         
                         if(x$structure == "picket fence" && x$sensor == "crump weir"){  
                           structure_cost <- x$structure_cost_fixed + x$structure_cost_m * site_d$ww[1]
                         }	
                         
                         if(x$structure == "picket fence" && x$sensor =="tubes"){  
                           structure_cost <- x$structure_cost_fixed + x$structure_cost_m * site_d$ww[1]
                         }	
                         
                         if(x$structure == "picket fence" && x$sensor =="flume"){  
                           structure_cost <- x$structure_cost_fixed + x$structure_cost_m * site_d$ww[1]
                         }	
                         
                         # this is the cost function from InStream and North West Hydraulics in GBP.
                         if(x$structure == "crump weir" && x$sensor == "optical beam"){ 
                           structure_cost <- 173.27*(site_d$ww[1]^2) + (7390.7 * site_d$ww[1]) + 3636.6
                         }
                         
                         if(x$structure == "crump weir" && x$sensor == "crump weir"){ 
                           structure_cost <- 173.27*(site_d$ww[1]^2) + (7390.7 * site_d$ww[1]) + 3636.6
                         }
                         
                         if(x$structure == "crump weir" && x$sensor =="tubes"){ 
                           structure_cost <- 173.27*(site_d$ww[1]^2) + (7390.7 * site_d$ww[1]) + 3636.6
                         }
                         
                         if(x$structure == "crump weir" && x$sensor =="flume"){ 
                           structure_cost <- 173.27*(site_d$ww[1]^2) + (7390.7 * site_d$ww[1]) + 3636.6
                         }
                         
                         # this is the cost function from Don and Barry in GBP.
                         if(x$structure == "floating fence" && x$sensor == "optical beam"){
                           structure_cost <- x$structure_cost_fixed + x$structure_cost_m * site_d$ww[1]
                         }
                         
                         if(x$structure == "floating fence" && x$sensor == "crump weir"){
                           structure_cost <- x$structure_cost_fixed + x$structure_cost_m * site_d$ww[1]
                         }
                         
                         if(x$structure == "floating fence" && x$sensor =="tubes"){
                           structure_cost <- x$structure_cost_fixed + x$structure_cost_m * site_d$ww[1]
                         }
                         
                         if(x$structure == "floating fence" && x$sensor =="flume"){
                           structure_cost <- x$structure_cost_fixed + x$structure_cost_m * site_d$ww[1]
                         }
                         
                         # Sonar counters don't usually require limited structure. 
                         # Here we add a fence that is half the width of the stream to maximize counter accuracy.
                         # This costs a fence for sonar that is half the max distance it can opperate.
                         # The idea of the fence is to increase accuracy by decreasing the beam needs to reach.
                         
                         if(x$sensor == "multibeam"){  
                           structure_cost <- ifelse(site_d$ww[1] - x$optimal_ww_per_sensor >1, x$structure_cost_fixed + 
                                                      (site_d$ww[1] - x$optimal_ww_per_sensor) * x$structure_cost_m, 0) 
                         }
                         
                         if(x$sensor == "splitbeam"){  
                           structure_cost <- ifelse(site_d$ww[1] - x$optimal_ww_per_sensor >1, x$structure_cost_fixed + 
                                                      (site_d$ww[1] - x$optimal_ww_per_sensor) * x$structure_cost_m, 0) 
                         }
                         
                         if(x$sensor == "video camera"){  
                           structure_cost <- ifelse(site_d$ww[1] - x$optimal_ww_per_sensor >1, x$structure_cost_fixed + 
                                                      (site_d$ww[1] - x$optimal_ww_per_sensor) * x$structure_cost_m, 0) 
                         }
                         
                         # Flat pads don't require much structure either. 
                         # Use a fence when the river is too wide.
                         if(x$sensor == "flat pad"){
                           structure_cost <- ifelse(site_d$ww[1] - x$optimal_ww_per_sensor >1, x$structure_cost_fixed + 
                                                      (site_d$ww[1] - x$optimal_ww_per_sensor) * x$structure_cost_m, 0) 
                         }
                         
                         # Mounting bracket cost.
                         mounting_bracket_cost <- x$mounting_bracket_cost
                         
                         counter_calibration_equipment_cost <- x$counter_calibration_equipment_cost
                         
                         # Capital costs if a concrete sill or chain ballast is needed.
                         if(x$structure_pad == "chain ballast"){  
                           sill_cost <- x$structure_pad_cost
                         }
                         
                         # This is the cost function from InStream and North West Hydraulics in GBP.
                         if(x$structure_pad == "concrete sill"){
                           sill_cost <- 278.08 * (site_d$ww^2) + (1739.5 * site_d$ww) + 15215
                         }
                         
                         # This is a linear cost function from InStream for a floating fence in GBP.
                         if(x$structure_pad == "none"){
                           sill_cost <- 0
                         }
                         
                         # Annual costs of the structure.
                         # Permits - incorporate SEPA's cost sheet for permits.
                         # Time to fill out the permits by bio.
                         permit_cost <- x$permit_cost + (x$permit_day * eng_day_rate)
                         
                         ################
                         # SENSOR COSTS #
                         ################
                         # Some counters require third party sensor units to be built.
                         # This only applies to resistivity counters as other counters don't require additional sensors.
                         sensor_cost <- x$sensor_cost_fixed + (x$sensor_cost_m * site_d$ww)
                         
                         ########################
                         # INFRASTRUCTURE COSTS #
                         ########################
                         # Cost of infrastructure to operate counters.
                         # Includes security shed, power, land costs.
                         # incorporate the cost of a security shed. Is there one there? 
                         # Only place that might not need it is a fishway.
                         if(site_d$security_shed_present[1] == "yes"){
                           security_shed_cost <- 0		
                         }
                         
                         if(site_d$security_shed_present[1] == "no"){
                           security_shed_cost <- x$security_shed_cost
                         }	
                         
                         # Incorporate the cost of adding mains power or getting a battery setup. 
                         # Is there mains power or do you have the batteries?
                         # If no, incoprorate cost; if yes, installation cost is zero.
                         if(site_d$existing_power_type[1] == "none"){
                           if(x$power_type == "battery"){
                             power_install_cost <- x$no_battery * x$battery_cost 
                           }
                           
                           # Need to define how much it costs to install a power line.
                           if(x$power_type == "mains"){
                             power_install_cost <- x$power_install_cost
                           }	
                         }
                         
                         if(site_d$existing_power_type[1] == "mains"){
                           power_install_cost <- 0
                         }
                         # Incorporate the cost of adding adding remote downloading. Is there a celular network?
                         if(site_d$cellular_network[1] == "yes"){
                           remote_downloading_install_cost <- 0 # this should includes the cost of remote access software, mobile internet stick, cell booster.
                         }
                         
                         if(site_d$cellular_network[1] == "no"){
                           remote_downloading_install_cost <- x$cell_booster_cost
                         }
                         
                         land_cost <- site_d$land_cost[1]
                         
                         computer_cost <- x$computer_cost
                         
                         data_storage_cost <- x$data_storage_cost
                         
                         ###################
                         # OPERATION COSTS #
                         ###################
                         # Site visit costs for counter operation.
                         # Need to build in a risk assessment for a site. 
                         # For example, if a counter site has a cellular network 
                         # Remote downloading reduces risk of data loss!
                         
                         # Fixed costs
                         # Annual cost of the counter.
                         # should the bio and tech day rates be a entered parameter or should the be set in the spreadsheet?
                         counter_annual_cost <- x$counter_service_cost_year
                         
                         annual_install_removal_fixed_cost <- (x$annual_install_tech_days * tech_day_rate) + 
                           (x$annual_install_bio_days * bio_day_rate) + 
                           (x$annual_removal_tech_days * tech_day_rate) + 
                           (x$annual_removal_bio_days * bio_day_rate)
                         
                         ##################
                         # Variable costs #
                         ##################
                         remote_download_month_cost <- (x$cell_plan_cost_month * site_d$migration_length_day / 30) + x$remote_access_software_annual_cost
                         
                         equipment_check_cost <- x$site_visit_week_tech * tech_day_rate * site_d$migration_length_day[1] / 7
                         data_download_cost <- x$downloading_freq_week * (tech_day_rate / 16) * site_d$migration_length_day[1] / 2# this suggests it takes an half an hour to download the counter and that it is done every second day.
                         
                         ##################
                         # COUNTING COSTS #
                         ##################
                         # The cost of counting if a counter isn't automated (i.e. hydroaccoustic)	
                         if(x$counting_software == "echoview"){
                           # the cost of counting is a function of the number of fish that can be counted in an minute times the mean abundance for the population time a tech day rate
                           counting_annual_cost <- (x$counting_effort * site_d$population_size_mean[1] / 480) * tech_day_rate
                         }
                         
                         if(x$counting_software == "none" | x$counting_software == "FishTick" ){
                           counting_annual_cost <- (x$counting_effort * site_d$migration_length_day[1]) / 8 * tech_day_rate
                         }
                         
                         if(x$counting_software == "automated"){
                           counting_annual_cost <- 0
                         }
                         
                         counting_software_cost <- x$counting_software_cost
                         
                         ####################
                         # VALIDATION COSTS #
                         ####################
                         # The cost of validation
                         # This is a function that finds the number of fish to be validated based on a number of user specified site characterisitics and management objectives.
                         validation_no <- function(performance_metric, uncertainty_metric, relative_error, counter_accuracy_var, counter_accuracy, no_species){ 
                           
                           r <- which(validation_d$performance_metric == performance_metric & 
                                        validation_d$uncertainty_metric == uncertainty_metric & 
                                        validation_d$relative_error == relative_error &
                                        validation_d$counter_accuracy_var == counter_accuracy_var &
                                        validation_d$counter_accuracy == counter_accuracy &
                                        validation_d$no_species == no_species)
                           
                           no_fish <- validation_d$no_fish[r]
                           return(no_fish)
                         }
                         
                         # This applys the function and generates a new column in the options dataset that can be used to determine the amount of time to validate x number of fish.
                         for(i in 1:dim(x)[1]){
                           
                           x$validation[i] <-  validation_no(performance_metric = site_d$performance_metric[1], 
                                                             uncertainty_metric = site_d$uncertainty_metric[1], 
                                                             relative_error = site_d$relative_error[1],
                                                             counter_accuracy_var = site_d$counter_accuracy_var[1],
                                                             counter_accuracy = x$counter_accuracy[i],
                                                             no_species = site_d$no_species[1])
                         }
                         
                         
                         # This calcualtes the amount of hours required to validate x number of fish based on population size and migration length.
                         
                         if(x$validation_method == "continuous video"){
                           validation_cost <- 1.1 * x$validation * (site_d$population_size_mean[1] / (site_d$migration_length_day[1] * 24)) * tech_day_rate / 8		
                         }
                         
                         # if validation does not apply to a technology (i.e., sonar) then the cost is zero. 
                         # Need to add in a risk section that highlights the fact that there is no validation and that this is risky!
                         else if(x$validation_method == "none"){
                           validation_cost <- 0
                         }
                         
                         # For some counters with triggered video the cost of validation is much different. 
                         # This is pseudo validation but it is much faster and almost as reliable.
                         else if(x$validation_method == "triggered video"){
                           validation_cost <- x$validation_equipment_cost + (x$validation_effort * site_d$population_size_mean[1] / 480) * tech_day_rate / 8
                         }
                         
                         data.frame(counter_capital_cost, 
                                    structure_cost, 
                                    sill_cost, 
                                    mounting_bracket_cost,
                                    counter_calibration_equipment_cost,
                                    permit_cost,
                                    land_cost, 
                                    sensor_cost, 
                                    computer_cost,
                                    data_storage_cost,
                                    security_shed_cost,
                                    power_install_cost,
                                    remote_downloading_install_cost,
                                    counter_annual_cost,
                                    annual_install_removal_fixed_cost,
                                    remote_download_month_cost,
                                    equipment_check_cost,
                                    data_download_cost,
                                    counting_annual_cost,
                                    counting_software_cost,
                                    validation_cost)
                       })
  
  for(i in 1:length(counter_opt$power_type)){
    if(counter_opt$power_type[i] == "battery"){
      counter_opt$power_install_cost_year10[i] <- counter_opt$power_install_cost[i] * 4
    }
    
    else if(counter_opt$power_type[i] == "mains"){
      counter_opt$power_install_cost_year10[i] <- counter_opt$power_install_cost[i]
    }
  }
  
  #######################
  # Total Capital Costs #
  #######################
  counter_opt$total_counter_capital_cost 		<- counter_opt$counter_capital_cost + 
    counter_opt$sensor_cost
  
  counter_opt$total_structure_capital_cost 	<- counter_opt$structure_cost + 
    counter_opt$mounting_bracket + 
    counter_opt$sill_cost + 
    counter_opt$permit_cost + 
    counter_opt$security_shed_cost + 
    counter_opt$land_cost + 
    counter_opt$counter_calibration_equipment_cost + 
    counter_opt$computer_cost + 
    counter_opt$data_storage_cost +
    counter_opt$power_install_cost + 
    counter_opt$remote_downloading_install_cost + 
    counter_opt$counting_software_cost
  
  ######################
  # Total Annual Costs #
  ######################
  counter_opt$total_annual_operations_cost 	<- counter_opt$counter_annual_cost + 
    counter_opt$annual_install_removal_fixed_cost + 
    counter_opt$remote_download_month_cost + 
    counter_opt$equipment_check_cost + 
    counter_opt$data_download_cost
  
  counter_opt$total_annual_counting_cost 		<- counter_opt$counting_annual_cost
  
  counter_opt$total_annual_validation_cost 	<- counter_opt$validation_cost 
  
  counter_opt$total_captial_cost 						<- counter_opt$total_counter_capital_cost + 
    counter_opt$total_structure_capital_cost 
  
  counter_opt$total_annual_cost 						<- counter_opt$total_annual_operations_cost + 
    counter_opt$total_annual_counting_cost + 
    counter_opt$total_annual_validation_cost
  
  counter_opt$total_cost_year1 							<- counter_opt$total_captial_cost + 
    counter_opt$total_annual_cost
  
  #####################################
  # total capital costs over 10 years #
  #####################################
  counter_opt$total_counter_capital_cost_year10 		<- counter_opt$counter_capital_cost + 
    counter_opt$sensor_cost
  
  counter_opt$total_structure_capital_cost_year10 	<- counter_opt$structure_cost + 
    counter_opt$sill_cost + 
    counter_opt$permit_cost + 
    counter_opt$security_shed_cost + 
    counter_opt$land_cost + 
    counter_opt$power_install_cost_year10 + 
    counter_opt$remote_downloading_install_cost + 
    counter_opt$counting_software_cost
  
  ####################################
  # Total Annual Costs Over 10 Years #
  ####################################
  # This is the cost of the counter over 10 years. Inflation has not been accounted for.
  counter_opt$total_annual_operations_cost_year10 	<- 10 * (counter_opt$counter_annual_cost + 
                                                              counter_opt$annual_install_removal_fixed_cost + 
                                                              counter_opt$remote_download_month_cost + 
                                                              counter_opt$equipment_check_cost + 
                                                              counter_opt$data_download_cost)
  
  counter_opt$total_annual_counting_cost_year10 		<- 10 * counter_opt$counting_annual_cost
  
  counter_opt$total_annual_validation_cost_year10 	<- 10 * counter_opt$validation_cost 	
  
  counter_opt$total_capital_cost_year10							<- counter_opt$total_counter_capital_cost_year10 + 
    counter_opt$total_structure_capital_cost_year10
  
  counter_opt$total_annual_cost_year10							<- counter_opt$total_annual_operations_cost_year10 + 
    counter_opt$total_annual_counting_cost_year10 + 
    counter_opt$total_annual_validation_cost_year10	
  
  counter_opt$total_cost_year10											<- counter_opt$total_capital_cost_year10 + 
    counter_opt$total_annual_cost_year10	
  
  counter_opt
  
  # Save a table for each site.
  write.csv(counter_opt, file = paste("", site_d$site, " - Counter Cost - All Costs.csv", sep = ""), 
            row.names = FALSE)
  
  ################
  # Cost Summary #
  ################
  # Cost_summary_1 is displays the different cost outputs. This is a big table and hard to read but has all info.
  cost_summary_1 <- data.frame("site" = site_d$site[1],
                               "option" = counter_opt$option,
                               "technology" = counter_opt$technology,
                               "company" = counter_opt$company,
                               "settings" = counter_opt$settings,
                               "counter" = counter_opt$counter,
                               "no counters" = counter_opt$no_counter,
                               "sensor" = counter_opt$sensor,
                               "structure" = counter_opt$structure,
                               "structure pad" = counter_opt$structure_pad,
                               "counter accuracy" = counter_opt$counter_accuracy,
                               "counting software" = counter_opt$counting_software,
                               "validation type" = counter_opt$validation_type,
                               "total counter capital cost" = round(counter_opt$total_counter_capital_cost),
                               "total structure capital cost" = round(counter_opt$total_structure_capital_cost),
                               "total annual operations cost" = round(counter_opt$total_annual_operations_cost),
                               "total annual counting cost" = round(counter_opt$total_annual_counting_cost),
                               "total annual validation cost" = round(counter_opt$total_annual_validation_cost),
                               "capital cost" = round(counter_opt$total_captial_cost),
                               "annual cost" = round(counter_opt$total_annual_cost),
                               "total cost" = round(counter_opt$total_cost_year1),
                               "total capital cost over 10 years" = round(counter_opt$total_capital_cost_year10),
                               "total annual cost over 10 years" = round(counter_opt$total_annual_cost_year10),
                               "total cost over 10 years" = round(counter_opt$total_cost_year10)
  )
  
  # Cost_summary_2 provides the year one costs.														
  cost_summary_2 <- data.frame("site" = site_d$site[1],
                               "option" = counter_opt$option,
                               "technology" = counter_opt$technology,
                               "company" = counter_opt$company,
                               "settings" = counter_opt$settings,
                               "counter" = counter_opt$counter,
                               "no counters" = counter_opt$no_counter,
                               "sensor" = counter_opt$sensor,
                               "structure" = counter_opt$structure,
                               "structure pad" = counter_opt$structure_pad,
                               "counter accuracy" = counter_opt$counter_accuracy,
                               "counting software" = counter_opt$counting_software,
                               "validation type" = counter_opt$validation_type,
                               "capital cost" = round(counter_opt$total_captial_cost),
                               "annual cost" = round(counter_opt$total_annual_cost),
                               "total cost" = round(counter_opt$total_cost_year1)
  )														
  
  # Rank the three summary costs.
  summary_2_1 <- cost_summary_2[order(cost_summary_2$capital.cost),]
  summary_2_1$capital_cost_rank <- seq(1:dim(summary_2_1)[1])
  
  summary_2_2 <- summary_2_1[order(summary_2_1$annual.cost),]
  summary_2_2$annual_cost_rank <- seq(1:dim(summary_2_2)[1])
  
  summary_2_3 <- summary_2_2[order(summary_2_2$total.cost),]
  summary_2_3$total_cost_rank <- seq(1:dim(summary_2_3)[1])
  
  # Save a table for each site.
  write.csv(summary_2_3, file = paste("", site_d$site, " - Counter Cost - 1 year Summarized Costs.csv", sep = ""),
            row.names = FALSE)
  
  
  # Cost_summary_3 provides the 10 year costs.														
  cost_summary_3 <- data.frame("site" = site_d$site[1],
                               "option" = counter_opt$option,
                               "technology" = counter_opt$technology,
                               "company" = counter_opt$company,
                               "settings" = counter_opt$settings,
                               "counter" = counter_opt$counter,
                               "no counters" = counter_opt$no_counter,
                               "sensor" = counter_opt$sensor,
                               "structure" = counter_opt$structure,
                               "structure pad" = counter_opt$structure_pad,
                               "counter accuracy" = counter_opt$counter_accuracy,
                               "counting software" = counter_opt$counting_software,
                               "validation type" = counter_opt$validation_type,
                               "total capital cost over 10 years" = round(counter_opt$total_capital_cost_year10),
                               "total annual cost over 10 years" = round(counter_opt$total_annual_cost_year10),
                               "total cost over 10 years" = round(counter_opt$total_cost_year10)
  )														
  
  # Rank the three summary costs
  c1 <- cost_summary_3[order(cost_summary_3$total.capital.cost.over.10.years),]
  c1$total_capital_cost_10_rank <- seq(1:dim(c1)[1])
  
  c2 <- c1[order(c1$total.annual.cost.over.10.years),]
  c2$total_annual_cost_10_rank <- seq(1:dim(c2)[1])
  
  c3 <- c2[order(c2$total.cost.over.10.years),]
  c3$total_cost_10_rank <- seq(1:dim(c3)[1])
  
  # Save a table for each site.
  write.csv(c3, paste("", site_d$site, " - Counter Cost - 10 Year Summarized Costs.csv", sep = ""),
            row.names = FALSE)
  
  
}

counter_cost_model(site_d, counter_option, counter_acc, validation_d)	