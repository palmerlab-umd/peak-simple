# calculate PPM dat for gc_data using standards
library(dplyr)

calculate_ppms <- function(gc_data, ch4_stds, co2_stds){
  ch4_calb <- lm(ch4_peak_area ~ 0 + ppm_value, data = ch4_stds)
  ch4_rsquared <- summary(ch4_calb)$adj.r.squared
  b1 <- ch4_calb$coefficients["(Intercept)"]
  m1 <- ch4_calb$coefficients["ppm_value"]
  ch4_stderror <- summary(ch4_calb)$coefficients[,2] # std errors
  message(paste(crayon::cyan("CH4 Adj. R Squared:"), ch4_rsquared))
  
  co2_calb <- lm(co2_peak_area ~ 0 + ppm_value, data = co2_stds)
  co2_rsquared <- summary(co2_calb)$adj.r.squared
  b2 <- co2_calb$coefficients["(Intercept)"]
  m2 <- co2_calb$coefficients["ppm_value"]
  co2_stderror <- summary(co2_calb)$coefficients[,2] # std errors
  message(paste(crayon::green("CO2 Adj. R Squared:"), co2_rsquared))
  
  if(is.na(b1)) {b1 = 0}
  if(is.na(b2)) {b2 = 0}
  
  sample_data <- gc_data %>% filter(exetainer_ID !="") %>%
    mutate(ch4_ppm = (ch4_peak_area - b1)/m1) %>% 
    mutate(co2_ppm = (co2_peak_area - b2)/m2) %>% 
    dplyr::select(exetainer_ID, ch4_ppm, ch4_retention_time, 
                  ch4_peak_area, ch4_peak_height,
                  co2_ppm, co2_retention_time, co2_peak_area, 
                  co2_peak_height, machineID, gc_date, gc_time)
  
  return(sample_data)
}







# add in uncertainty handling for range of ppm values

# calb_Sint <- summary(sample_data[[3]])$coefficients[1,2]
# calb_Sslope <- summary(sample_data[[3]])$coefficients[2,2]
# calb_int <- sample_data[[3]]$coefficients[1]
# calb_slope <- sample_data[[3]]$coefficients[2]
# 
# sample_data[[1]][["calculated_error"]] <- sample_data[[1]]$ch4_ppm * sqrt((calb_Sint/(sample_data[[1]]$Area1 - calb_int))^2 + (calb_Sslope/calb_slope)^2)
# 
# sample_data[[1]][["percent_calculated_error"]] <- sample_data[[1]][["calculated_error"]]/sample_data[[1]]$ch4_ppm*100
