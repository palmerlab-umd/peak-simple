# filter gc_peaks data for standards for calibration curve
# based on names given in std_values data

library(dplyr)
get_stds <- function(gc_data, std_values, gas = "ch4", savedata = TRUE){
  
  stds_sample_types <- std_values$sample_type[which(std_values$gas %in% gas)]
  std_file_name <- paste0(gas,"_stds_", range(gc_data$machineID)[1], "-", range(gc_data$machineID)[2], ".csv")
  
  if(gas == "ch4"){
    stds <- filter(gc_data, sample_type %in% stds_sample_types) %>%
      left_join(std_values, by = "sample_type") %>%
      dplyr::select(machineID, ch4_retention_time, ch4_peak_area, ch4_peak_height, sample_type, ppm_value)
  }
  
  if(gas == "co2"){
    stds <- filter(gc_data, sample_type %in% stds_sample_types) %>%
      left_join(std_values, by = "sample_type") %>%
      dplyr::select(machineID, co2_retention_time, co2_peak_area, co2_peak_height, sample_type, ppm_value)
  }
  
  if(savedata){
    if(!dir.exists(file.path("data", "stds"))){
      dir.create(file.path("data", "stds"))
    }
    write.csv(stds, file = file.path("data", "stds", std_file_name), row.names = FALSE)  
  }

  
  return(stds)
}

# doesn't handle getting rid of bad standards!
