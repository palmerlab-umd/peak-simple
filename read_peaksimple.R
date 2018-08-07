# function to read in one or more peak simple file
library(tidyverse)

read_peaksimple <- function(files){
  # read in and create one data frame
  gc_peaks <- files %>%
    map(function(x) read_csv(x, col_names = FALSE)) %>%
    reduce(rbind) %>%
    distinct()  
  
  # deal with misasligned peaks where no methane detected
  noch4peak <- which(gc_peaks$X11 == "CO2 as methane")
  gc_peaks[noch4peak, 18:23] <- gc_peaks[noch4peak, 11:16]
  gc_peaks[noch4peak, 11:16] <- NA
  # extract numbers from machine sample IDs in first column
  gc_peaks$instrumentID <- as.integer(sapply(gc_peaks$X1, function(x) stringr::str_extract_all(x, "\\(?[0-9]+\\)?")))
  # rename columns
  names(gc_peaks)[c(2,3,13,14,15,20,21,22)] <- c("gc_date", 
                                                 "gc_time",
                                                 "ch4_retention_time", 
                                                 "ch4_peak_area",
                                                 "ch4_peak_height", 
                                                 "co2_retention_time",
                                                 "co2_peak_area",
                                                 "co2_peak_height")
  
  return(gc_peaks)
  
}