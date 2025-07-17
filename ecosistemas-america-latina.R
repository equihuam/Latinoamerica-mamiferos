library(readr)

ecos_amlat_arch <- list.files("C:/Users/equih/1 Nubes/OneDrive - El Instituto de Ecología/America Latina y el Caribe/", 
                         pattern = "rle-query-results_rectángulo_america_larina_y_caribe.csv", 
                         full.names = TRUE)

ecos_amlat <- read_csv(ecos_amlat_arch)
