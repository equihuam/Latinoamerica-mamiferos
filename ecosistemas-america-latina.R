pacman::p_load(readr, stringr, dplyr, terra)


proyect_lst <- "C:/Users/equih/1 Nubes/OneDrive - El Instituto de Ecología/America Latina y el Caribe/"

ecos_amlat_arch <- list.files(proyect_lst, 
                              pattern = "rle-query-results_ame.*csv$",
                              full.names = TRUE)

# Unidades ecológicas en latinoamérica
ecos_amlat <- read_csv(ecos_amlat_arch, show_col_types = FALSE) |> 
  select(1:6) |> 
  unique()

json_lst <- "C:/Users/equih/1 Nubes/OneDrive - El Instituto de Ecología/America Latina y el Caribe/all-maps-vector-geojson"

# Jsons de unidades ecológicas
ecos_amlat_jsons <- tibble(file = list.files(json_lst)) |>
  filter(str_detect(file, "No-en|lat-amer", negate = TRUE)) |> 
  mutate(ID = str_extract(file, "\\w+\\d+\\.\\d+")) |> 
  relocate(ID, .before = file) |> 
  right_join(ecos_amlat)


map_lim <- vect(paste0(proyect_lst, "/", 
                       "límites continentales y marinos.shp"))
for (mapa_n in ecos_amlat_jsons$file[67:69])
{
  mapa <- vect(paste0(json_lst, "/", mapa_n))
  mapa_1 <- crop(mapa, map_lim)
  writeVector(mapa_1, paste0(json_lst, "/lat-amer/", mapa_n), 
              filetype = "GeoJSON", overwrite = TRUE)
}

plot(mapa)

