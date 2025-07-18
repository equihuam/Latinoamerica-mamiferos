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

# define OGR_GEOJSON_MAX_OBJ_SIZE option set to 0 to remove any size limit
Sys.setenv("OGR_GEOJSON_MAX_OBJ_SIZE"=0)

map_lim <- vect(paste0(proyect_lst, "/", 
                       "límites continentales y marinos.shp"))
for (i in (95:length(ecos_amlat_jsons$file)))
{
  if(!is.na(ecos_amlat_jsons$file[i]))
  {
    mapa_i <- ecos_amlat_jsons$file[i]
    mapa <- vect(paste0(json_lst, "/", mapa_i))
    mapa_crop <- crop(mapa, map_lim)
    nombre <-  paste0(ecos_amlat_jsons$ID[i], "-", 
                      ecos_amlat_jsons$Group[i], ".json")
    print(nombre)
    if (!is.empty(mapa_crop))
    {
      nombre <- str_replace(nombre, "/", "-")
      writeVector(mapa_crop, paste0(json_lst, "/lat-amer-1/", nombre), 
                  filetype = "GeoJSON", overwrite = TRUE)
      print(paste0("-->>", " guardado"))
    }
  }
}



