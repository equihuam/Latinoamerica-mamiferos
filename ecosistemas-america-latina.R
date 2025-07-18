pacman::p_load(readr, stringr, dplyr, terra)


proyect_lst <- "C:/Users/equih/1 Nubes/OneDrive - El Instituto de Ecología/America Latina y el Caribe/"

ecos_amlat_arch <- list.files(proyect_lst, 
                              pattern = "rle-query-results_ame.*csv$",
                              full.names = TRUE)

# Unidades ecológicas en latinoamérica
ecos_amlat_dat <- read_csv(ecos_amlat_arch, show_col_types = FALSE) |> 
  select(1:6) |> 
  unique()

mapa_latam_lst <- "C:/Users/equih/1 Nubes/OneDrive - El Instituto de Ecología/America Latina y el Caribe/"

tiff_lst <- "C:/Users/equih/1 Nubes/OneDrive - El Instituto de Ecología/America Latina y el Caribe/all-maps-raster-geotiff"

json_lst <- "C:/Users/equih/1 Nubes/OneDrive - El Instituto de Ecología/America Latina y el Caribe/all-maps-vector-geojson"

# geotiffs de unidades ecológicas
ecos_amlat_tiffs <- tibble(file_tif = list.files(tiff_lst)) |>
  filter(str_detect(file_tif, "No-en|lat-amer", negate = TRUE)) |> 
  mutate(ID = str_extract(file_tif, "\\w+\\d+\\.\\d+")) |> 
  relocate(ID, .before = file_tif) |> 
  right_join(ecos_amlat_dat) 


# Jsons de unidades ecológicas
ecos_amlat_all <- tibble(file_json = list.files(json_lst)) |>
  filter(str_detect(file_json, "No-en|lat-amer", negate = TRUE)) |> 
  mutate(ID = str_extract(file_json, "\\w+\\d+\\.\\d+")) |> 
  relocate(ID, .before = file_json) |> 
  right_join(ecos_amlat_dat) |> 
  inner_join(ecos_amlat_tiffs) |> 
  relocate(file_tif, .after = file_json)


 # define OGR_GEOJSON_MAX_OBJ_SIZE option set to 0 to remove any size limit
Sys.setenv("OGR_GEOJSON_MAX_OBJ_SIZE"=0)

map_lim <- vect(paste0(proyect_lst, "/", 
                       "límites continentales y marinos.shp"))
for (i in (1:length(ecos_amlat_all$ID)))
{
  if(!is.na(ecos_amlat_all$file_json[i]))
  {
    # vectores
    mapa_v_i <- ecos_amlat_all$file_json[i]
    mapa_v <- vect(paste0(json_lst, "/", mapa_v_i))
    mapa_v_crop <- crop(mapa_v, map_lim)
    nombre_v <-  paste0(ecos_amlat_all$ID[i], "-", 
                      ecos_amlat_all$Group[i], ".json")
    print(nombre_v)
    
    # raster
    mapa_r_i <- ecos_amlat_all$file_tif[i]
    mapa_r  <-  rast(paste0(tiff_lst, "/", mapa_r_i))
    mapa_r_crop <- crop(mapa_r, map_lim)
    nombre_r <-  paste0(ecos_amlat_all$ID[i], "-", 
                        ecos_amlat_all$Group[i], ".tif")
    print(nombre_r )
    
    if (!is.empty(mapa_v_crop))
    {
      nombre_v <- str_replace(nombre_v, "/", "-")
      nombre_r <- str_replace(nombre_r, "/", "-")
      writeVector(mapa_v_crop, 
                  paste0(mapa_latam_lst, "/lat-amer/vect/", nombre_v), 
                  filetype = "GeoJSON", overwrite = TRUE)
      writeRaster(mapa_r_crop, 
                  paste0(mapa_latam_lst, "/lat-amer/rast/", nombre_r), 
                  filetype = "GTiff", overwrite = TRUE)
      print(paste0("-->>", " guardados"))
    }
  }
}



