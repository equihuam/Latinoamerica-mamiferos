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
  filter(str_detect(file_tif, "No-en|lat-amer|aux", negate = TRUE)) |> 
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

mapa_r_ref <- rast(paste0(tiff_lst, "/", ecos_amlat_all$file_tif[1]))
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
    if(ext(mapa_r) != ext(mapa_r_ref))
    {
      mapa_r <- resample(mapa_r, mapa_r_ref, method = "near")
    }
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
                  filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
      print(paste0("-->>", " guardados"))
    }
  }
}

t1_tif <- ecos_amlat_all[str_detect(ecos_amlat_all$ID, "^T1\\."),]
map_1 <- rast(paste0(tiff_lst, "/", t1_tif$file_tif[1]))
map_2 <- rast(paste0(tiff_lst, "/", t1_tif$file_tif[2]))
map_3 <- rast(paste0(tiff_lst, "/", t1_tif$file_tif[3]))
map_4 <- rast(paste0(tiff_lst, "/", t1_tif$file_tif[4]))

map_3 <- resample(map_3, map_1, method = "near")

ext(map_1)
ext(map_2)
ext(map_3)
ext(map_4)

dim(map_1)
dim(map_2)
dim(map_3)
dim(map_4)

mapa_r_crop_1 <- crop(map_1, map_lim)
mapa_r_crop_2 <- crop(map_2, map_lim)
mapa_r_crop_3 <- crop(map_3, map_lim)
mapa_r_crop_4 <- crop(map_4, map_lim)

ext(mapa_r_crop_1)
ext(mapa_r_crop_2)
ext(mapa_r_crop_3)
ext(mapa_r_crop_4)

plot(mapa_r_crop_1)
plot(mapa_r_crop_2)
plot(mapa_r_crop_3)
plot(mapa_r_crop_4)


nombre_1 <- paste0(ecos_amlat_all$ID[66], "-", ecos_amlat_all$Group[66], ".tif")
nombre_2 <- paste0(ecos_amlat_all$ID[67], "-", ecos_amlat_all$Group[67], ".tif")
nombre_3 <- paste0(ecos_amlat_all$ID[68], "-", ecos_amlat_all$Group[68], ".tif")
nombre_4 <- paste0(ecos_amlat_all$ID[69], "-", ecos_amlat_all$Group[69], ".tif")

nombre_1 <- str_replace(nombre_1, "/", "-")
nombre_2 <- str_replace(nombre_2, "/", "-")
nombre_3 <- str_replace(nombre_3, "/", "-")
nombre_4 <- str_replace(nombre_4, "/", "-")

writeRaster(mapa_r_crop_1, 
            paste0(mapa_latam_lst, "/lat-amer/rast/", nombre_1), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(mapa_r_crop_2, 
            paste0(mapa_latam_lst, "/lat-amer/rast/", nombre_2), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(mapa_r_crop_3, 
            paste0(mapa_latam_lst, "/lat-amer/rast/", nombre_3), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(mapa_r_crop_4, 
            paste0(mapa_latam_lst, "/lat-amer/rast/", nombre_4), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")



mapas <-  c(mapa_r_crop_1,mapa_r_crop_2,mapa_r_crop_3,mapa_r_crop_4)


plot(mapas)
mapas_df <- as.data.frame(mapas)

