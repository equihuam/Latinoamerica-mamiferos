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
mapa_rc_ref <- crop(mapa_r_ref, map_lim)
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
    if(ext(mapa_r_crop) != ext(mapa_rc_ref))
      mapa_r_crop <- resample(mapa_r_crop, mapa_rc_ref, method = "near")
    
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
    print(nombre_r )
  }
}


#Mapas de grandes grupos
mapas_lst <- list.files(paste0(mapa_latam_lst, "lat-amer/rast/"), 
                        pattern = ".*tif$",
                        full.names = TRUE)
mapas <- tibble(ruta = mapas_lst) |> 
  mutate(nombre = str_remove(ruta, ".*rast/"),
         biome = str_extract(nombre, "^\\w+\\d"),
         funGrp = as.integer(str_extract(nombre, "(^\\w+\\d)\\.(\\d)", 2))) 

mapas_grupo <- function(eco_id = "T1", mapas_lst)
{
  eco_id_re <- paste0("/rast/", eco_id, "\\.")
  grupo_mapas_lst <- mapas_lst[str_detect(mapas_lst, eco_id_re)]
  print(grupo_mapas_lst)
  for (mapa in grupo_mapas_lst)
  {
    mapa_tx <- str_extract_all(mapa, "(?<=rast/)(.*)")
    if (mapa == grupo_mapas_lst[[1]])
    { 
      mapa_r <- rast(mapa)
      mapas <-mapa_r
      print(paste0("--1> ", mapa_tx))
    } else {
      mapa_r <- rast(mapa)
      mapas <- c(mapas, mapa_r)
      print(paste0("--2> ", mapa_tx))
    }
  }
  return(mapas)
}

mapas_grupo_T1 <- mapas_grupo("T1", mapas_lst) 
mapas_grupo_T2 <- mapas_grupo("T2", mapas_lst)
mapas_grupo_T3 <- mapas_grupo("T3", mapas_lst)
mapas_grupo_T4 <- mapas_grupo("T4", mapas_lst)
mapas_grupo_T5 <- mapas_grupo("T5", mapas_lst)
mapas_grupo_T6 <- mapas_grupo("T6", mapas_lst)
mapas_grupo_T7 <- mapas_grupo("T7", mapas_lst)

mapas_grupo_F1 <- mapas_grupo("F1", mapas_lst)
mapas_grupo_F2 <- mapas_grupo("F2", mapas_lst)
mapas_grupo_F3 <- mapas_grupo("F3", mapas_lst)

mapas_grupo_M1 <- mapas_grupo("M1", mapas_lst)
mapas_grupo_M2 <- mapas_grupo("M2", mapas_lst)
mapas_grupo_M3 <- mapas_grupo("M3", mapas_lst)
mapas_grupo_M4 <- mapas_grupo("M4", mapas_lst)


# Operación directamente sobre el raster
baseLatAm <- rast(ext(mapas_grupo_T1[[1]]), res=res(mapas_grupo_T1[[1]]))
baseLatAm <- init(baseLatAm,  NA)
names(baseLatAm) <-"biome"

biomas_fnc <- function(base, grupo, bioma_id, grFunc)
{
  bioDat <- filter(grFunc, str_detect(biome, paste0("^", bioma_id)))
  
  for(i in (1:nlyr(grupo)))
  {
    base[(base == 0) | (is.na(base))] <- 
      as.int(!is.na(grupo[[i]])) * bioDat$funGrp[i] 
  }
  base[base == 0] <- NA  
  return(base)
}  

biome_F1 <- biomas_fnc(baseLatAm, mapas_grupo_F1, "F1", mapas)  
biome_F2 <- biomas_fnc(baseLatAm, mapas_grupo_F2, "F2", mapas)  
biome_F3 <- biomas_fnc(baseLatAm, mapas_grupo_F3, "F3", mapas)   

biome_M1 <- biomas_fnc(baseLatAm, mapas_grupo_M1, "M1", mapas)  
biome_M2 <- biomas_fnc(baseLatAm, mapas_grupo_M2, "M2", mapas)  
biome_M3 <- biomas_fnc(baseLatAm, mapas_grupo_M3, "M3", mapas)  
biome_M4 <- biomas_fnc(baseLatAm, mapas_grupo_M4, "M4", mapas)  

biome_T1 <- biomas_fnc(baseLatAm, mapas_grupo_T1, "T1", mapas)  
biome_T2 <- biomas_fnc(baseLatAm, mapas_grupo_T2, "T2", mapas)  
biome_T3 <- biomas_fnc(baseLatAm, mapas_grupo_T3, "T3", mapas)  
biome_T4 <- biomas_fnc(baseLatAm, mapas_grupo_T4, "T4", mapas)  
biome_T5 <- biomas_fnc(baseLatAm, mapas_grupo_T5, "T5", mapas)  
biome_T6 <- biomas_fnc(baseLatAm, mapas_grupo_T6, "T6", mapas)  
biome_T7 <- biomas_fnc(baseLatAm, mapas_grupo_T7, "T7", mapas)

#---------------------------------------------------------------- 

writeRaster(biome_F1, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_F1"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(biome_F2, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_F2"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(biome_F3, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_F3"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(biome_M1, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_M1"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(biome_M2, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_M2"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(biome_M3, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_M3"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(biome_M4, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_M4"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(biome_T1, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_T1"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(biome_T2, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_T2"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(biome_T3, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_T3"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(biome_T4, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_T4"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(biome_T5, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_T5"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(biome_T6, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_T6"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")
writeRaster(biome_T7, 
            paste0(mapa_latam_lst, "/lat-amer/rast-grupo/", "biome_T7"), 
            filetype = "GTiff", overwrite = TRUE, datatype = "INT1U")

