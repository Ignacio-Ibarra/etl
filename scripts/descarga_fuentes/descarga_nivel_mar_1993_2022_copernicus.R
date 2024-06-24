

url_evol_nivel_mar_1993_2022="https://s3.waw3-1.cloudferro.com/mdl-native-14/native/OMI_CLIMATE_SL_GLOBAL_area_averaged_anomalies/omi_climate_sl_global_area_averaged_anomalies_202311/omi_climate_sl_global_area_averaged_anomalies_19930101_P20230403.nc?x-cop-client=MyOcean&x-cop-usage=FileBrowser"

download.file(url = url_evol_nivel_mar_1993_2022, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/evol_nivel_mar_1993_2022.nc"))

# agrego la fuente
agregar_fuente_raw(url = "https://s3.waw3-1.cloudferro.com/mdl-native-14/native/OMI_CLIMATE_SL_GLOBAL_area_averaged_anomalies/omi_climate_sl_global_area_averaged_anomalies_202311/omi_climate_sl_global_area_averaged_anomalies_19930101_P20230403.nc?x-cop-client=MyOcean&x-cop-usage=FileBrowser", 
                   institucion = "Copernicus Climate Change Service", 
                   actualizable = T,
                   fecha_descarga = Sys.Date(),
                   path_raw = "evol_nivel_mar_1993_2022.nc", 
                   dir = tempdir(),
                   script = "descarga_nivel_mar_1993_2022_copernicus.R",
                   nombre = "Nivel del mar 1992-2022"
)

actualizar_fuente_raw(id_fuente=162 ,url = "https://s3.waw3-1.cloudferro.com/mdl-native-14/native/OMI_CLIMATE_SL_GLOBAL_area_averaged_anomalies/omi_climate_sl_global_area_averaged_anomalies_202311/omi_climate_sl_global_area_averaged_anomalies_19930101_P20230403.nc?x-cop-client=MyOcean&x-cop-usage=FileBrowser",
                      fecha_descarga = Sys.Date())


