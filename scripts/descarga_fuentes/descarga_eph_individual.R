# require(data.table)

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)
tipo_encuesta <- "individual"
# version <- paste0("v",format(Sys.Date(), "%Y%m%d"))
version <- "v20240506"
ext <- "csv"
output_folder = "data/_FUENTES/raw/"

# Descargar última data disponible
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Descargar años anteriores
# anio_start <- 2003
# anio_end <- current_year - 1
periods <- 1:4
# #Asume que todas las fuentes de EPH se cargaron juntas. 
# # Toma el id de la EPH 2003 e itera a partir de ahi. 
# fuente_id = fuentes_raw() %>% 
#   filter(nombre == "Encuesta Permanente de Hogares, Individual (2003)")  %>% 
#   select(id_fuente) %>% 
#   pull()
# 
# for (anio in anio_start:anio_end){
#   download_filename <- glue::glue("eph_{tipo_encuesta}_{anio}_{version}.{ext}")
#   
#   if (anio == 2003){
#     periods <- 3:4
#     
#     
#   }
#   
#   if (anio == 2015){
#     periods <- 1:2
#     
#   }
#   
#   if (anio == 2016){
#     periods <- 2:4
#   }
#     
#   df <- eph::get_microdata(year= anio,
#                              period = periods,
#                              type = "individual")  
#   
#   df  %>% write_csv_fundar(., glue::glue("{output_folder}{download_filename}"))
#   
#   
#   # agregar_fuente_raw(url = "https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos",
#   #                    nombre = glue::glue("Encuesta Permanente de Hogares, {str_to_title(tipo_encuesta)} ({anio})"),
#   #                    institucion = "INDEC",
#   #                    actualizable = T,
#   #                    dir = output_folder,
#   #                    path_raw = download_filename,
#   #                    script = code_name,
#   #                    api = T
#   # )
#   
#   
#   actualizar_fuente_raw(id_fuente = fuente_id, actualizable = T, dir = "data/_FUENTES/raw")
#   
#   log <- sprintf("Actualizado: id %s año %s archivo %s. \n\n", fuente_id, anio, download_filename)
#   cat(log)
#   
#   fuente_id <- fuente_id + 1
#   }


# Descargar current_year
download_filename <- glue::glue("eph_{tipo_encuesta}_{current_year}_{version}.{ext}")

df <- eph::get_microdata(year= current_year,
                         period = periods,
                         type = "individual")

df  %>% write_csv_fundar(., glue::glue("{output_folder}{download_filename}"))

agregar_fuente_raw(url = "https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos",
                  nombre = glue::glue("Encuesta Permanente de Hogares, {str_to_title(tipo_encuesta)} ({current_year})"),
                  institucion = "INDEC",
                  actualizable = T,
                  dir = output_folder,
                  path_raw = download_filename,
                  script = code_name,
                  api = T)
