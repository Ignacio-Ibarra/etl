# require(data.table)

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)
tipo_encuesta <- "individual"
version <- paste0("v",format(Sys.Date(), "%Y%m%d"))
ext <- "csv"
output_folder = "data/_FUENTES/raw/"

# Descargar años anteriores
anio_start <- 2016
anio_end <- 2023

anio <- 2016
fuente_id = 63
for (anio in anio_start:anio_end){
  download_filename <- glue::glue("eph_{tipo_encuesta}_{anio}_{version}.{ext}")
  periods <- 1:4
  
  if (anio == 2003){
    periods <- 3:4
    
    
  }
  
  if (anio == 2015){
    periods <- 1:2
    
  }
  
  if (anio == 2016){
    periods <- 2:4
  }
    
  df <- eph::get_microdata(year= anio,
                             period = periods,
                             type = "individual")  
  
  # DT <- data.table(df)
  # 
  # # Cita solo los valores que no son numéricos
  # for (col in names(DT)) {
  #   if (!is.numeric(DT[[col]])) {
  #     DT[[col]] <- sprintf('"%s"', DT[[col]])
  #   }
  # }
  # 
  # # Luego, escribe el data.table en un archivo CSV utilizando fwrite
  # fwrite(DT, file = "archivo.csv", sep = ",")
  # 
  df  %>% write_csv_fundar(., glue::glue("{output_folder}{download_filename}"))
  
  
  # agregar_fuente_raw(url = "https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos",
  #                    nombre = glue::glue("Encuesta Permanente de Hogares, {str_to_title(tipo_encuesta)} ({anio})"),
  #                    institucion = "INDEC",
  #                    actualizable = T,
  #                    dir = output_folder,
  #                    path_raw = download_filename,
  #                    script = code_name,
  #                    api = T
  # )
  
  
  actualizar_fuente_raw(id_fuente = fuente_id, actualizable = T, dir = "data/_FUENTES/raw/")
  
  log <- sprintf("Descargado: id %s año %s archivo %s. \n\n", fuente_id, anio, download_filename)
  cat(log)
  
  fuente_id <- fuente_id + 1
  }


# Descargar última data disponible
currente_year <- format(Sys.Date, "%Y")













actualizar_fuente_raw(id_fuente = 49, actualizable = T, dir = "data/_FUENTES/raw/")