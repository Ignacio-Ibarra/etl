#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
limpiar_temps() #Borro para que arranque en cero el directorio temporal del server

funcion_por_anio <- function(anio,
                             fuentes_raw_df,
                             tipo_encuesta, 
                             version, 
                             ext, 
                             output_folder
){
  
  fuente_id = fuentes_raw_df %>%
    filter(nombre == sprintf("Encuesta Permanente de Hogares, Individual (%s)", anio))  %>%
    select(id_fuente) %>%
    pull()
  
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
  
  df  %>% write_csv_fundar(., glue::glue("{output_folder}{download_filename}"))
  
  accion_str <- ""
  if (length(fuente_id) == 0){
    
    agregar_fuente_raw(url = "https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos",
                       nombre = glue::glue("Encuesta Permanente de Hogares, {str_to_title(tipo_encuesta)} ({anio})"),
                       institucion = "INDEC",
                       actualizable = T,
                       dir = output_folder,
                       path_raw = download_filename,
                       script = code_name,
                       api = T
    )
    accion_str <- "Nueva fuente"
  }else{
    
    actualizar_fuente_raw(id_fuente = fuente_id, actualizable = T, dir = output_folder)
    accion_str <- "Actualiza fuente"
  }
  
  return(sprintf("%s: id %s año %s archivo %s", accion_str, fuente_id, anio, download_filename))
  
}



# Me quedo con el nombre del script
code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# Parametros para guardar archivo
tipo_encuesta <- "individual"
version <- glue::glue("v{format(Sys.Date(), '%Y%m%d')}")
ext <- "csv"
output_folder <- glue::glue("{tempdir()}/_FUENTES/raw/")

# Cuanto comenzó la EPH
eph_init_year <- 2003

# Año actual
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Año anterior al actual
last_year <-  current_year - 1

# Años transcurridos
anios_efectivos <- eph_init_year:last_year

# Fuentes_raw
fuentes_raw_df <- fuentes_raw()

# Años descargados
anios_descargados <- fuentes_raw_df %>% 
  filter(grepl("Encuesta Permanente de Hogares, Individual*",nombre)) %>% 
  select(nombre) %>% 
  pull() %>% 
  str_extract(., "\\d{4}") %>% 
  as.numeric(.)

# Años anteriores no descargados
anios_anteriores_a_agregar <- setdiff(anios_efectivos, anios_descargados)

stopifnot(length(anios_anteriores_a_agregar) != 0)


# Si no tengo algun anio anterior lo descarga
if (length(anios_anteriores_a_agregar)>0){
  
  for (anio in anios_anteriores_a_descargar){
    
    funcion_por_anio(anio = anio, 
                     fuentes_raw_df = fuentes_raw_df, 
                     tipo_encuesta = tipo_encuesta,
                     version = version, 
                     ext = ext,
                     output_folder = output_folder)
    
  }
  
}


# Setear en TRUE si se quiere actualizar
ACTUALIZAR_ANIOS_ANTERIORES <- FALSE 


# Agregar años anteriores si no están descargados o los actualiza
if (ACTUALIZAR_ANIOS_ANTERIORES){
  
  # itero sobre los descargados porque los 
  # que no estaban descargados fueron descargados
  # en las lineas de arriba
  for (anio in anios_descargados){
    
    funcion_por_anio(anio = anio, 
                     fuentes_raw_df = fuentes_raw_df, 
                     tipo_encuesta = tipo_encuesta,
                     version = version, 
                     ext = ext,
                     output_folder = output_folder)
    
  }
}


# Descarga de año actual
fecha_prim_trim <- as.Date(sprintf("%s-08-05", current_year))
current_date <- as.Date(Sys.Date())
if ( current_date > fecha_prim_trim ){
  
  funcion_agregar_anio(current_year,
                       fuentes_raw_df,
                       tipo_encuesta, 
                       version, 
                       ext, 
                       output_folder)
  
}

cat("Fin de script!")



