require(eph)

funcion_por_anio <- function(anio,
                             fuentes_raw_df,
                             tipo_encuesta, 
                             version, 
                             ext,
                             code_name, 
                             fecha_actualizar,
                             output_folder = tempdir()
){
  
  fuente_id = fuentes_raw_df %>%
    filter(nombre == sprintf("Encuesta Permanente de Hogares, %s (%s)", str_to_title(tipo_encuesta), anio))  %>%
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
                           type = tipo_encuesta)
  
  df  %>% write_csv_fundar(., glue::glue("{output_folder}/{download_filename}"))
  
  accion_str <- ""
  if (length(fuente_id) == 0){
    
    agregar_fuente_raw(url = glue::glue("https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos (ver año {anio})"),
                       nombre = sprintf("Encuesta Permanente de Hogares, %s (%s)", str_to_title(tipo_encuesta), anio),
                       institucion = "Instituto Nacional de Estadísticas y Censos",
                       actualizable = T,
                       path_raw = download_filename,
                       fecha_actualizar = fecha_actualizar,
                       script = code_name,
                       api = F
                       )
    accion_str <- "Nueva fuente"
  }else{
    
    actualizar_fuente_raw(id_fuente = id_fuente,
                          fecha_actualizar = fecha_actualizar,
                          path_raw = download_filename,
                          script = code_name)
    
    accion_str <- "Actualiza fuente"
  }
  
  return(sprintf("%s: id %s año %s archivo %s", accion_str, fuente_id, anio, download_filename))
  
}