
# Aclaracion --------------------------------------------------------------


#' Este script requiere tener clonado el repo de transformers
#' el path de trasnformers deberia ser "~/transformers"
#' Ademas es necesario definir contra cual entrega del drive comparar
#' Tambien requiere tener clonado el repo data de donde se levantan los datasets
#' que seran comparados contra el drive

# flujo -------------------------------------------------------------------

subtopico <-  "INFDES"
entrega <- "primera_entrega"
directorios <- list.dirs("~/transformers")
directorios <- directorios[!grepl("\\.", directorios)]
directorios <-  directorios[!grepl("/transformers$", directorios)]
mappings <- list()


for (i in 1:length(directorios)) {
  archivos <- list.files(directorios[i], full.names = T)
  map_path <- grep("mappings.json", archivos, value = T)
  subtop <- str_extract(directorios[i], "(?<=transformers/).{6}")
  maps_subtop <- jsonlite::fromJSON(map_path)
  maps_subtop <- bind_rows(maps_subtop, .id = "dataset")
  mappings[[subtop]] <- maps_subtop
  
}

mappings <- mappings %>% bind_rows(.id = "subtopico") 



salidas <- list.files(path = glue::glue("~/data/{subtopico}"), full.names = T)

resumen_control_subtopico <- list()

for (i in subtopico_outputs(subtopico_nombre = subtopico,
                            entrega_subtopico = entrega)$name) {
  
  json_name <- gsub("\\.csv$", ".json", i)
  
  json_path <- grep(json_name, salidas, value = T)
  
  controles <- jsonlite::fromJSON(json_path)
  
  
  x <- descargar_output(i, subtopico = subtopico, entrega_subtopico = entrega) %>% 
    suppressMessages()
  y <- read_csv(grep(i, salidas, value = T)) %>% suppressMessages()
  
  if (controles$es_serie_tiempo) {
    
    
    # print(glue::glue("La fecha maxima nueva es mayor? {max(y[,controles$columna_indice_tiempo]) >  max(x[,controles$columna_indice_tiempo])}"))
    hay_nueva_fecha_max <- as.character(max(y[,controles$columna_indice_tiempo], na.rm = T) >  max(x[,controles$columna_indice_tiempo], na.rm = T))
    # print(glue::glue("Fecha maxima nueva: {max(y[,controles$columna_indice_tiempo])}"))
    fecha_max_nueva <- as.character(max(y[,controles$columna_indice_tiempo], na.rm = T))
    # print(glue::glue("Fecha maxima vieja: {max(x[,controles$columna_indice_tiempo])}"))
    fecha_max_anterior <- as.character(max(x[,controles$columna_indice_tiempo], na.rm = T))
    
  } else {
    hay_nueva_fecha_max <- "-"
    # print(glue::glue("Fecha maxima nueva: {max(y[,controles$columna_indice_tiempo])}"))
    fecha_max_nueva <- "-"
    # print(glue::glue("Fecha maxima vieja: {max(x[,controles$columna_indice_tiempo])}"))
    fecha_max_anterior <- "-"
  }
  
  
  metricas_cols <- list()
  
  if (is.character(controles$control)) {
    metricas_cols <- controles$control
    diferencia_filas <- "-"
    aclaracion = "-"
    nuevos_na_max = "-"
    test_min = "-"
    
  } else {
    
    for (j in names(controles$control$comparacion_cols)) {
      
      metricas_cols[[j]] <- controles$control$comparacion_cols[[j]][sapply(controles$control$comparacion_cols[[j]],
                                                                           function(x) !is.list(x))] 
      
      
      
    }
    
    metricas_cols <- bind_rows(metricas_cols, .id = "variable")
    metricas_cols <- metricas_cols %>%
      pivot_longer(cols = -variable, names_to = "metrica", values_to = "valor") 
    nuevos_na_max <- max(metricas_cols$valor[metricas_cols$metrica == "nuevos_na"], na.rm = T)
    test_min <- min(metricas_cols$valor[grepl("test", metricas_cols$metrica)], na.rm = T)
    
    aclaraciones <- controles$aclaraciones %>% as.character()
    diferencia_filas <- controles$control$diferencia_nfilas
    
  }
  
  resumen_control_subtopico[[i]] <- list(
    # dataset = i,
    diferencia_filas = as.character(diferencia_filas),
    hay_nueva_fecha_max = as.character(hay_nueva_fecha_max),
    fecha_max_nueva = as.character(fecha_max_nueva),
    fecha_max_anterior = fecha_max_anterior,
    aclaracion = as.character(aclaraciones),
    nuevos_na_max = as.character(nuevos_na_max),
    test_min = as.character(test_min)
  )
  
  
}

# resumen_control_subtopico <- resumen_control_subtopico %>%
resumen_control_subtopico <- lapply(resumen_control_subtopico, function(x) unlist(x))
resumen_control_subtopico <- resumen_control_subtopico %>% bind_rows(.id = "dataset")

# hay que joinearle mappings a esto
resumen_con_ids <- mappings %>% 
  left_join(resumen_control_subtopico, by = c("dataset" = "dataset"))

resumen_con_ids %>% 
  filter(!is.na(diferencia_filas)) %>% 
  writexl::write_xlsx("control_cambios_datasets.xlsx")
