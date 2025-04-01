encontrar_splits_grandes <- function(column_splits){
  column_splits[is.na(column_splits)] <- "null_value"
  ids <- 1:length(column_splits)
  total_general <- ids[column_splits == "Total general"]
  totales <- ids[column_splits == "Total"] - 1
  return(c(total_general, totales))
}

obtener_subset <- function(data, start_idx, end_idx) {
  # Obtener el subset
  subset_data <- data[start_idx:end_idx, ]
  
  return(subset_data)
}



datos_entre_filas_nulas <- function(data) {
  # Encontrar índices de las filas completamente nulas
  filas_nulas <- which(apply(data, 1, function(row) all(is.na(row))))
  
  # Si hay al menos dos filas completamente nulas, obtener los datos entre ellas
  if (length(filas_nulas) >= 2) {
    # Guardar los índices de la primera y la última fila nula
    primera_fila_nula <- filas_nulas[1]
    ultima_fila_nula <- filas_nulas[length(filas_nulas)]
    
    # Quedarse solo con los datos entre estas dos filas nulas
    data <- data[(primera_fila_nula + 1):(ultima_fila_nula - 1), ]
  }
  
  return(data)
}

eliminar_filas_footer <- function(data) {
  # Filtrar filas donde el número de valores no nulos sea mayor que 1
  data <- data[rowSums(!is.na(data)) > 1, ]
  return(data)
}

clean_cgi <- function(all_data, sheet_name){
  
  sheet_normalized <- sheet_name %>% janitor::make_clean_names(.)
  
  indicator_name <- all_data[[1]] %>% str_remove(., "\\ Años.*")
  
  anios_str <- all_data%>% 
    slice(3) %>%       
    select(3:ncol(.)) %>% 
    unlist() %>% 
    unname() %>% 
    str_remove(., " \\(\\d\\)") 
  
  splits_grandes <- encontrar_splits_grandes(all_data$...2)
  
  cortes_splits <- all_data$...2[splits_grandes]
  
  
  data_final <- data.frame() 
  
  for (i in seq_along(splits_grandes)) {
    
    edad_sexo <- cortes_splits[i]
    
    cat(edad_sexo, "\n")
    
    start_idx <- splits_grandes[i]
    
    if (i < length(splits_grandes)) {
      end_idx <- splits_grandes[i + 1] - 1
    } else {
      end_idx <- nrow(all_data)
    }
    
    # Obtener el subset y quedarse con los datos entre las filas nulas
    subset <- obtener_subset(all_data, start_idx, end_idx)
    subset <- datos_entre_filas_nulas(subset)
    names(subset) <- c("letra", "letra_desc", anios_str)
    
    subset <- eliminar_filas_footer(subset)
    
    subset$edad_sexo <- edad_sexo
    
    subset <- subset %>%
      mutate(across(all_of(anios_str), as.numeric)) %>% 
      pivot_longer(all_of(anios_str),
                   names_to = "anio",
                   names_transform = as.integer,
                   values_to = sheet_normalized)

    # Apilar el subset en data_final
    data_final <- bind_rows(data_final, subset)
    
  }
  
  data_final <- data_final %>% dplyr::filter(!is.na(letra))
  return(data_final)
}
