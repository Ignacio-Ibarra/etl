
write_argendata <- function(data, file_name, subtopico) {
  write.csv(x = data, file = glue::glue("data/{subtopico}/datasets/outputs/{file_name}"),
            eol = "\n", dec = ".", na = "",row.names = F, fileEncoding = "UTF-8")
}

# para empalmar una serie usando variaciones de otra serie
# dada una columna x con los valores de la serie a expandir: Xi a Xn de 1 a N 
# y dada una columna var donde VARi = Xi/Xi-1 como las proporciones entre un valor de x y su antecedente
# si Xi es NA lo calcula y reemplaza como Xi-1*VARi, si no es NA lo deja tal cual

expansor_xvar <- function(x,var) {
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      new_value <- x[i-1]*var[i]
      
      if (length(new_value) == 0) {
        new_value <- NA
      }
      
      x[i] <- new_value
    } 
  }
  
  if (any(is.na(x))) {
    warning("La expansion del vector dejo valores faltantes") 
  }
  
  x
}

expansor_imf_maddison <- expansor_xvar

# levanta diccionario de codigos iso y nombres de pais
get_iso_paises <- function() {
  read.csv("https://docs.google.com/spreadsheets/d/1kK1Yu6gz5kEWe_i0vamiGttkXUH5H90e/export?format=csv&id=1kK1Yu6gz5kEWe_i0vamiGttkXUH5H90e&gid=808722347") %>% 
  select(iso3, pais = iso3_desc_fundar)
}

# Cargar bibliotecas necesarias
library(dplyr)

# Función para comparar pares de columnas homónimas
comparar_cols <- function(dataframe) {
  # Identificar pares de columnas homónimas
  nombres <- names(dataframe)
  pares <- nombres[grep("\\.x$", nombres)]
  pares <- sub("\\.x$", "", pares)
  
  # Inicializar un dataframe para almacenar los resultados
  resultados <- data.frame(anio = dataframe$anio)
  
  # Recorrer cada par y calcular la diferencia
  for (par in pares) {
    col_x <- paste(par, ".x", sep = "")
    col_y <- paste(par, ".y", sep = "")
    
    # Verificar si ambas columnas existen en el dataframe
    if (col_x %in% nombres && col_y %in% nombres) {
      # Calcular la diferencia y añadir al dataframe de resultados
      resultados[[par]] <- dataframe[[col_x]] - dataframe[[col_y]]
    }
  }
  
  return(resultados)
}


tidy_indec <- function(x, tabla = NULL) {
  
  tablas_posibles <- c("sh_oferta_demanda", "serie_cgi")
  
  if (is.null(tabla)) {
    stop(glue::glue("Tabla no puede ser nulo. Tablas posibles:\n{paste0(tablas_posibles, collapse = '\n')}"))
  }
  
  if (tabla == "sh_oferta_demanda") {
    x <- x %>% 
      .[-c(1:3),] %>% 
      t() %>% 
      as_tibble(.name_repair = "unique")
    
    names(x) <- x[1,] %>%
      janitor::make_clean_names() 
    
    x <- x %>% 
      rename(anio = na, trim = na_2)
    
    x <- x %>% 
      mutate(anio = as.numeric(gsub(" .*", "", anio )))
    
    x <- x %>% 
      fill(anio)
    
    x <- x %>% 
      filter(!is.na(trim))
    
    x
    
  } else if (tabla == "serie_cgi") {
    x <- x %>% 
      .[-c(1,4:5),] %>% 
      t() %>% 
      as_tibble(.name_repair = "unique")
    
    names(x) <- x[2,] %>%
      janitor::make_clean_names() 
    
    x <- x[-c(1:2),]
    
    x <- x %>% 
      rename(anio = na, trim = na_2)
    
    x <- x %>% 
      mutate(anio = as.numeric(gsub(" .*", "", anio )))
    
    x <- x %>% 
      fill(anio)
    
    x <- x %>% 
      filter(!is.na(trim))
    x
    
  } else {
    stop(glue::glue("Tabla no contemplada. Tablas posibles:\n{paste0(tablas_posibles, collapse = '\n')}"))
  }
  
  
}

subtopico_init <- function(subtopico_nombre, entrega_subtopico) {
  
  df <- drive_ls(path = as_id("https://drive.google.com/drive/folders/16Out5kOds2kfsbudRvSoHGHsDfCml1p0"))
  
  id_subtopicos <- df[df$name == "SUBTOPICOS",]$id
  
  subtopicos <- drive_ls(id_subtopicos)
  
  
  # eleccion del subtopico
  # subtopico <- subtopicos$name[subtopicos$name == x]
  
  # levanta los outputs del subtopico
  
  outputs <- drive_ls(subtopicos$id[subtopicos$name == subtopico_nombre]) %>% 
    filter(name == "datasets") %>% 
    pull(id) %>% 
    drive_ls(.) %>% 
    filter(name == "outputs") %>% 
    pull(id) %>% 
    drive_ls(.) %>% 
    # seleccionar la entrega
    filter(str_detect(name, entrega_subtopico)) %>% 
    pull(id) %>% 
    drive_ls(.)
  
  # levanta lista de scripts del subtopico
  # scripts_subtopico <- list.files(glue::glue("scripts/subtopicos/{subtopico}/"),
  # full.names = T)
  
  
  # le asigna nombres a los elementos de la lista
  # names(scripts_subtopico) <-  list.files(glue::glue("scripts/subtopicos/{subtopico}/")) %>% 
  # str_remove_all(pattern = "\\d_")
  
  # scripts_subtopico <- as.list(scripts_subtopico)
  
  # correr script de la lista para el subtopico
  # source(scripts_subtopico$pib_pibpc_pob_arg_esp.R)
  
  rm(list = c("df", "subtopicos", "id_subtopicos"))
  
  outputs
}


tidy_weo <- function(x) {
  
  
  x <- x %>% 
    # limpio nombres de columnas: pasar a minusculas, remove non-ascii chars y cambia " " por "_"
    janitor::clean_names()
  
  
  # proceso weo_imf
  
  x <- x %>% 
    # selecciono vars de interes
    select(-c(weo_country_code,country, subject_descriptor, subject_notes, units,
              scale, country_series_specific_notes, estimates_start_after))
  
  # limpieza de n/a y strings de los valores
  x <- x %>% 
    mutate(across(matches("\\d{4}"), \(x) parse_number(x, na  =c("n/a", "--"))))
  
  # le doy formato longer adecuado
  x <- x %>% 
    pivot_longer(cols = -c(iso, weo_subject_code), names_to = "anio")
  
  # una columna por indicador
  x <- x %>% 
    pivot_wider(names_from = weo_subject_code, values_from = value) 
  
  # limpio nombres de columnas (nombre de indicadores)
  x <- janitor::clean_names(x )
  
  x <- x %>% 
    mutate(anio = as.numeric(gsub("\\D","", anio)))
  
  x <- x %>% 
    mutate(anio = as.numeric(gsub("\\D","", anio)))
  
  x
  
}
