
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
      x[i] <- x[i-1]*var[i]
    } 
  }
  x
}

expansor_imf_maddison <- expansor_xvar

# levanta diccionario de codigos iso y nombres de pais
get_iso_paises <- function() read.csv("https://docs.google.com/spreadsheets/d/1kK1Yu6gz5kEWe_i0vamiGttkXUH5H90e/export?format=csv&id=1kK1Yu6gz5kEWe_i0vamiGttkXUH5H90e&gid=808722347") %>% 
  select(iso3, pais = iso3_desc_fundar)

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
  
  if (is.null(tabla)) {
    stop("Tabla no puede ser nulo")
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
    
    x
  } else {
    stop("Tabla no contemplada")
  }
  
  
}

