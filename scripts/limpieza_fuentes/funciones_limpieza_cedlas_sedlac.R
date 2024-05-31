

strip_chars <- function(x) {
  if (is.numeric(x)) {
    return(x)
  } else {
    return(str_replace_all(str_trim(x), "\\.", ""))
  }
}

quitar_coma_adelante <- function(s) {
  if (substr(s, 1, 1) == ",") {
    s <- substr(s, 2, nchar(s))
  }
  return(trimws(s))
}


es_entero <- function(x) {
  
  suppressWarnings({
    num <- as.numeric(x)
  })
  
  return(!is.na(num) && grepl("^\\d+$", x))
}


cols_and_data_row_locations <- function(input_sheet) {
  
  # Saco las filas que contienen todas las columnas nulas y las columnas que contienen todas las filas nulas.
  input_sheet <- input_sheet %>%
    select(where(~ any(!is.na(.)))) %>%
    filter(rowSums(is.na(.)) != ncol(.)) %>%
    as.data.frame()
  
  col0 <- input_sheet[, 1]
  
  # Cuando la cantidad de nulos llega a 2 paro y sumo 1 para encontrar el índice
  upper_loc <- which(cumsum(is.na(col0)) == 1)[1]
  
  # Me fijo donde arranca el primer entero en la col0 y tomo el índice de dos filas antes
  start_data <- NA
  for (i in seq_along(col0)) {
    if (!is.na(col0[i]) && grepl("^\\d+", as.character(col0[i]))) {
      start_data <- i
      break
    }
  }
  bottom_loc <- start_data - 3
  
  return(list(upper_loc = upper_loc, bottom_loc = bottom_loc, start_data = start_data))
}

obtengo_tabla_numeros <- function(df){
  df <- df %>%
    select(where(~ !all(is.na(.)))) %>%
    filter(rowSums(is.na(.)) != ncol(.)) %>%
    as.data.frame()
  
  locs <- cols_and_data_row_locations(df)
  cols_upper <- locs$upper_loc
  cols_bottom <- locs$bottom_loc
  start_data <- locs$start_data
  cat("Data was founded in row", start_data, "\n")
  
  data <- df[start_data:nrow(df), 2:ncol(df)] %>%
    filter(rowSums(is.na(.)) != ncol(.))
  
  return( data )
} 

obtengo_columnas <- function(df, multidx = FALSE) {
  df <- df %>%
    select(where(~ !all(is.na(.)))) %>%
    filter(rowSums(is.na(.)) != ncol(.)) %>%
    as.data.frame()
  
  locs <- cols_and_data_row_locations(df)
  cols_upper <- locs$upper_loc
  cols_bottom <- locs$bottom_loc
  
  locations <- cols_upper:cols_bottom
  
  levels <- lapply(locations, function(l) {
    level <- df[l, 2:ncol(df)] %>% 
      unlist() %>% 
      as_tibble() %>% 
      tidyr::fill(value) %>% 
      pull()
    
    return(level)
  })
  
  columnas <- do.call(rbind, levels)
  columnas <- apply(columnas, 2, function(x) paste(x[!is.na(x)], collapse = ", "))
  
  return(columnas)
}