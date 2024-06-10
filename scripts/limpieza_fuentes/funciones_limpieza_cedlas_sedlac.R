library(httr)
library(rvest)

# Función para decodificar entidades HTML
unescape_html <- function(text) {
  text <- gsub("&quot;", "\"", text)
  text <- gsub("&amp;", "&", text)
  text <- gsub("&lt;", "<", text)
  text <- gsub("&gt;", ">", text)
  text <- gsub("&#39;", "'", text)
  text <- gsub("&nbsp;", " ", text)
  return(text)
}

# Función para traducir texto usando Google Translate
translate <- function(to_translate, to_language="es", from_language="en") {
  base_link <- "http://translate.google.com/m?tl=%s&sl=%s&q=%s"
  to_translate <- URLencode(to_translate)
  link <- sprintf(base_link, to_language, from_language, to_translate)
  
  # Realizar la solicitud GET
  response <- GET(link, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"))
  
  # Leer el contenido de la respuesta
  content <- content(response, "text")
  
  # Extraer la traducción del HTML
  translation <- read_html(content) %>%
    html_nodes(".result-container") %>%
    html_text()
  
  # Decodificar entidades HTML
  translation <- unescape_html(translation)
  
  return(translation)
}


# función para sacar espacios en blanco
strip_chars <- function(x) {
  if (is.numeric(x)) {
    return(x)
  } else {
    return(str_replace_all(str_trim(x), "\\.", ""))
  }
}

# función para quitar la coma adelante
quitar_coma_adelante <- function(s) {
  if (substr(s, 1, 1) == ",") {
    s <- substr(s, 2, nchar(s))
  }
  return(trimws(s))
}


# Chequea si es entero
es_entero <- function(x) {
  
  suppressWarnings({
    num <- as.numeric(x)
  })
  
  return(!is.na(num) && grepl("^\\d+$", x))
}


# Devuelve el nro de fila donde empiezan los datos, el número de fila donde empiezan las columnas multi-índice
# el nro de fila donde terminan las columnas multi-índice
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


# obtiene el bloque de números 
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
  
  if (ncol(df) == 2){
    data <- as.data.frame(df[start_data:nrow(df), 2:ncol(df)]) %>%
      filter(rowSums(is.na(.)) != ncol(.))
  }else{
    data <- as.data.frame(df[start_data:nrow(df), 2:ncol(df)]) %>%
      filter(rowSums(is.na(.)) != ncol(.)) %>% 
      filter(rowSums(is.na(.)) != (ncol(.) - 1)) 
             
  }
  
  
  return( data )
} 

#Obtiene los nombres de las columnas y los devuelve aplanados. 
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


# obtiene lista de países, fuentes, orden_fuentes y fechas de encuestas. 
obtengo_paises_fuentes <- function(df, lista.paises) {
  df <- df %>%
    select(where(~ !all(is.na(.)))) %>%
    filter(rowSums(is.na(.)) != ncol(.)) %>%
    as.data.frame()
  
  locs <- cols_and_data_row_locations(df)
  start_data <- locs$start_data
  
  col0 <- df[[1]]
  strings <- lapply(col0[(start_data-2):length(col0)], function(x) strip_chars(x) ) %>% unlist()
  
  
  paises_fuentes_dates <- data.frame(
    pais = character(),
    fuente = character(),
    fuente_orden = integer(),
    encuesta_date = character(),
    stringsAsFactors = FALSE
  )
  
  
  for (v in strings) {
    # cat(v,"\n")
    if (!es_entero(v)) {
      # cat("1\n")
      if (v %in% lista_paises) {
        # cat("1.1\n")
        pais <- v
        fuente <- "sd"
        fuente_orden <- 0
      } 
      else {
        # cat("1.2\n")
        if (is.na(as.numeric(strsplit(v, "-")[[1]][1]))) {
          # cat("1.2.1\n")
          fuente <- v
          fuente_orden <- fuente_orden + 1
        } 
        else {
          # cat("1.2.2\n")
          encuesta_date <- v
          paises_fuentes_dates <- rbind(paises_fuentes_dates,
                                        data.frame(pais = pais, fuente = fuente, 
                                                   fuente_orden = fuente_orden, 
                                                   fecha_sedlac = encuesta_date, 
                                                   stringsAsFactors = FALSE))
        }
      }
    } 
    else {
      # cat("2\n")
      encuesta_date <- v
      paises_fuentes_dates <- rbind(paises_fuentes_dates,
                                    data.frame(pais = pais, fuente = fuente, 
                                               fuente_orden = fuente_orden, 
                                               fecha_sedlac = encuesta_date, 
                                               stringsAsFactors = FALSE))
    }
  }
  return(paises_fuentes_dates)
}  

obtener_isos_de_paises <- function(df, mapper){
  data <- df %>% mutate(iso3 = mapper[pais])
  return(data)
}


corregir_paises_de_isos <- function(data, mapper){
  return(data %>% mutate(pais = mapper[iso3]))
}

armar_serie_original<- function(df,
                        topico,
                        tematica,
                        variable,
                        lista.paises, 
                        mapper.paises_cedlas.a.isos, 
                        mapper.isos.a.paises){
  
  columnas_sheet <- obtengo_columnas(df = df)
  
  tabla_numeros <- obtengo_tabla_numeros(df = df)
  
  paises_fuentes_dates <- obtengo_paises_fuentes(df = df, lista.paises = lista.paises)
  
  paises_fuentes_dates <- obtener_isos_de_paises(paises_fuentes_dates, mapper = mapper.paises_cedlas.a.isos) %>%
    select(iso3, pais, fuente, fuente_orden, fecha_sedlac)
  
  paises_fuentes_dates <- corregir_paises_de_isos(data = paises_fuentes_dates, mapper = mapper.isos.a.paises)
  
  # armo data
  data <- data.frame(tabla_numeros)
  colnames(data) <- columnas_sheet
  data <- cbind(paises_fuentes_dates, data)
  
  # Pivoteo
  data <- data %>% 
    pivot_longer(!all_of(c("iso3","pais","fuente","fuente_orden","fecha_sedlac")), 
                 names_to = "apertura", values_to = "valor",
                 values_transform = list(valor = as.numeric))
  
  # Hago traducciones de Apertura
  traducc_df <-  data %>% 
    distinct(apertura) %>% 
    mutate(traduc  = map(apertura, translate)) 
  
  
  mapper_traduccion <- setNames(traducc_df$traduc, traducc_df$apertura)
  
  topico <- translate(topico)
  tematica <- translate(tematica)
  variable <- translate(variable)
  
  data <- data %>% mutate(apertura = mapper_traduccion[apertura],
                          topico = topico,
                          tematica = tematica,
                          variable = variable)
  
  data <- data %>% mutate(anio = substr(fecha_sedlac, start = 1, stop = 4))
  
  data <- data %>% select(topico, tematica, variable, 
                          iso3, pais, fuente, fuente_orden, 
                          fecha_sedlac, anio, apertura, valor)
  
  
  
  
  return (data)
  
}


armar_serie_anualizada <- function(df_original){
  
  data <- data.frame(df_original)
  
  data <- data %>% 
    group_by(topico, tematica, variable, 
             iso3, pais, fuente, fuente_orden, 
             anio, apertura) %>% 
    summarise(valor = mean(valor, na.rm=T)) %>% 
    ungroup() %>% 
    mutate(serie = "Serie original")
  
  
  return(data)
  
  
}



armar_serie_empalme <- function(df_anual, mapper = mapeo_iso3_pais){
  
  data <- data.frame(df_anual)
  
  cols <- colnames(data)
  
  empalme <- data %>%
    arrange(iso3, pais, fuente_orden, anio) %>%
    mutate(id_fuente = paste(fuente_orden, fuente, sep = " - "))
  
  # Indico en que filas tengo empalmes
  empalme <- empalme %>%
    group_by(iso3, apertura, anio) %>%
    mutate(is_empalme = as.integer(n() == 2)) %>%
    ungroup()
  
  # Filtrar filas donde is_empalme es 1, agrupar y calcular max de fuente_orden
  max_fuente_orden <- empalme %>%
    dplyr::filter(is_empalme == 1) %>%
    group_by(iso3, apertura) %>%
    summarise(fuente_orden_max = max(fuente_orden, na.rm = TRUE), .groups = 'drop')
  
  # Unir esta información de vuelta al dataset original
  empalme <- empalme %>%
    left_join(max_fuente_orden, by = c("iso3", "apertura")) %>%
    mutate(fuente_orden_max = ifelse(is.na(fuente_orden_max), 0, fuente_orden_max))
  
  
  # Filtrar filas donde is_empalme es 1, agrupar y calcular min de fuente_orden
  min_fuente_orden <- empalme %>%
    dplyr::filter(is_empalme == 1) %>%
    group_by(iso3, apertura) %>%
    summarise(fuente_orden_min = min(fuente_orden, na.rm = TRUE), .groups = 'drop')
  
  # Unir esta información de vuelta al dataset original
  empalme <- empalme %>%
    left_join(min_fuente_orden, by = c("iso3", "apertura")) %>%
    mutate(fuente_orden_min = ifelse(is.na(fuente_orden_min), -1, fuente_orden_min))
  
  # genero una variable booleana
  empalme <- empalme %>%
    mutate(selection = as.integer(fuente_orden <= fuente_orden_max & fuente_orden >= fuente_orden_min))
  
  grid <- empalme %>% 
    dplyr::filter(selection == 1) %>% 
    distinct(topico, tematica, variable, iso3, pais, apertura)
  
  resultados_empalmes <- data.frame()
  # data <- data %>% mutate(serie = "Serie empalmada")
  for (i in 1:nrow(grid)){
    
    t = grid$topico[[i]]
    s = grid$tematica[[i]]
    v = grid$variable[[i]]
    iso = grid$iso3[[i]]
    pais = grid$pais[[i]]
    a = grid$apertura[[i]]
    fuente = NA
    fuente_orden = NA
    serie = "Serie empalmada"
    
    log <- sprintf("Haciendo empalme para %s - %s\n", iso, a)
    cat(log)
    
    x <- empalme %>% 
      dplyr::filter(topico == t & tematica == s & variable == v & iso3 == iso & apertura == a & selection == 1) %>% 
      select(anio, id_fuente, valor) %>% 
      pivot_wider(names_from = id_fuente, values_from = valor)
    
    resultado_empalme_iso_ap <- x %>% select(anio)
    x1 <- x %>% select(-anio)
    
    sources <- colnames(x1)
    
    x2 <- as.data.frame(lapply(x1, function(x) lead(x,1)))
    colnames(x2) <- sources
    
    division <- x2/x1
    tasas <- unlist(lapply(1:nrow(division), function(row) quedarse_con_el_ultimo_no_na(division[row,])))
    tasas <-  c(tasas[1:(length(tasas)-1)], 1)
    idx <- 1:length(tasas)
    idx_rev <- sort(idx, decreasing = T)
    tasas_rev <- tasas[idx_rev]
    
    result <- x1[,length(sources)] %>% tail(1) %>% pull()
    emp <- c()
    for (tasa in tasas_rev){
      # cat("tengo la tasa ", tasa, " la divido por ", result,"\n")
      result <- result/tasa
      # cat("obtengo: ", result, "\n")
      emp <- c(emp, result)
    }
    resultado_empalme_iso_ap <- resultado_empalme_iso_ap %>% 
      mutate(
        valor = emp[idx_rev],
        topico = t,
        tematica = s,
        variable = v,
        iso3 = iso,
        pais = pais, 
        apertura = a,
        serie = serie,
        fuente = fuente,
        fuente_orden = fuente_orden,
        pais = mapper[iso3]
      ) %>% 
      select(topico, tematica, variable, iso3, pais, fuente, fuente_orden, anio, apertura,  valor, serie)
    
    resultados_empalmes <- rbind(resultados_empalmes, resultado_empalme_iso_ap)
  }
  
  
  
  return(resultados_empalmes)
}


armar_tabla <- function(df_anual, df_empalme){
  data <- rbind(df_anual, 
                df_empalme
                )
  return(data)
}


encontrar_coordenadas <- function(df, cadena) {
  # Encontrar la posición de la cadena en el dataframe
  pos <- which(apply(df, 1:2, function(x) x == cadena ))
  
  # Manejo del caso en que la cadena no se encuentra en el dataframe
  if (length(pos) == 0) {
    return(c(NA, NA))  # Devolver NA si no se encuentra la cadena
  } else {
    # Convertir la posición a coordenadas de fila y columna
    fila <- (pos - 1) %% nrow(df) + 1
    columna <- (pos - 1) %/% nrow(df) + 1
    
    # Devolver un vector con fila y columna
    return(c(fila, columna))
  }
}


quitar_string_source <- function(df){
  source_str = "Source: SEDLAC (CEDLAS and The World Bank)"
  coords = encontrar_coordenadas(df = df, cadena = source_str)
  print(coords)
  row = coords[1]
  col = coords[2]
  df[row,col] <- NA
  return(df)
}

quedarse_con_el_ultimo_no_na = function(row.df){
  row.vec <- as.numeric(row.df)
  value <- row.vec[!is.na(row.vec)]
  if (length(value)>0){return(row.vec[!is.na(row.vec)] %>% tail(.,1))}
  else{return(NA)}
  
}
