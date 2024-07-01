################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "INFDES"
output_name <- "tasa_desempleo_eph_niveled"
codigos.eph <- fuentes_raw() %>% filter(grepl("Encuesta Permanente de Hogares, Individual*", nombre)) %>% select(nombre, codigo) 

# librerías


#-- Lectura de Datos ----

anios <- 2003:2023


# Creo una función custom para aplicar un determinado wrangling a cada dataset de EPH
eph_tasa_desocupacion_nivel_ed <- function(eph_data) {
  outdf <- eph_data %>%
    filter(nivel_ed %in% 1:7) %>% 
    select(anio = ano4, nivel_ed, edad = ch06, estado, pondera) %>%
    mutate(
      desocupado = case_when(
        estado == 2 ~ 1,
        estado == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      nivel_ed_cod = case_when(
        nivel_ed <= 3 ~ 1,
        nivel_ed == 7 ~ 1,
        nivel_ed == 4 ~ 2,
        nivel_ed == 5 ~ 3,
        nivel_ed == 6 ~ 4,
        TRUE ~ NA_real_
      ),
      nivel_ed_desc = case_when(
        nivel_ed_cod == 1 ~ "Hasta secundaria incompleta",
        nivel_ed_cod == 2 ~ "Secundaria completa",
        nivel_ed_cod == 3 ~ "Superior incompleta",
        nivel_ed_cod == 4 ~ "Superior completa",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(desocupado)) %>%
    filter(edad>=30 & edad<55) %>% 
    group_by(anio, desocupado, nivel_ed_cod, nivel_ed_desc) %>%
    summarise(pondera = sum(pondera, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(anio, nivel_ed_cod, nivel_ed_desc) %>%
    mutate(totalactivos = sum(pondera),
           tasa_desocupacion = pondera / totalactivos) %>%
    filter(desocupado != 0) %>%
    select(anio, nivel_ed_cod, nivel_ed_desc, tasa_desocupacion)

  return(outdf)
}

get_eph_fuente <- function(year, codes_and_names) {
  fuente <- codigos.eph%>% filter(grepl(year, nombre)) %>% select(codigo) %>% pull()
  fuente
}

# Creo una función que levanta el dataset correspondiente a un año
load_eph_by_year <- function(year, codes_and_names){
  fuente <- get_eph_fuente(year, codes_and_names)
  eph_df <- data.table::fread(argendataR::get_temp_path(fuente))
  return(eph_df)
}

# Creo una función de cleaning de cada archivo
cleaning_eph <- function(eph_data){
  colnames(eph_data) <- tolower(colnames(eph_data))
  return(eph_data)
}


# Creo una función que procesa una lista de años 
eph_processing <- function(years, codes_and_names, custom_wrangling){
  result_processing <- data.table()
  for (year in years){
    cat(sprintf("Archivo EPH (%s)... empezando\n", year))
    
    # Cargo archivo
    eph_df <- load_eph_by_year(year = year, codes_and_names = codes_and_names)
    dimensiones <- dim(eph_df)
    cat(sprintf("... cargando %s filas y %s columnas \n", dimensiones[1], dimensiones[2]))
    
    # Hago cleaning
    eph_df <- cleaning_eph(eph_data = eph_df)
    cat("... limpieza\n")
    
    # Hago wrangling
    result_df <- custom_wrangling(eph_data = eph_df)
    cat("... procesado\n")
    
    result_processing <- rbind(result_processing, result_df, fill=F)
    cat("... finalizado\n")
    
  }
  return(result_processing)
}

#-- Procesamiento ----

df_output <- eph_processing(years = anios, codes_and_names = codigos.eph, custom_wrangling = eph_tasa_desocupacion_nivel_ed)
lista_fuentes <- map(anios, function(x) {get_eph_fuente(x, codes_and_names = codigos.eph)})
lista_fuentes <- as.character(lista_fuentes)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("anio", "nivel_ed_cod"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = lista_fuentes,
    analista = "",
    control = comparacion,
    pk = c("anio", "nivel_ed_cod","nivel_ed_desc"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    etiquetas_indicadores = list("tasa_desocupacion" = "Tasa de desocupación"),
    unidades = list("tasa_desocupacion" = "unidades")
  )

