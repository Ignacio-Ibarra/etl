################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "DESIGU"
output_name <- "brecha_horas_trabajadas_genero"
codigos.eph <- fuentes_raw() %>% filter(grepl("Encuesta Permanente de Hogares, Individual*", nombre)) %>% select(nombre, codigo) 



meta_desigu <- metadata("DESIGU")
meta_desigu <- meta_desigu %>% 
  distinct(dataset_archivo, variable_nombre, descripcion, primary_key)

# librerías

require(data.table)

#-- Lectura de Datos ----

anios <- 2003:2023


# Creo una función custom para aplicar un determinado wrangling a cada dataset de EPH
eph_brecha_horas_trabajadas_genero <- function(eph_data) {
  outdf <- eph_data %>%
    dplyr::filter(estado == 1) %>% 
    dplyr::filter(pp3e_tot<900) %>% 
    select(anio = ano4, genero_cod = ch04, edad = ch06, pp3e_tot, pondera) %>%
    mutate(genero_desc = ifelse(genero_cod == 1, "Hombres", "Mujeres")) %>% 
    group_by(anio, genero_cod, genero_desc) %>%
    summarise(hs_trabajadas_sem = stats::weighted.mean(pp3e_tot, pondera) ) %>%
    ungroup() 
    
  
  return(outdf)
}

# Creo una función que levanta el dataset correspondiente a un año
load_eph_by_year <- function(year, codes_and_names){
  fuente <- codigos.eph%>% filter(grepl(year, nombre)) %>% select(codigo) %>% pull()
  eph_df <- fread(argendataR::get_raw_path(fuente))
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

df_output <- eph_processing(years = anios, codes_and_names = codigos.eph, custom_wrangling = eph_brecha_horas_trabajadas_genero)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- descargar_output(output_name, subtopico = "DESIGU")


comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c("anio", "genero_cod", "genero_desc"),
  drop_joined_df =  F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

etiquetas <- meta_desigu %>% 
  filter(dataset_archivo == output_name) %>% 
  pull(descripcion) %>% 
  as.list()

names(etiquetas) <- meta_desigu %>% 
  filter(dataset_archivo == output_name) %>% 
  pull(variable_nombre)

pks <- meta_desigu %>% 
  filter(dataset_archivo == output_name & primary_key == "TRUE") %>% 
  pull(variable_nombre)

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = codigos.eph$codigo,
    analista = "",
    pk = c("anio", "genero_cod", "genero_desc"),
    es_serie_tiempo = T,
    control = comparacion,
    columna_indice_tiempo = "anio",
    etiquetas_indicadores = list("hs_trabajadas_sem" = "Horas promedio trabajadas semanalmente"),
    unidades = list("hs_trabajadas_sem" = "unidades"),
    aclaraciones = "El dataset update contiene algunas diferencias para el ultimo anio disponible 2023, dado que el analista creo un dataset sin tener todos los trimestres completos de dicho año, las clases de las variables genero_cod y anio no coinciden, aunque el mismatch no es dado que en el dataset del analista vienen como numeric y en el dataset nuevo se encuentran como integer."
  )
