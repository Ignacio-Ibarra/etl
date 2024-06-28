################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SALING"
output_name <- "prima_formalidad_argentina"
codigos.eph <- fuentes_raw() %>% filter(grepl("Encuesta Permanente de Hogares, Individual*", nombre)) %>% select(nombre, codigo) 

# librerías

require(data.table)

#-- Lectura de Datos ----

anios <- 2003:2023

normalize_pp3e_tot <- function(value){
  if (is.character(value)){
    value <- str_replace(value, ",",".")
    value <- as.numeric(value)
  }
  return(as.numeric(value))
}



verificacion_datos <- function(eph_data){
  data <- eph_data %>%
      select(anio = ano4, trimestre, edad = ch06, sexo = ch04, nivel_ed, pp3e_tot, p21, cat_ocup, pp07h, region) %>%
      mutate(pp3e_tot = normalize_pp3e_tot(pp3e_tot)) %>%
      mutate(salario_horario = p21 / pp3e_tot) %>%
      mutate(salario_horario =  ifelse(pp3e_tot > 900 | salario_horario < 0.1, NA, p21 / pp3e_tot)) %>%
      mutate(salario_horario = ifelse(is.infinite(salario_horario), NA, salario_horario)) %>%
      mutate(ln_salario_horario = log(salario_horario)) %>%
      dplyr::filter(cat_ocup == 3 & !is.na(ln_salario_horario))


  grouped <- data %>%
    group_by(anio, trimestre) %>%
    summarise(salario_horario_medio = mean(salario_horario, na.rm = T),
              ln_salario_horario_medio = mean(ln_salario_horario, na.rm = T),
              p21_medio = mean(p21, na.rm = T),
              pp3e_tot = mean(pp3e_tot, na.rm=T)) %>%
    ungroup()
  return(grouped)
}



eph_prima_formalidad <- function(eph_data){
  data <- eph_data %>% 
    select(anio = ano4, trimestre, edad = ch06, sexo = ch04, nivel_ed, pp3e_tot, p21, cat_ocup, pp07h, region) %>% 
    mutate(pp3e_tot = normalize_pp3e_tot(pp3e_tot)) %>% 
    mutate(salario_horario = p21 / pp3e_tot) %>% 
    mutate(salario_horario =  ifelse(pp3e_tot > 900 | salario_horario < 0.1, NA, p21 / pp3e_tot)) %>% 
    mutate(salario_horario = ifelse(is.infinite(salario_horario), NA, salario_horario),
           ln_salario_horario = log(salario_horario),
           formal = case_when(
             cat_ocup == 3 & pp07h == 1 ~ 1,
             cat_ocup == 3 & pp07h == 2 ~ 0,
             TRUE ~ NA_real_),
           edad2 = edad^2
           ) %>% 
    dplyr::filter(cat_ocup == 3 & !is.na(ln_salario_horario))
  
  reg_trim_control <- data %>% 
    group_by(anio, trimestre) %>% 
    do(model = stats::lm(ln_salario_horario ~ factor(sexo) + edad + edad2 + factor(region) + factor(nivel_ed) + formal, data = . ))
  
  beta_anual_control <- reg_trim_control %>% 
    ungroup() %>% 
    mutate(coeficientes = sapply(model, function(x) x$coefficients['formal'])) %>% 
    group_by(anio) %>% 
    summarize(prima = mean(coeficientes, na.rm =T)) %>% 
    ungroup() %>% 
    mutate(tipo_prima = "Controlando por variables sociodemográficas")
  
  
  reg_trim_sin_control <- data %>% 
    group_by(anio, trimestre) %>% 
    do(model = stats::lm(ln_salario_horario ~ formal, data = . ))
  
  beta_anual_sin_control <- reg_trim_sin_control %>% 
    ungroup() %>% 
    mutate(coeficientes = sapply(model, function(x) x$coefficients['formal'])) %>% 
    group_by(anio) %>% 
    summarize(prima = mean(coeficientes, na.rm =T)) %>% 
    ungroup() %>% 
    mutate(tipo_prima = "Sin control por otras variables")
  
  outdf <- bind_rows(beta_anual_control, beta_anual_sin_control)
  return(outdf)
}



# Creo una función que levanta el dataset correspondiente a un año
load_eph_by_year <- function(year, codes_and_names){
  fuente <- codigos.eph%>% filter(grepl(year, nombre)) %>% select(codigo) %>% pull()
  eph_df <- fread(argendataR::get_temp_path(fuente))
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

dani <- eph_processing(years = anios, codes_and_names = codigos.eph, custom_wrangling = verificacion_datos)

dani %>% write_csv('daniel.csv')

df_anterior <- argendataR::descargar_output(nombre = output_name, 
                                            subtopico = subtopico, 
                                            entrega_subtopico = "datasets_primera_entrega")

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  pk = c("anio", "tipo_prima"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = codigos.eph$codigo,
    analista = "",
    pk = c("anio", "tipo_prima"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    aclaraciones = "El dataset generado por este script conserva una diferencia sistemática para los años 2003 a 2007 con respecto al analista, aunque no es sustantiva",
    etiquetas_indicadores = list("prima" = "Brecha en el salario horario entre asalariados registrados y asalariados no registrados"),
    unidades = list("prima" = "unidades")
  )