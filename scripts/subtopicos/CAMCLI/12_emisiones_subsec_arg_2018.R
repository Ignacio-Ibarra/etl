#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CAMCLI"
output_name <- "emisiones_subsec_arg_2018"
analista = "Ana Julia Aneise y Elisabeth Möhle"
fuente1 <- "R131C55"



get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}

get_clean_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}clean/")
  df_fuentes_clean <- fuentes_clean() 
  path_clean <- df_fuentes_clean[df_fuentes_clean$codigo == codigo,c("path_clean")]
  return(paste0(prefix, path_clean))
}

# Cargo data desde server
data <- arrow::read_parquet(get_clean_path(fuente1))


## me quedo con las variables necesarias para el dataset
emisiones_sector <- data %>%
  select(anio, sector, categoria, subcategoria_1er_orden, fuente, valor_en_mtco2e) %>%
  mutate(subsector = case_when(
    subcategoria_1er_orden == "Industrias de la energía" ~ subcategoria_1er_orden,
    subcategoria_1er_orden == "Industrias manufactureras y de la construcción" ~ subcategoria_1er_orden,
    subcategoria_1er_orden == "Transporte" ~ subcategoria_1er_orden,
    subcategoria_1er_orden == "Otros sectores" ~ subcategoria_1er_orden,
    categoria == "Emisiones fugitivas provenientes de la fabricación de combustibles" ~ categoria,
    categoria == "Industria de los minerales" ~ categoria,
    categoria == "Industria química" ~ categoria,
    categoria == "Industria de los metales" ~ categoria,
    subcategoria_1er_orden %in% c("Uso de productos no energéticos de combustibles y de solvente","Usos de productos como sustitutos de las sustancias que agotan la capa de ozono","") ~ "Otros",
    categoria == "Ganado" ~ categoria,
    categoria == "Tierra" ~ categoria,
    subcategoria_1er_orden == "Emisiones de la quema de biomasa" ~ subcategoria_1er_orden,
    subcategoria_1er_orden %in% c("Emisiones directas de N2O de los suelos gestionados", "Emisiones indirectas de N2O de los suelos gestionados", "Emisiones indirectas de N2O resultantes de la gestión del estiércol", "Cultivo de Arroz", "Aplicación de urea") ~ "Emisiones directas e indirectas de N2O y otros",
    categoria %in% c("Eliminación de residuos sólidos", "Tratamiento biológico de los Residuos sólidos", "Incineración de residuos") ~ "Residuos sólidos",
    categoria == "Tratamiento y eliminación de aguas residuales" ~ "Aguas residuales",
    (categoria == "0" & subcategoria_1er_orden == "0") ~ "Emisiones directas e indirectas de N2O y otros",
    TRUE ~ NA_character_
  )) %>% 
  group_by(sector, subsector, anio) %>%
  summarise(valor_en_mtco2e = sum(valor_en_mtco2e, na.rm = TRUE)) %>% 
  ungroup() 

df_output <- emisiones_sector %>% 
  group_by(anio) %>% 
  mutate(valor_en_porcent = 100 * valor_en_mtco2e / sum(valor_en_mtco2e)) %>% 
  ungroup()
            

df_anterior <- descargar_output(nombre=output_name,
                                subtopico = "CAMCLI")


comparacion <- argendataR::comparar_outputs(df,
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  subtopico = subtopico,
  pk = c("sector","anio","subsector"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    control = comparacion,
    subtopico = subtopico,
    fuentes = c(fuente1),
    analista = analista,
    pk = c("sector","anio","subsector"),
    es_serie_tiempo = T,
    aclaraciones = "Datos a 2018. Sin cambios",
    columna_indice_tiempo = "anio",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("valor_en_mtco2e" = "Emisiones de GEI en millones de toneladas de CO2 equivalente",
                                 "valor_en_porcent" = "Porcentaje sobre el total de mtco2e de GEI"),
    unidades = list("valor_en_mtco2e" = "Millones de toneladas de CO2 equivalente",
                    "valor_en_porcent" = "Porcentaje")
  )



mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "dev")

