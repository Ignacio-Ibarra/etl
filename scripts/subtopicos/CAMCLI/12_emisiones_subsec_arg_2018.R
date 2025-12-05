#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CAMCLI"
output_name <- "emisiones_subsec_arg"
old_name <- "emisiones_subsec_arg_2018"
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

data %>% 
  colnames()

# equivalencias vs 2018
# Categoria	= Actividad
# Subcategoria 1er Orden = Subactividad
# Fuente = Categoria


data %>% 
  distinct(actividad, subactividad) %>% 
  print(n = 100)


## me quedo con las variables necesarias para el dataset
emisiones_sector <- data %>%
  select(anio, sector, actividad, subactividad, categoria, valor_en_mtco2e) %>%
  mutate(
    # Map sector based on actividad/subactividad
    sector = case_when(
      # Agriculture, livestock, forestry and other land use
      actividad == "Ganadería" ~ "Agricultura, ganadería, silvicultura y otros usos de la tierra",
      actividad == "Uso de Suelos" ~ "Agricultura, ganadería, silvicultura y otros usos de la tierra",
      actividad == "Tierra" ~ "Agricultura, ganadería, silvicultura y otros usos de la tierra",
      # Energy
      actividad == "Actividades de quema de combustible" ~ "Energía",
      actividad == "Emisiones fugitivas provenientes de la fabricación de combustibles" ~ "Energía",
      # Industrial processes and product use
      actividad == "Industria química" ~ "Procesos industriales y uso de productos",
      actividad == "Industria de los metales" ~ "Procesos industriales y uso de productos",
      actividad == "Industria de los minerales" ~ "Procesos industriales y uso de productos",
      actividad == "Uso de productos" ~ "Procesos industriales y uso de productos",
      # Waste
      actividad == "Residuos Sólidos" ~ "Residuos",
      actividad == "Incineración de residuos" ~ "Residuos",
      actividad == "Aguas Residuales" ~ "Residuos",
      TRUE ~ NA_character_
    ),
    # Map subsector based on actividad/subactividad
    subsector = case_when(
      # Agriculture, livestock, forestry and other land use
      str_detect(categoria ,"Quema de biomasa") ~ "Emisiones de la quema de biomasa",
      str_detect(categoria, "Productos de madera recolectada") ~ "Emisiones directas e indirectas de N2O y otros", # esto lo aplico por consistencia con el tratamiento anterior pero no es fiel a la categorizacion de origen
      actividad == "Ganadería" ~ "Ganado",
      actividad == "Uso de Suelos" & subactividad %in% c("Excretas en pasturas", "Fertilizantes orgánicos") ~ "Emisiones directas e indirectas de N2O y otros",
      actividad == "Uso de Suelos" & subactividad %in% c("Uso de Suelos", "Cambio de carbono en suelos") ~ "Tierra",
      actividad == "Tierra" ~ "Tierra",
      # Energy
      actividad == "Actividades de quema de combustible" & subactividad == "Industrias de la energía" ~ "Industrias de la energía",
      actividad == "Actividades de quema de combustible" & subactividad == "Industrias manufactureras y de la construcción" ~ "Industrias manufactureras y de la construcción",
      actividad == "Actividades de quema de combustible" & subactividad == "Transporte" ~ "Transporte",
      actividad == "Actividades de quema de combustible" & subactividad == "Otros sectores" ~ "Otros sectores",
      actividad == "Emisiones fugitivas provenientes de la fabricación de combustibles" ~ "Emisiones fugitivas provenientes de la fabricación de combustibles",
      # Industrial processes and product use
      actividad == "Industria química" ~ "Industria química",
      actividad == "Industria de los metales" ~ "Industria de los metales",
      actividad == "Industria de los minerales" ~ "Industria de los minerales",
      actividad == "Uso de productos" & subactividad %in% c("Uso no energético", "Usos de Sustitutos de SAO") ~ "Otros",
      # Waste
      actividad == "Residuos Sólidos" ~ "Residuos sólidos",
      actividad == "Incineración de residuos" ~ "Residuos sólidos",
      actividad == "Aguas Residuales" ~ "Aguas residuales",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(anio = as.numeric(anio)) %>% 
  group_by(sector, subsector, anio) %>%
  summarise(valor_en_mtco2e = sum(valor_en_mtco2e, na.rm = TRUE)) %>% 
  ungroup() 

## legacy
# emisiones_sector <- data %>%
#   select(anio, sector, actividad, subactividad, categoria, valor_en_mtco2e) %>%
#   mutate(subsector = case_when(
#     subactividad == "Industrias de la energía" ~ subactividad,
#     subactividad == "Industrias manufactureras y de la construcción" ~ subactividad,
#     subactividad == "Transporte" ~ subactividad,
#     subactividad == "Otros sectores" ~ subactividad,
#     actividad == "Emisiones fugitivas provenientes de la fabricación de combustibles" ~ actividad,
#     actividad == "Industria de los minerales" ~ actividad,
#     actividad == "Industria química" ~ actividad,
#     actividad == "Industria de los metales" ~ actividad,
#     subactividad %in% c("Uso de productos no energéticos de combustibles y de solvente","Usos de productos como sustitutos de las sustancias que agotan la capa de ozono","") ~ "Otros",
#     actividad == "Ganado" ~ actividad,
#     actividad == "Tierra" ~ actividad,
#     subactividad == "Emisiones de la quema de biomasa" ~ subactividad,
#     subactividad %in% c("Emisiones directas de N2O de los suelos gestionados", "Emisiones indirectas de N2O de los suelos gestionados", "Emisiones indirectas de N2O resultantes de la gestión del estiércol", "Cultivo de Arroz", "Aplicación de urea") ~ "Emisiones directas e indirectas de N2O y otros",
#     actividad %in% c("Eliminación de residuos sólidos", "Tratamiento biológico de los Residuos sólidos", "Incineración de residuos") ~ "Residuos sólidos",
#     actividad == "Tratamiento y eliminación de aguas residuales" ~ "Aguas residuales",
#     (actividad == "0" & subactividad == "0") ~ "Emisiones directas e indirectas de N2O y otros",
#     TRUE ~ NA_character_
#   )) %>% 
#   group_by(sector, subsector, anio) %>%
#   summarise(valor_en_mtco2e = sum(valor_en_mtco2e, na.rm = TRUE)) %>% 
#   ungroup() 

df_output <- emisiones_sector %>% 
  group_by(anio) %>% 
  mutate(valor_en_porcent = 100 * valor_en_mtco2e / sum(valor_en_mtco2e)) %>% 
  ungroup()
            

df_anterior <- descargar_output(nombre=output_name,
                                subtopico = "CAMCLI")


comparacion <- argendataR::comparar_outputs(df,
  df_anterior = df_anterior %>% 
    filter(anio == 2018),
  df = df_output %>% 
    mutate(anio = as.numeric(anio)) %>% 
    filter(anio == 2018),
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
    cambio_nombre_output = list('nombre_nuevo' = output_name,
                                'nombre_anterior' = old_name),
    control = comparacion,
    subtopico = subtopico,
    fuentes = c(fuente1),
    analista = analista,
    pk = c("sector","anio","subsector"),
    es_serie_tiempo = T,
    aclaraciones = "Datos a 2022",
    columna_indice_tiempo = "anio",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("sector" = "Sector de fuente de emisiones",
                                 "subsector" = "Subsector de fuente de emisiones", "anio" = "Año",
                                 "valor_en_mtco2e" = "Emisiones de GEI en millones de toneladas de CO2 equivalente",
                                 "valor_en_porcent" = "Porcentaje sobre el total de mtco2e de GEI"),
    unidades = list("valor_en_mtco2e" = "Millones de toneladas de CO2 equivalente",
                    "valor_en_porcent" = "Porcentaje")
  )



mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "main")

