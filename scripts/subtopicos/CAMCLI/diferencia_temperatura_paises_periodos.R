# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CAMCLI"
output_name <- "diferencia_temperatura_paises"
branch <- "main"
analista <- ""

fuente1 <- 'R452C295'

df <- read_fuente_clean(295)

anio_max <- max(df$year)

df_40_69 <- df %>% 
  filter(year %in% 1940:1969) %>%
  summarise(temperature_anomaly_40_69 = mean(temperature_anomaly, na.rm = T), .by = c("code", "entity"))

df_20_adelante <- df %>% 
  filter(year >= 2020) %>%
  summarise(temperature_anomaly_20_adel = mean(temperature_anomaly, na.rm = T), .by = c("code", "entity"))

df <- left_join(df_40_69, df_20_adelante)

df <- df %>% 
  mutate(dif = temperature_anomaly_20_adel- temperature_anomaly_40_69)

df <- df %>% 
  filter(entity != "Australia (NIAID)")

df <- df %>% 
  mutate(entity = str_replace(entity, "\\s+\\(NIAID\\)\\s*", ""))


check_iso3(df$code)

df <- df %>% 
  mutate(code = case_when(
    str_detect("OWID_WRL", code) ~ "WLD",
    str_detect("OWID_KOS", code) ~ "XKX",
    T ~ code
    
  ))

check_iso3(df$code)


df <- df %>% 
  left_join(get_nomenclador_geografico_front(), by = c("code" = "geocodigo"))


df <- df %>% 
  mutate(name_long = ifelse(is.na(code), entity, name_long),
         name_short = ifelse(is.na(code), entity, name_short))


df <- df %>% 
  select(-c(entity, name_short, iso_2)) %>% 
  rename(geocodigoFundar = code,
         geonombreFundar = name_long)

df <- df %>% 
    mutate(
      geonombreFundar = case_when(
        
        geonombreFundar == "Africa" ~ "África",
        geonombreFundar == "Antarctica" ~ "Antártida",
        geonombreFundar == "Arctic Ocean" ~ "Océano Ártico",
        geonombreFundar == "Asia" ~ "Asia",
        geonombreFundar == "Australia" ~ "Australia",
        geonombreFundar == "Baltic Sea" ~ "Mar Báltico",
        geonombreFundar == "Europe" ~ "Europa",
        geonombreFundar == "Fiji" ~ "Fiyi",
        geonombreFundar == "Hong Kong" ~ "Hong Kong",
        geonombreFundar == "Indian Ocean (NIAID" ~ "Océano Índico",
        geonombreFundar == "Mediterranean Region" ~ "Región Mediterránea",
        geonombreFundar == "North America" ~ "América del Norte",
        geonombreFundar == "North Atlantic Ocean" ~ "Océano Atlántico Norte",
        geonombreFundar == "North Pacific Ocean" ~ "Océano Pacífico Norte",
        geonombreFundar == "Oceania" ~ "Oceanía",
        geonombreFundar == "South America" ~ "América del Sur",
        geonombreFundar == "South Atlantic Ocean" ~ "Océano Atlántico Sur",
        geonombreFundar == "South China and Easter Archipelagic Seas" ~ "Mares del Sur de China y del Archipiélago Oriental",
        geonombreFundar == "South Pacific Ocean" ~ "Océano Pacífico Sur",
        geonombreFundar == "Southern Ocean" ~ "Océano Austral",
        geonombreFundar == "Sri Lanka" ~ "Sri Lanka",
        T ~ geonombreFundar
        
      )
  )

df_output <- df %>% 
  arrange(desc(dif))


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico) 


comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("geocodigoFundar", "geonombreFundar")
)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = list("Dataset nuevo"), 
    fuentes = argendataR::colectar_fuentes(),
    analista = "",
    pk = c("geocodigoFundar", "geonombreFundar"),
    descripcion_columnas = list(
      "geonombreFundar" = "Nombre de entidad geografica",
      "geocodigoFundar"   = "Codigo en geonomenclador argendata" ,
      "temperature_anomaly_40_69" = "Diferencia promedio de temperatura en °C entre 1940 y 1969 relativa al promedio 1991-2020",
      "temperature_anomaly_20_adel" = glue::glue("Diferencia promedio de temperatura en °C entre 2020 y {anio_max} relativa al promedio 1991-2020"),
      "dif" = glue::glue("Diferencia en °C entre el periodo 1940 - 1969 y el periodo 2020 - {anio_max}")
      
    ),
    unidades = list("temperature_anomaly_40_69" = "°C",
                    "temperature_anomaly_20_adel" = "°C",
                    dif = "°C")
  )


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = branch)
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = branch)
