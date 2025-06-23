#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "06_desembarques_puertos_totales.csv"
analista <- "Ignacio Ibarra"

fuente1 <- 'R331C204' # MAGyP Desembarque por puerto, flota, especie, anio, mes (2022- ultimo anio)

df_magyp <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)


df_puertos <- df_magyp %>% 
  dplyr::filter(anio == max(anio)) %>% 
  mutate(puerto = tolower(puerto) %>% 
           str_replace(., "\\.","\\. ") %>% 
           str_replace(., "\\/","/ ") %>% 
           tools::toTitleCase(.) %>% 
           str_replace(., "\\s+"," ")) %>% 
  group_by(anio, provincia, puerto) %>% 
  summarise(
    desembarque_toneladas = sum(desembarque_toneladas, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    share = desembarque_toneladas / sum(desembarque_toneladas)
  ) 
  

puertos_mapa <- data.frame(
  puerto = c(
    "Gral. Lavalle", "Ing. White", "Mar Del Plata", "Necochea/ Quequen",
    "Rio Salado", "Rosales", "San Clemente Del Tuyú", "San Antonio Este",
    "San Antonio Oeste", "Caleta Cordova", "Camarones", "Comodoro Rivadavia",
    "Puerto Madryn", "Rawson", "Caleta Olivia/ Paula", "Pto. Deseado",
    "San Julian", "Ushuaia"
  ),
  latitud = c(
    -36.331309206051436, -38.790735093548, -38.0145142529216, -38.5587277023737,
    -35.98612365050119, -38.89851906829617, -36.3704488240768, -40.8161985485906, 
    -40.7304625210211, -45.75030308356952, -44.799876730024, -45.8595405354173, 
    -42.6523223561081, -43.3373832464434, -46.4365514331273, -47.7494295781015,
    -49.3110012030052, -54.8019813614958
  ),
  longitud = c(
    -56.908404188507916, -62.2657430260339, -57.5311675060392, -58.6967627366004,
    -57.37153917132423, -62.07959556931424, -56.7118156659205, -64.7566274815578,
    -64.9293096237399, -67.37655971806964, -65.7037054585528, -67.4771848534541,
    -65.0661610144471, -65.0544875760684, -67.5144022831659, -65.8890829225222,
    -67.716666856714, -68.3029661143961
  )
)


df_output <- df_puertos %>% 
  left_join(puertos_mapa, join_by(puerto)) %>% 
  drop_na(latitud) %>% 
  mutate(provincia = str_remove(provincia, "Pcia. "))

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("puerto"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

colectar_fuentes <- function(pattern = "^fuente.*"){
  
  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)
  
  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())
  
  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))
  
  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]
  
  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk =  c("puerto"),
    es_serie_tiempo = F,
    control = comparacion,
    columna_indice_tiempo = NULL,
    columna_geo_referencia = NULL,
    nivel_agregacion = NULL,
  )





# library(rnaturalearth)
# library(rnaturalearthdata)
# library(sf)
# library(ggrepel)
# 
# 
# 
# arg_mapa <- ne_states(country = "Argentina", returnclass = "sf") %>%
#   filter(name != "Antártida Argentina")
# 
# 
# malvinas <- ne_countries(country = "Falkland Islands", scale = "large", returnclass = "sf")
# 
# 
# malvinas$admin <- "Argentina"
# malvinas$name <- "Islas Malvinas"
# 
# 
# malvinas <- st_cast(malvinas, "MULTIPOLYGON")
# 
# 
# malvinas_clean <- malvinas %>%
#   transmute(
#     name = "Islas Malvinas",
#     geometry = geometry
#   )
# 
# 
# arg_mapa_clean <- arg_mapa %>%
#   select(name, geometry)
# 
# 
# argentina_completa <- rbind(arg_mapa_clean, malvinas_clean)
# 
# 
# 
# 
# plot_data <- df_output %>%
#   mutate(
#     puerto = case_when(
#       puerto == "Caleta Olivia/ Paula" ~ "Caleta Olivia", 
#       puerto == "Mar Del Plata" ~ "Mar del Plata",
#       TRUE ~ puerto
#     ),
#     label = paste0(puerto, " ", round(100*share, 1), "%") ) %>%
#   filter(share >= 0.01)
# 
# puntos_sf <- st_as_sf(plot_data, coords = c("longitud", "latitud"), crs = 4326)
# 
# 
# 
# 
# ggplot() +
#   geom_sf(data = argentina_completa, fill = "#d5e3f2", color = "#000000") +
#   geom_sf(data = puntos_sf, aes(size = share), alpha = 0.9, color = "#2e75bc") +
#   geom_text_repel(
#     data = plot_data,
#     aes(x = longitud, y = latitud, label = label),
#     size = 4,
#     segment.size = 0.3,
#     hjust = -1,                 # Alineado a izquierda del texto
#     nudge_x = 1,
#     direction = "both",
#     box.padding = 0.1,
#     point.padding = 0.4,
#     force = 2
#   ) +
#   scale_color_viridis_c(option = "C") +
#   scale_size_continuous(range = c(2, 10)) +
#   coord_sf(xlim = c(-75, -52), ylim = c(-56, -21)) +
#   theme_void() + 
#   theme(
#     legend.position = "none"
#   )
