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





df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggrepel)




plot_data <- df_output %>% 
  mutate(label = paste(puerto, "", round(100*share,2),"%") %>% str_wrap(., width = 20))

# Obtener el mapa de Argentina
arg_mapa <- ne_states(country = "Argentina", returnclass = "sf") %>%
  filter(!name %in% c("Antártida Argentina"))  # Excluir Antártida si aparece


puntos_sf <- sf::st_as_sf(df_output, coords = c("longitud", "latitud"), crs = 4326)

ggplot() +
  geom_sf(data = arg_mapa, fill = "pink", color = "blue") +
  geom_sf(data = puntos_sf, aes(size = share), alpha = 0.3) +
  geom_text_repel(
    data = plot_data,
    aes(x = longitud, y = latitud, label = label),
    size = 2.5,
    direction = "y",
    segment.size = 0.2,
    hjust = 1,
    nudge_x = 8,
    nudge_y = 3
  )+
  scale_color_viridis_c(option = "C") +
  scale_size_continuous(range = c(2, 10)) +
  coord_sf(xlim = c(-75, -52), ylim = c(-56, -21)) +
  theme_minimal() +
  theme(
    legend.position = "None"
  )


