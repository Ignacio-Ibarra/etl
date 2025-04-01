################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "vab_por_provincia"
analista = "Pablo Sonzogni"
fuente1 <- "R222C0"
fuente2 <- "R84C0"



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

# require(readr)

# Cargo data desde server
empalme_df <- read_csv(get_raw_path(fuente1)) 

# daniel_df <- read_csv("pbg por provincia.xlsx - serie empalmada PIBpc.csv") %>% 
#   select(-region_pbg) %>% 
#   pivot_longer(-provincia, names_to ="anio", values_to = "vab_pc_dani", names_transform = as.numeric)
# 
# 
# X <- empalme_df %>% 
#   left_join(daniel_df, join_by(provincia, anio)) %>% 
#   mutate(pob_implicita_dani = vab_pb / vab_pc_dani)
# 
# require(ggplot2)
# 
# plot_data <- X %>% pivot_longer(c(-anio,-provincia)) %>% dplyr::filter(name %in% c("vab_pb_per_capita","vab_pc_dani"))
# 
# 
# 
# # Crea el gráfico
# grafico <- ggplot(plot_data, aes(x = anio, y = value, colour = name)) +
#   geom_line() +
#   facet_wrap(~provincia, scales = "free", ncol = 4) +
#   theme(axis.text = element_blank(),    # Quita el texto de los ejes
#         axis.ticks = element_blank(),   # Quita las marcas de los ejes
#         strip.text = element_text(size = 10),
#         panel.spacing = unit(1, "lines")) +
#   labs(title = "Curvas de VABpb por provincia",
#        x = "Año", 
#        y = "VABpb")
# 
# # Muestra el gráfico en pantalla
# print(grafico)
# 
# # Guarda el gráfico en un archivo
# ggsave("grafico_sin_texto_ejes2.png", plot = grafico, width = 14, height = 10)


dicc_provs <- read_csv(get_raw_path(fuente2)) %>% 
  select(prov_cod, prov_desc, reg_desc_fundar) %>% 
  mutate(region = case_when(
    reg_desc_fundar %in% c("Partidos del GBA", "Pampeana", "CABA") ~ "Pampeana y AMBA",
    reg_desc_fundar == "Noreste" ~ "NEA",
    reg_desc_fundar == "Noroeste" ~ "NOA",
    TRUE ~ reg_desc_fundar
  )) %>% 
  distinct(provincia_id = prov_cod, provincia_nombre = prov_desc, region) %>% 
  dplyr::filter(!(provincia_id == 6 & region == "Patagonia")) %>% 
  mutate(provincia_id = stringr::str_pad(provincia_id, width = 2, pad = "0", side = "left"))


df_output <- empalme_df %>% 
  dplyr::filter(provincia != "No distribuido") %>% 
  rename(provincia_nombre = provincia) %>% 
  left_join(dicc_provs, join_by(provincia_nombre)) %>% 
  group_by(anio) %>% 
  mutate(participacion = vab_pb / sum(vab_pb),
         vab_pb = vab_pb / 1000000) %>%
  ungroup() %>% 
  select(-vab_pb_per_capita, -pob_total) %>% 
  dplyr::filter(anio>=2004) 




df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")  


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", "provincia_id"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)


check_iso3(df_output$provincia_id)

geo <- get_nomenclador_geografico()


df_output <- df_output %>%
  mutate(provincia_nombre = textclean::replace_non_ascii(tolower(provincia_nombre))) %>% 
  left_join(geo %>% 
              mutate(name_short = textclean::replace_non_ascii(tolower(name_short))),
            by = c("provincia_nombre" = "name_short")
            )

df_output <- df_output %>% 
  select(-c(provincia_nombre, iso_2, name_long, provincia_id)) %>% 
  rename(provincia_id = geocodigo)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1),
    analista = analista,
    pk = c("anio", "provincia_id"),
    control = comparacion,
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "provincia_id",
    nivel_agregacion = "provincia",
    aclaraciones = "Se corrigieron los codigos de provincia de acuerdo al geonomenclador de Argendata.",
    etiquetas_indicadores = list("vab_pb" = "Valor Agregado Bruto a precios básicos, en millones de pesos a precios de 2004",
                                 "participacion" = "Participacion de la provincia en el VAB nacional"),
    unidades = list("vab_pb" = "Millones de pesos a precios de 2004",
                    "participacion" = "unidades")
  )

mandar_data(paste0(output_name, ".csv"), subtopico = "CRECIM", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CRECIM",  branch = "dev")



