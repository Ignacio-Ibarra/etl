################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "provincia_pib_pc_relativo"
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
#
# X %>% write_csv(., file="revisar.csv")

# A <- X %>% group_by(anio) %>% summarise(vab_pb = sum(vab_pb, na.rm = T),
#                                         pob_total = sum(pob_total, na.rm = T),
#                                         pob_dani = sum(pob_implicita_dani, na.rm=T))


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
  mutate(provincia_id = stringr::str_pad(provincia_id, width = 2, pad = "0", side = "left")) %>% 
  select(provincia_id, provincia_nombre, region_pbg = region)


media_nacional <- empalme_df %>% 
  dplyr::filter(provincia != "No distribuido") %>%
  group_by(anio) %>%
  summarise(pib_pc_nacional = sum(vab_pb, na.rm = T) / sum(pob_total, na.rm = T))

df_output <- empalme_df %>% 
  dplyr::filter(provincia != "No distribuido") %>%
  left_join(media_nacional, by=join_by(anio)) %>% 
  rename(provincia_nombre = provincia) %>% 
  left_join(dicc_provs, join_by(provincia_nombre)) %>% 
  mutate(pib_pc_relativo = 100*vab_pb_per_capita / pib_pc_nacional) %>%
  ungroup() %>% 
  select(provincia_id, provincia_nombre, region_pbg, anio, pib_pc_relativo)


df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  mutate(provincia_nombre = ifelse(provincia_nombre=="Ciudad Autónoma de Buenos Aires", "CABA", provincia_nombre))


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", "provincia_id"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)






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
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "provincia_id",
    nivel_agregacion = "provincia",
    aclaraciones = "Los datos difieren del analista. El dato del VABpb per cápita fue construído con distintas fuentes (para ver las fuentes del analista revisar la hoja 'Notas metodológicas' del googlesheet https://docs.google.com/spreadsheets/d/1iIrqpAINo7iVbKyA0Jw5PjF4jPICi2AS/edit?usp=sharing&ouid=104529103904611843855&rtpof=true&sd=true)",
    etiquetas_indicadores = list("pib_pc_relativo" = "Producto bruto interno per cápita relativo a la media (media nacional = 100)"),
    unidades = list("pib_pc_relativo" = "porcentaje")
  )


