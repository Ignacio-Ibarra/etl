################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "provincia_pib_pc_scatter"
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

ultimo_anio <- max(empalme_df$anio)

df_output <- empalme_df %>% 
  dplyr::filter(provincia != "No distribuido") %>% 
  rename(provincia_nombre = provincia) %>% 
  dplyr::filter(anio %in% c(1895, max(anio))) %>% 
  pivot_wider(id_cols =provincia_nombre, names_from = anio, names_prefix = "pib_pc_", values_from = vab_pb_per_capita) %>% 
  mutate(var_pib_pc_1895_ultimo_anio = (get(paste0("pib_pc_",ultimo_anio))/pib_pc_1895) - 1) %>% 
  left_join(dicc_provs, by = join_by(provincia_nombre)) %>% 
  select(provincia_id, provincia_nombre, region_pbg, pib_pc_1895, var_pib_pc_1895_ultimo_anio)
  
  


df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  mutate(provincia_nombre = ifelse(provincia_nombre=="Ciudad Autónoma de Buenos Aires", "CABA", provincia_nombre)) %>% 
  mutate(var_pib_pc_1895_ultimo_anio = var_pib_pc_1895_2022 - 1) %>% 
  select(-var_pib_pc_1895_2022)


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("provincia_id"), # variables pk del dataset para hacer el join entre bases
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

check_iso3(df_output$provincia_id)





#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1),
    control = comparacion,
    aclaraciones = "Se corrigieron los codigos de provincia de acuerdo al geonomenclador de Argendata.",
    analista = analista,
    pk = c("provincia_id"),
    columna_geo_referencia = "provincia_id",
    nivel_agregacion = "provincia",
    etiquetas_indicadores = list("pib_pc_1895" = "VAB a precios básicos per cápita a pesos constantes de 2004 de 1895",
                                 "var_pib_pc_1895_ultimo_anio" = "Variación entre el VABpb per capita de 1895 y 2022"),
    unidades = list("pib_pc_1895" = "en pesos constantes de 2004",
                    "var_pib_pc_1895_ultimo_anio" = "unidades")
  )

mandar_data(paste0(output_name, ".csv"), subtopico = "CRECIM", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CRECIM",  branch = "dev")


