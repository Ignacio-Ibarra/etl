################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "INFDES"
output_name <- "satisfaccion_vida_desempleo_pais"
fuente1 <- "R105C0"

#-- Lectura de Datos ----

wvs_df <- readr::read_csv(argendataR::get_temp_path(fuente1))


data <- wvs_df %>% 
  mutate(
    estado = case_when(
      x028 %in% c(1, 2, 3) ~ "Ocupado",
      x028 == 7 ~ "Desocupado",
      TRUE ~ NA
  )) %>% 
  filter(!is.na(estado) & a170 >= 0 & a170 < 11) %>%
  group_by(iso3c = cntry, cntry_an, anio = year, estado) %>%
  summarise(satisfaccion_vida = stats::weighted.mean(a170, gwght, na.rm = TRUE)) %>%
  ungroup()

geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  select(iso3c = m49_code_unsd, iso3 = codigo_fundar, pais_desc = desc_fundar)

#-- Procesamiento ----

df_output <- left_join(data, geonomenclador, by = join_by(iso3c)) %>% 
  mutate(iso3 = ifelse(cntry_an == "NIR", "NIR", iso3),
         pais_desc = ifelse(cntry_an == "NIR", "Irlanda del Norte", pais_desc)
         ) %>%  
  select(iso3, pais_desc, anio, estado, satisfaccion_vida)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("iso3","anio",'estado'),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = fuente1,
    analista = "",
    es_serie_tiempo = F,
    control = comparacion,
    pk = c("iso3","anio",'estado'),
    nivel_agregacion = "nacional",
    etiquetas_indicadores = list("satisfaccion_vida" = "Promedio del nivel de satisfacción con la vida (valores en la escala de 0 a 10)"),
    unidades = list("satisfaccion_vida" = "unidades")
  )
