################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MERTRA"
output_name <- "lavarropas_tasa_empleo_fem_provincia"
fuente0 <- "R106C0"
fuente1 <- "R49C16" 
fuente2 <- "R84C14"


# ENGHO 2017-2018. Equipamientos
engho_equip <- readr::read_delim(argendataR::get_temp_path(fuente0), delim="|")

# EPHTU
ephtu_df <- readr::read_csv(argendataR::get_temp_path(fuente1))


# EPH Total Urbano Diccionario Alomerados, Provincias, Regiones
codigos <- readr::read_csv(argendataR::get_temp_path(fuente2))
codigos <- codigos %>% select(aglomerado = aglom_cod_indec, provincia = prov_cod, prov_desc)

#-- Procesamiento 1----

df_lavarropas <- engho_equip %>% 
  dplyr::filter(articulo == 541110) %>% 
  dplyr::filter(cg44 != 99) %>% 
  mutate(utilizacion = ifelse(cg44 == 1, "utiliza", "no_utiliza")) %>% 
  group_by(provincia, utilizacion) %>% 
  summarize(pondera = sum(pondera)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = utilizacion, values_from = pondera, values_fill = 0) %>% 
  mutate(prop_usa_lavarropas = utiliza / (no_utiliza + utiliza)) %>% 
  select(prov_cod = provincia, prop_usa_lavarropas)

#-- Procesamiento 2----

ephtu_df <- ephtu_df %>% 
  left_join(codigos, by = join_by(aglomerado, provincia)) # Joineo así por los casos en que hay mismo aglomerado pero distinta provincia e.g. San Nicolás-Villa Constitucion

ephtu_df <- ephtu_df %>% mutate(
  # activo = case_when(
  #   estado == 1 | estado == 2 ~ 'activo',
  #   TRUE ~ 'no_activo'
  # ),
  ocupado = case_when(
    estado == 1 ~ 'ocupado',
    TRUE ~ 'no_ocupado'
  )
)


ultimo_anio <- max(ephtu_df$ano4)

df_empleo <- ephtu_df %>% 
  dplyr::filter(ano4 == ultimo_anio) %>% 
  dplyr::filter(ch06>17 & ch06<66) %>% 
  dplyr::filter(ch04 == 2) %>% 
  group_by(provincia, prov_desc, ocupado) %>% 
  summarize(pondera = sum(pondera)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ocupado, values_from = pondera, values_fill = 0) %>% 
  mutate(tasa_empleo_18_65_mujeres = ocupado / (no_ocupado + ocupado)) %>%   # La variable se guarda con este nombre pero los datos corresponden al ultimo año disponible. 
  select(prov_cod = provincia, prov_desc, tasa_empleo_18_65_mujeres)


df_output <- left_join(df_empleo, df_lavarropas, by = join_by(prov_cod))


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("prov_cod"),
  drop_output_drive = F
)

#-- Exportar Output ----

# cambio nombre archivo

path <- glue::glue("{tempdir()}/{output_name}.csv")

df_output %>% write_csv_fundar(.,path)
# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1, fuente2, fuente3), # fuente3 debería ser la de la ENGHO
    analista = "",
    pk = c("prov_cod"),
    es_serie_tiempo = F,
    etiquetas_indicadores = list("tasa_empleo_18_65_mujeres" = "Ratio entre la cantidad de personas ocupadas y la cantidad de personas pertenecientes a la población económicamente activa, femenina, en la franja de los 18 a los 65 años",
                                 "prop_usa_lavarropas" = "Proporción de hogares donde usan lavarropas"),
    unidades = list("tasa_empleo_18_65_mujeres" = "unidades",
                    "prop_usa_lavarropas" = "unidades")
 )
