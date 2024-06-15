################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "precios_relativos_empalmada.csv"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

ipc_rubros_1947_2018_fyns <- readr::read_csv(argendataR::get_temp_path("R120C52"))
ipc_1810_2018_fyns <- readr::read_csv(argendataR::get_temp_path("R120C49"))
ipc_san_luis <- readr::read_csv(argendataR::get_temp_path("R156C66"))
ipc_capitulos_gba_abril_2016 <- readr::read_csv(argendataR::get_temp_path("R117C65"))
ipc_total_regiones_divisiones_indec <- readr::read_csv(argendataR::get_temp_path("R127C54"))
ipc_ponderadores_2016 <- readr::read_csv(get_temp_path("R117C29"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ---


# preparacion base fyns ---------------------------------------------------


ipc_fyns <- ipc_1810_2018_fyns %>% 
  select(anio, indice_promedio_anual_base_2015) %>% 
  filter(anio >= 1947 & anio <= 2006)  %>% 
  left_join(ipc_rubros_1947_2018_fyns %>% 
              filter(anio >= 1947 & anio <= 2006))

ipc_fyns <- ipc_fyns %>% 
  pivot_longer(-anio, names_to = "rubro", values_to = "indice")


ipc_fyns <- ipc_fyns %>% 
  mutate(rubro = case_when(
    str_detect(rubro, "alimentacion") ~ "alimentos",
    str_detect(rubro, "indumentaria") ~ "indumentaria",
    str_detect(rubro, "vivienda") ~ "vivienda",
    str_detect(rubro, "equipamiento") ~ "equipamiento",
    str_detect(rubro, "salud") ~ "salud",
    str_detect(rubro, "transporte") ~ "transporte",
    str_detect(rubro, "esparcimiento") ~ "esparcimiento",
    str_detect(rubro, "educacion") ~ "educacion",
    str_detect(rubro, "bienes_y_servicios_varios") ~ "otros",
    rubro == "indice_promedio_anual_base_2015" ~ "nivel_general"
    
  ))



ipc_fyns <- ipc_fyns %>% 
  select(anio, rubro, indice_fyns = indice)


# preparacion base sl -----------------------------------------------------



ipc_san_luis <- ipc_san_luis %>% 
  filter(fecha >= ym("2006-01") & fecha <= ym("2016-04")) 

ipc_san_luis <- ipc_san_luis %>% 
  mutate(rubro = case_when(
    rubro == "Nivel General" ~ "nivel_general",
    rubro == "Alimentos y Bebidas" ~ "alimentos",
    rubro == "Indumentaria" ~ "indumentaria",
    rubro == "Viviendas y Servicios básicos" ~ "vivienda",
    rubro == "Equipamiento y matenimiento del hogar" ~ "equipamiento",
    rubro == "Atención médica y gastos para la salud" ~ "salud",
    rubro == "Transporte y comunicaciones"            ~ "transporte",
    rubro == "Esparcimiento"            ~ "esparcimiento",
    rubro == "Educación"                           ~ "educacion",
    rubro == "Otros Bienes y Servicios"  ~ "otros"
  ))



ipc_san_luis <- ipc_san_luis %>% 
  select(fecha, rubro, indice_sl = indice)

# ipc_san_luis <- ipc_san_luis %>% 
#   pivot_wider(id_cols = c(fecha, fuente), names_from = "rubro", values_from = "indice")

ipc_san_luis


# prepracion base ipc gba indec 2016 --------------------------------------


ipc_capitulos_gba_abril_2016 <- ipc_capitulos_gba_abril_2016 %>% 
  filter(fecha >= ym("2016-04") & fecha <= ym("2016-12")) 

ipc_capitulos_gba_abril_2016 <- ipc_capitulos_gba_abril_2016 %>% 
  mutate(rubro = case_when(
    capitulo == "Nivel general" ~ "nivel_general",
    capitulo == "Alimentos y Bebidas" ~ "alimentos",
    capitulo == "Indumentaria" ~ "indumentaria",
    capitulo == "Vivienda y servicios básicos" ~ "vivienda",
    capitulo == "Equipamiento y mantenimiento del hogar" ~ "equipamiento",
    capitulo == "Atención médica y gastos para la salud" ~ "salud",
    capitulo == "Transporte y comunicaciones" ~ "transporte",
    capitulo == "Esparcimiento" ~ "esparcimiento",
    capitulo == "Educación" ~ "educacion",
    capitulo == "Otros bienes y servicios" ~ "otros"
  ))

ipc_capitulos_gba_abril_2016 <- ipc_capitulos_gba_abril_2016 %>% 
  select(fecha, rubro, indice_gba = indice)



# preparacion base ipc indec 2016 nacional --------------------------------


ipc_total_regiones_divisiones_indec <- ipc_total_regiones_divisiones_indec %>%
  mutate(fecha = lubridate::ym(paste(anio, mes, sep = "-"))) %>% 
  filter(region == "Nacional" & fecha >= lubridate::ym("2016-12")) %>% 
  select(-v_m_ipc, -v_i_a_ipc)

# para homologar a ipc_capitulos_gba_abril_2016_mayo_2017_indec
ipc_total_regiones_divisiones_indec <- ipc_total_regiones_divisiones_indec %>% 
  mutate(rubro = case_when(
    codigo == "01" ~ "alimentos",
    codigo == "02" ~ "alimentos",
    codigo == "03" ~ "indumentaria",
    codigo == "04" ~ "vivienda",
    codigo == "05" ~ "equipamiento",
    codigo == "06" ~ "salud",
    codigo == "07" ~ "transporte",
    codigo == "08" ~ "transporte",
    codigo == "09" ~ "esparcimiento",
    codigo == "10" ~ "educacion",
    codigo == "11" ~ "alimentos", # Casi todo es restaurantes, muy poco hoteles, que casi no pondera
    codigo == "12" ~ "otros",
    codigo == "0" ~ "nivel_general",
    TRUE ~ NA_character_ # Para manejar otros casos, si existieran
  ))


ipc_total_regiones_divisiones_indec <- ipc_total_regiones_divisiones_indec %>% 
  filter(!is.na(rubro))

ipc_total_regiones_divisiones_indec <- ipc_total_regiones_divisiones_indec %>% 
  left_join(ipc_ponderadores_2016 %>% 
              mutate(division = case_when(
                division == "Comunicaciones" ~ "Comunicación",
                T  ~ division
              )),
            by = c("region" = "region", "descripcion" = "division"))

ipc_total_regiones_divisiones_indec <- ipc_total_regiones_divisiones_indec %>% 
  mutate(peso_division = ifelse(descripcion == "Nivel general", 1, peso_division))

ipc_total_regiones_divisiones_indec <- ipc_total_regiones_divisiones_indec %>% 
  group_by(anio, mes, rubro) %>% 
  summarise(indice_ipc = sum(indice_ipc*peso_division)) %>% 
  ungroup()

ipc_total_regiones_divisiones_indec <- ipc_total_regiones_divisiones_indec %>% 
  mutate(fecha = lubridate::ym(paste(anio, mes, sep = "-")))

ipc_total_regiones_divisiones_indec <- ipc_total_regiones_divisiones_indec %>% 
  select(fecha, rubro, indice_ipc_2016  = indice_ipc )


# empalme gba sl ----------------------------------------------------------



ipc_capitulos_gba_abril_2016 <- ipc_capitulos_gba_abril_2016 %>% 
  left_join(ipc_san_luis, by = c("fecha" = "fecha", "rubro" = "rubro"))


ipc_capitulos_gba_abril_2016 <- ipc_capitulos_gba_abril_2016 %>% 
  mutate(coef_sl_gba = indice_sl/indice_gba)


ipc_capitulos_gba_abril_2016 <- ipc_capitulos_gba_abril_2016 %>%
  group_by(rubro) %>% 
  mutate(indice_empalme = indice_gba*coef_sl_gba[!is.na(coef_sl_gba)])


# empalme ipc nac 2016 con gba empalmada -------------------------------------------------------------------------


ipc_total_regiones_divisiones_indec <- ipc_total_regiones_divisiones_indec %>% 
  left_join(ipc_capitulos_gba_abril_2016, by = c("fecha", "rubro"))


ipc_total_regiones_divisiones_indec <- ipc_total_regiones_divisiones_indec %>% 
  mutate(coef_ipc_nac_indice_empalme = indice_empalme/indice_ipc_2016)



# piso var de empalme anterior
ipc_total_regiones_divisiones_indec <- ipc_total_regiones_divisiones_indec %>%
  group_by(rubro) %>% 
  mutate(indice_empalme = indice_ipc_2016*coef_ipc_nac_indice_empalme[!is.na(coef_ipc_nac_indice_empalme)])


  

# union series sl + gba + ipc nac  ----------------------------------------


df <- ipc_san_luis %>% 
  rename(indice_empalme = indice_sl) %>% 
  bind_rows(ipc_capitulos_gba_abril_2016) %>% 
  bind_rows(ipc_total_regiones_divisiones_indec)

df %>% 
  count(fecha, rubro, indice_empalme) %>% 
  filter(n > 1)

df <- df %>% 
  distinct(fecha, rubro, indice_empalme)

df <- df %>% 
  mutate(anio = year(fecha)) %>% 
  group_by(anio, rubro) %>% 
  mutate(n = n())  %>% 
  ungroup() %>% 
  filter(n == 12) %>% 
  select(-c(fecha, n))
  

df <- df %>% 
  group_by(anio, rubro) %>% 
  summarise(indice_empalme = mean(indice_empalme, na.rm = T)) %>% 
  ungroup()


# empalme df union con fyns -----------------------------------------------



ipc_fyns <- ipc_fyns %>% 
  left_join(df, by = c("anio", "rubro"))


ipc_fyns <- ipc_fyns %>% 
  mutate(coef_indice_empalme = indice_empalme/indice_fyns)


# piso var de empalme anterior
ipc_fyns <- ipc_fyns %>%
  group_by(rubro) %>% 
  mutate(indice_empalme = indice_fyns*coef_indice_empalme[!is.na(coef_indice_empalme)]) %>% 
  ungroup()



# union df empalmado y fyns -----------------------------------------------



df <- df %>%
  bind_rows(ipc_fyns)



# paso a base 1999 --------------------------------------------------------

df <- df %>% 
  group_by(rubro) %>% 
  mutate(indice_base1999 = 100*indice_empalme/indice_empalme[anio == 1999], 
         indice_base1974 = 100*indice_empalme/indice_empalme[anio == 1974]) %>%
  ungroup() %>%
  group_by(anio) %>% 
  mutate(preciorel_base1974 =  100*indice_base1974/indice_base1974[rubro == "nivel_general"]) 

df <- df %>% 
  mutate(rubro = case_when(
    str_detect(rubro, "alimentos") ~ "Alimentos y bebidas",
    str_detect(rubro, "indumentaria") ~ "Indumentaria y calzado",
    str_detect(rubro, "general") ~ "Nivel general",
    str_detect(rubro, "equipamiento") ~ "Equipamiento y mantenimiento del hogar",
    str_detect(rubro, "salud") ~ "Salud",
    str_detect(rubro, "vivienda") ~ "Vivienda",
    str_detect(rubro, "transporte") ~ "Transporte y comunicaciones",
    str_detect(rubro, "otros") ~ "Otros bienes y servicios",
    str_detect(rubro, "educacion") ~ "Educación",
    str_detect(rubro, "esparcimiento") ~ "Esparcimiento"
  ))



df_output <- df %>% 
  select(anio, rubro, precio_relativo = preciorel_base1974)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  subtopico = "PRECIO", entrega_subtopico = "datasets_update",
  pk = c("anio", "rubro"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "PRECIO",
    fuentes = c("R120C52", "R120C49", "R156C66", "R117C65", "R127C54", "R127C54", "R117C29"),
    analista = "",
    pk = c("anio", "rubro"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    # columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("precio_relativo" = "Precio relativo respecto al nivel general. Base 1974 = 100"),
    unidades = list("precio_relativo" = "porcentaje")
  )

