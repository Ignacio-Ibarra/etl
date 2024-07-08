################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "4_tasa_de_inflacion_anual_paises_promedio_2007_2022.csv"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
ipc_fyns <- readr::read_csv(argendataR::get_temp_path("R120C49"))

ipc_sanluis <- readr::read_csv(argendataR::get_temp_path("R124C50"))

ipc_indec <- readr::read_csv(argendataR::get_temp_path("R127C54"))

ipc_weo <- readr::read_csv(get_temp_path("R34C2")) %>% 
  filter(weo_subject_code %in% c("PCPIEPCH", "LP"))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----


ipc_fyns <-  ipc_fyns %>% 
  filter(anio %in% 2017:2018) %>% 
  select(anio,valor = var_percent_promedio_anual_dic) %>% 
  mutate(valor = 100*valor)


ipc_sanluis <- ipc_sanluis %>% 
  filter(anio %in% 2006:2016) %>% 
  arrange(anio) %>% 
  filter(mes == 12) %>% 
  mutate(valor = (indice/lag(indice) -1)*100) %>% 
  filter(anio != 2006) %>% 
  select(anio, valor)


ipc_indec <- ipc_indec %>% 
  filter(anio >= 2019 & mes == 12 & region == "Nacional" & descripcion == "Nivel general") %>% 
  select(anio, valor =  v_i_a_ipc)


df_ipc <- bind_rows(ipc_fyns, ipc_sanluis, ipc_indec) %>% 
  arrange(anio) %>% 
  mutate(iso3 = "ARG")

max_anio <- max(df_ipc$anio)


paises_sel <- ipc_weo %>% 
  group_by(iso3) %>% 
  filter(weo_subject_code == "LP" & !is.na(valor)) %>% 
  filter(anio == max(anio)) %>% 
  filter(valor > 2.5) %>% 
  pull(iso3) %>% unique()

ipc_weo <- ipc_weo %>% 
  filter(iso3 %in% paises_sel & anio %in% 2007:max(df_ipc$anio) &
           weo_subject_code == "PCPIEPCH" & iso3 != "ARG")  %>% 
  select(-weo_subject_code)

paises_excl_by_na <- ipc_weo %>% 
  filter(is.na(valor)) %>% 
  count(iso3) %>%
  filter(n > 3) %>% 
  pull(iso3) %>%  unique()

paises_excl_by_na <- paises_excl_by_na[!paises_excl_by_na %in% "ZWE"]

ipc_weo <- ipc_weo %>% 
  filter(! iso3 %in% paises_excl_by_na)

df_ipc <- df_ipc %>% 
  bind_rows(ipc_weo)

df_ipc <- df_ipc %>% 
  group_by(iso3) %>% 
  summarise(inflacion_prom_07_22 = mean(valor, na.rm = T)) %>% 
  ungroup()

df_ipc <- df_ipc %>% 
  filter(iso3 != "WBG")

df_output <- df_ipc

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  subtopico = "PRECIO",
  entrega_subtopico = "datasets_update",
  nombre = output_name,
  pk = c("iso3"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "PRECIO",
    fuentes = c("R120C49", "R124C50", "R127C54", "R34C2"),
    analista = "",
    control = comparacion,
    pk = c("iso3"),
    es_serie_tiempo = F,
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("inflacion_prom_07_22" = glue::glue("Porcentaje de inflacion interanual promedio entre 2007 y {max_anio}")),
    unidades = list("inflacion_prom_07_22" = "porcentaje")
  )

