################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "3_tasa_de_inflacion_anual_argentina_1935_2022.csv"

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
ipc_fyns <- readr::read_csv(argendataR::get_temp_path("R120C49"))

ipc_sanluis <- readr::read_csv(argendataR::get_temp_path("R124C50"))

ipc_indec <- readr::read_csv(argendataR::get_temp_path("R127"))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

ipc_fyns <-  ipc_fyns %>% 
  filter((anio >= 1935 & anio <= 2006 ) | anio %in% 2017:2018) %>% 
  select(anio,var_dic_anual = var_percent_promedio_anual_dic) %>% 
  mutate(var_dic_anual = 100*var_dic_anual)


ipc_sanluis <- ipc_sanluis %>% 
  filter(anio %in% 2006:2016) %>% 
  arrange(anio) %>% 
  filter(mes == 12) %>% 
  mutate(var_dic_anual = (indice/lag(indice) -1)*100) %>% 
  filter(anio != 2006) %>% 
  select(anio, var_dic_anual)


ipc_indec <- ipc_indec %>% 
  filter(anio >= 2019 & mes == 12 & region == "Nacional" & descripcion == "Nivel general") %>% 
  select(anio, var_dic_anual =  v_i_a_ipc)


df_ipc <- bind_rows(ipc_fyns, ipc_sanluis, ipc_indec)

df_ipc <- df_ipc %>% 
  mutate(inflacion_todos = var_dic_anual) %>% 
  mutate(inflacion_positivos = sign(inflacion_todos)*log(abs(inflacion_todos))) %>% 
  select(-var_dic_anual)

df_output <- df_ipc

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  subtopico = "PRECIO", 
  entrega_subtopico = "datasets_update",
  pk = c("anio"),
  drop_joined_df  = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "PRECIO",
    fuentes = c("R37C1", "R34C2"),
    analista = analista,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("pbi_per_capita_ppa_porcentaje_argentina" = "PBI per cápita PPA como porcentaje del de Argentina"),
    unidades = list("pbi_per_capita_ppa_porcentaje_argentina" = "porcentaje")
  )

