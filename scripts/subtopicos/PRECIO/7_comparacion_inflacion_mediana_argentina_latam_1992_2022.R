################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "7_comparacion_inflacion_mediana_argentina_latam_1992_2022.csv"


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

amlat_iso3 <- c(
  "ATG",  "ABW",  "BHS",  "BRB",  "BLZ",  "BOL",  "BRA",  "CHL",  "COL",
  "CRI",  "DMA",  "DOM",  "ECU",  "SLV",  "GRD",  "GTM",  "GUY","HTI",  "HND",
  "JAM",  "MEX",  "NIC",  "PAN",  "PRY",  "PER",  "KNA",  "LCA",  "VCT",  "SUR",
  "TTO","URY","VEN"
)


ipc_fyns <-  ipc_fyns %>% 
  filter(anio %in% 1992: 2006 | anio %in% 2017:2018) %>% 
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
  filter(iso3 %in% paises_sel & anio %in% 1992:max(df_ipc$anio) &
           weo_subject_code == "PCPIEPCH" & iso3 != "ARG")  %>% 
  select(-weo_subject_code) %>% 
  filter(iso3 %in% amlat_iso3)

# paises_excl_by_na <- ipc_weo %>% 
#   filter(is.na(valor)) %>% 
#   pull(iso3) %>% unique()

# ipc_weo <- ipc_weo %>% 
#   filter(! iso3 %in% paises_excl_by_na)

ipc_weo <- ipc_weo %>% 
  bind_rows(df_ipc)


ipc_weo <- ipc_weo %>% 
  filter(iso3 != "WBG")

ipc_weo <- ipc_weo %>% 
  group_by(anio) %>% 
  summarise(mediana_paises_inflacion = stats::median(valor, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(pais_o_grupo_de_paises = "América Latina y Caribe")

df_ipc <- df_ipc %>% 
  mutate(pais_o_grupo_de_paises = "Argentina") %>% 
  rename(mediana_paises_inflacion = valor) %>% 
  select(-iso3)


df_output <- df_ipc %>% 
  bind_rows(ipc_weo)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_anterior <- descargar_output(nombre = output_name, subtopico = "PRECIO", 
                                entrega_subtopico = "datasets_update", delim = ";")

df_anterior <- df_anterior %>% 
  mutate(mediana_paises_inflacion = as.numeric(mediana_paises_inflacion))


comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c("anio", "pais_o_grupo_de_paises"),
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
    pk = c("pais_o_grupo_de_paises", "anio"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    # columna_geo_referencia = "grupo_de_paises",
    nivel_agregacion = "grupos de paises",
    etiquetas_indicadores = list("mediana_paises_inflacion" = glue::glue("Mediana de inflación anual de los paises del grupo para cada año")),
    unidades = list("mediana_paises_inflacion" = "porcentaje")
  )
