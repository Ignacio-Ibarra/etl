################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "precios_relativos_caba_empalmada.csv"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
ipcba <- readr::read_csv(argendataR::get_temp_path("R136C64"))

ipcba_empalme <- readr::read_csv(argendataR::get_temp_path("R135C63"))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df <- bind_rows(ipcba_empalme %>% 
                  filter(fecha < lubridate::ym("2022-02")),
                ipcba)

anios_excl <- df %>% 
  group_by(anio = year(fecha), apertura) %>% 
  mutate(n = n()) %>% 
  filter(n != 12) %>% 
  ungroup() %>% 
  pull(anio) %>% unique()

df <- df %>% 
  filter(! year(fecha) %in% anios_excl)

df <- df %>%
  group_by(anio = year(fecha), codigo, apertura, nivel) %>%
  summarize(ipc = mean(indice, na.rm = TRUE)) %>% 
  ungroup()

df <- df %>%
  group_by(codigo) %>%
  mutate(ipc_2013 = ipc[anio == 2013]) %>%
  ungroup() %>%
  mutate(ipc_base2013 = ipc / ipc_2013 * 100) %>%
  select(anio, codigo, nivel, apertura, ipc_base2013) %>%
  rename(ipc = ipc_base2013)

df <- df %>%
  group_by(anio) %>% 
  mutate(nivelgeneral = ipc[codigo == "0"]) %>% 
  ungroup()

# Calcular precios relativos
df <- df %>%
  mutate(precio_relativo = ipc / nivelgeneral * 100) %>%
  filter(codigo != "0") %>%
  select(-c(nivelgeneral, ipc))

# Ordenar y exportar datos
df_output <- df %>%
  arrange(anio, codigo) %>% 
  rename(rubro = apertura)


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  subtopico = "PRECIO", entrega_subtopico = "datasets_update",
  pk = c("anio", "codigo", "nivel"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "PRECIO",
    fuentes = c("R136C64", "R135C63"),
    analista = "",
    pk = c("anio", "codigo", "rubro", "nivel"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    # columna_geo_referencia = "iso3",
    # nivel_agregacion = "pais",
    etiquetas_indicadores = list("precio_relativo" = "Nivel de precio relativo respecto del nivel general"),
    unidades = list("precio_relativo" = "indice")
  )

