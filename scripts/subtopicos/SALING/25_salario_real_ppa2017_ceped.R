################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SALING"
output_name <- "salario_real_ppa2017_ceped"
fuente_ceped <- "R209C0"
fuente_cgi_rta <- 'R35C83'
fuente_cgi_puestos_ar <- 'R35C84'
fuente_cgi_puestos_anr <- 'R35C85'
fuente_ipc <- 'R127C54'

# Del dataset descargado nos quedamos únicamente con 
# las filas donde 'nombre.variable' == Salario real en dólares 
# de paridad de poder adquisitivo de 2017 (PPA de consumo privado) 
# y las columnas iso3c, nombre.país, ANO4, valor
ceped.df <- readxl::read_excel(argendataR::get_fuente_path(fuente_ceped)) %>% 
  dplyr::filter(nombre.variable == "Salario real en dólares de paridad de poder adquisitivo de 2017 (PPA de consumo privado)") %>% 
  select(iso3 = iso3c, anio = ANO4, salario_real_ppa_consumo_privado_2017 = valor)

anio_ult_ceped <- max(ceped.df$anio)
valor_ult_ceped <- ceped.df[(ceped.df$anio == anio_ult_ceped) & (ceped.df$iso3 == "ARG"),  c("salario_real_ppa_consumo_privado_2017")][[1]]

                
# Tomamos el dato Total general/Total
rta.df <- arrow::read_parquet(argendataR::get_fuente_path(fuente_cgi_rta)) %>% 
  dplyr::filter(trim == "Total") %>% 
  dplyr::filter(indicador == "Total general") %>% 
  select(-trim,-indicador)

# Tomamos el dato Total general/Total 
puestos_ar.df <- arrow::read_parquet(argendataR::get_fuente_path(fuente_cgi_puestos_ar)) %>% 
  dplyr::filter(trim == "Total") %>% 
  dplyr::filter(indicador == "Total general") %>% 
  select(-trim,-indicador)

# # Tomamos el dato Total general/Total
puestos_anr.df <- arrow::read_parquet(argendataR::get_fuente_path(fuente_cgi_puestos_anr)) %>% 
  dplyr::filter(trim == "Total") %>% 
  dplyr::filter(indicador == "Total general") %>% 
  select(-trim,-indicador)

cgi_jn <- left_join(rta.df, puestos_ar.df,by = join_by(anio)) %>% 
  left_join(puestos_anr.df, by= join_by(anio)) %>% 
  # vab en millones  y puestos en miles, multiplico por mil para que me quede el cociente en unidades
  mutate(salario_medio_nominal = valor_agregado_bruto * 1000 / (puestos_ar + puestos_anr) ) %>% 
  select(anio, salario_medio_nominal)

ipc.df <- arrow::read_parquet(argendataR::get_fuente_path(fuente_ipc)) %>% 
  dplyr::filter(region == "Nacional") %>% 
  dplyr::filter(descripcion == "Nivel general") %>% 
  group_by(anio) %>% 
  summarise(indice_ipc = mean(indice_ipc, na.rm = T)) %>% 
  ungroup()

salario_real_indec <- left_join(cgi_jn, ipc.df, by=join_by(anio)) %>% 
  mutate(
    salario_medio_real = salario_medio_nominal / indice_ipc, 
    tasas = salario_medio_real /lag(salario_medio_real)
    ) %>% 
  dplyr::filter(anio > anio_ult_ceped)


hacer_empalme <- function(tasas, valor_inicial){
  result <- valor_inicial
  resultados <- c()
  for (t in tasas){
    result = result * t
    resultados <- c(resultados, result)
  }
  return(resultados)
}


filas_nuevas <- salario_real_indec %>% 
  select(anio, tasas) %>% 
  mutate(salario_real_ppa_consumo_privado_2017 = hacer_empalme(tasas, valor_inicial = valor_ult_ceped)) %>% 
  mutate(iso3 = "ARG") %>% 
  select(-tasas)


df_output <- bind_rows(ceped.df, filas_nuevas)


# El dataset del analista tenía filas erróneas que se sacaron para poder hacer correctamente la comparacion
df_anterior <- argendataR::descargar_output(nombre = output_name, 
                                            subtopico = subtopico) %>% 
  group_by(iso3, anio) %>% 
  mutate(row_id = row_number()) %>% 
  dplyr::filter(row_id == 1) %>% 
  select(-row_id) %>% 
  ungroup()


comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  pk = c('iso3','anio'), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

print(comparacion)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_ceped, fuente_cgi_puestos_anr, fuente_cgi_puestos_ar, fuente_cgi_rta, fuente_ipc),
    analista = "",
    control = comparacion, 
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "paises",
    aclaraciones = "El dataset del analista tenía filas erróneas (que estaban presentes en la fuente de información) que se sacaron para poder hacer correctamente la comparacion",
    etiquetas_indicadores = list("salario_real_ppa_consumo_privado_2017" = "Salario real en dólares de paridad de poder adquisitivo de 2017 (PPA de consumo privado)"),
    unidades = list("salario_real_ppa_consumo_privado_2017" = "unidades")
  )

