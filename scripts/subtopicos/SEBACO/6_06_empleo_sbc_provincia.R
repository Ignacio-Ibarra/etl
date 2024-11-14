################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "06_empleo_sbc_provincia"
analista = "Nicolas Sidicaro"
fuente1 <- "R247C140"

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R"
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

#readr::read_csv(argendataR::get_temp_path("RXXCX"))

get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}

## traigo la data
data_final <- arrow::read_parquet(get_clean_path(fuente1)) %>%
  separate(Trim, into = c("trim", "anio"), sep = " Trim ") %>%
  mutate(
    trim = as.numeric(gsub("°", "", trim)),  # Convierte el trimestre a numérico
    anio = as.numeric(anio)                  # Convierte el año a numérico
  )

#-- ParameTrim#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

# Filtrar por CIIUs de interes 
data_final <- data_final %>% 
  filter(Ramas %in% c('2213',
                      '3530',
                      '7210','7220','7230','7240','7290',
                      '7300',
                      '7410','7421','7430','7491','7494',
                      '9211'
                      #,'7499' # Se agregan "Servicios empresariales ncp, pese a que pueden tomar actividades que no estan dentro de SBC
  ))

data_final <- data_final %>% 
  filter(!is.na(Empleo))

# Armar datos anuales 
data_final <- data_final %>% 
#  mutate(anio = str_extract(Trim,'[0-9]+$')) %>% 
  group_by(provincia,anio) %>% 
  summarize(Empleo = sum(Empleo)/4)

# Sacar 2023
data_final <- data_final %>% 
  filter(anio != 2023)

# Calcular proporción anual 
data_final <- data_final %>% 
  group_by(anio) %>% 
  mutate(prop = Empleo / sum(Empleo))

# Armar uno para ver evolucion
data_final2 <- data_final %>% 
  mutate(provincia = if_else(provincia %in% c('CABA','GBA','Córdoba','Santa Fe','Resto de Buenos Aires'),provincia,'Resto')) %>% 
  group_by(provincia,anio) %>% 
  summarize(Empleo = sum(Empleo))
data_final2 <- data_final2 %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(prop = Empleo / sum(Empleo))
data_final2$Empleo <- NULL
data_final$Empleo <- NULL 

##########################

df_output <- data_final 

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

## data anterior

df_anterior <- argendataR::descargar_output(nombre =output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  pk = c("provincia","anio"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    control = comparacion,
    subtopico = subtopico,
    fuentes = c("R247C140"),
    analista = analista,
    pk = c("anio","provincia"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "",
    nivel_agregacion = "provincia",
    etiquetas_indicadores = list("provincia"="Provincia en la que se encuentra el empleo",
                                 "anio"="Año de referencia",
                                 "prop"="Proporción del empleo de SBC explicado por cada provincia"),
    unidades = list("prop" ="proporcion")
  )

#temp_files <- list.files(temp_dir, full.names = TRUE)
#view(temp_files)
#list.files(tempdir())

