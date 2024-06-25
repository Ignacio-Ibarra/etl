#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)
subtopico <- 'DESIGU'
output_name <- 'ISA_funcional_i1'


fuente_1 <- "R210C0"
fuente_2 <- "R35C76"
fuente_3 <- "R211C77"
# nombre_archivo_raw1 <- str_split_1(fuentes_raw() %>% 
#                                     filter(codigo == fuente_raw1) %>% 
#                                     select(path_raw) %>% 
#                                     pull(), pattern = "\\.")[1]


# Participación en el Valor Agregado Bruto a precios básicos (por sector o total de la economía)
ceped_df <- readxl::read_excel(argendataR::get_temp_path(fuente_1)) %>% 
  dplyr::filter(variable == "particip.vab.pb") %>% 
  select(anio = Anio, particip_vab_pb_total = Total)
  
# Cuenta Generacion del Ingeso (RTA pp) - INDEC
cgi_df <- read_csv(argendataR::get_temp_path(fuente_2)) %>% 
  dplyr::filter(trim == "Total") %>% 
  dplyr::filter(indicador == "Total general") %>% 
  select(anio, participacion)

grania_df <- read_csv(argendataR::get_temp_path(fuente_3)) %>% select(anio, masa_salarial)


data_total <- grania_df %>% 
  full_join(., ceped_df, by=join_by(anio)) %>% 
  full_join(., cgi_df, by=join_by(anio))

#-- Lectura de Datos ----



#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- proceso

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("var1", "var2"), # variables pk del dataset para hacer el join entre bases
  drop_output_drive = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
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

