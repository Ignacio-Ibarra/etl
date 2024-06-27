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

