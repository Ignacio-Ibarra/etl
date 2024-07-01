################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "INFDES"
output_name <- "composicion_empleo_ultimo_anio_latam"
fuente1 <- "R115C39"



#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
stru_type_df <- readr::read_csv(argendataR::get_temp_path(fuente1))

#-- Procesamiento ----

reemplazos <- c(
  "Categoría laboral, Formal, Tipo de empresa, Emprendedores" = "Empleadores",
  "Categoría laboral, Formal, Trabajadores asalariados, Grandes empresas" = "Asalariados en pequeñas y grandes empresas",
  "Categoría laboral, Formal, Trabajadores asalariados, Sector público" = "Asalariados públicos",
  "Categoría laboral, Formal, Autónomos, profesionales" = "Cuentapropistas profesionales",
  "Categoría laboral, Informal, Asalariado, Pequeñas empresas" = "Asalariados en microempresas",
  "Categoría laboral, Informal, Autónomos, No Calificados" = "Cuentapropistas sin calificación",
  "Categoría laboral, Informal, Trabajadores con ingresos cero" = "Trabajadores sin ingresos"
)

cod_cat_ocup <- c(
  "Empleadores" = 1,
  "Asalariados en pequeñas y grandes empresas" = 2,
  "Asalariados públicos" = 3,
  "Cuentapropistas profesionales" = 4,
  "Asalariados en microempresas" = 5,
  "Cuentapropistas sin calificación" = 6,
  "Trabajadores sin ingresos" = 7
)

df_procesamiento <- stru_type_df %>% 
  dplyr::filter(grepl("Categoría*", apertura)) %>% 
  dplyr::filter(serie == "Serie original") %>% 
  mutate(
    formal_def_productiva = ifelse(grepl("*Formal*", apertura), "Formal", "Informal"),
    cat_ocup = ifelse(grepl("Asalariado | asalariado", apertura), "Asalariados", "No asalariados"),
    cat_ocup_detalle = reemplazos[apertura],
    cat_ocup_cod = cod_cat_ocup[cat_ocup_detalle]
    )

df_detalle <- df_procesamiento %>% 
  group_by(pais, anio, fuente) %>% 
  mutate(selection1 = (sum(valor, na.rm = T)>99)) %>% 
  ungroup() %>% 
  group_by(pais) %>% 
  mutate(selection2 = (max(anio, na.rm=T) == anio) & (anio>2015)) %>% 
  ungroup() %>% 
  dplyr::filter(selection1 & selection2) %>% 
  select(iso3, pais, anio, formal_def_productiva, cat_ocup_cod, cat_ocup_detalle, valor)
  

df_total <- df_detalle %>% 
  filter(formal_def_productiva == "Formal") %>% 
  group_by(iso3, pais, anio, formal_def_productiva) %>% 
  summarise(valor= sum(valor, na.rm = T)) %>% 
  mutate(cat_ocup_detalle = "Total formal")

df_output <- data.table::rbindlist(list(df_detalle, df_total), fill = T)


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("iso3", "anio","formal_def_productiva", "cat_ocup_detalle"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1),
    analista = "",
    pk = c("iso3", "anio","formal_def_productiva", "cat_ocup_detalle"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("valor" = "Distribución de trabajadores por tipo de relación laboral"),
    unidades = list("valor" = "porcentaje")
  )

