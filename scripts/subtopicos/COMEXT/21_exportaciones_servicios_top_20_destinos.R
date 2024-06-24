################################################################################
##                              Dataset: cambio_origenes_importacion          ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


output_name <- stringr::str_sub(string = code_name, start = 4, end = -3)


#-- Librerias ----


#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
INDEC <-  data.table::fread(get_temp_path("R95C0")) 

ISO <-  read_csv(get_temp_path("R158C0")) 


#-- Parametros Generales ----


#-- Procesamiento ----


# Nos quedamos con "Total servicios"
indec <- INDEC %>% filter(cabps2010 == "200") %>% 
# Dropeamos a Mundo
  filter(descripcion_pais != "Mundo") %>% 
# Nos quedamos con las exportaciones
  filter(operacion == "Crédito") %>% 
# Montos
  mutate(export_value = str_replace_all(millones_de_dolares, "\\.", ""), 
         export_value = str_replace_all(export_value, "\\,", "."), 
         export_value = as.numeric(export_value)) %>%
# Dropeamos observaciones con cero exportaciones o missing values
 filter(!is.na(export_value) & export_value != 0)



indec <- indec %>%
  left_join(ISO %>% rename(codigo_pais =Alpha2code)) %>%
  mutate(iso3_oficial = !is.na(Alpha3code))

# Mostrar frecuencias de descripciónpaís si iso3_oficial es 0
indec %>%
  filter(iso3_oficial == FALSE) %>%
  count(descripcion_pais) %>%
  print()


# Renombramos y ordenamos
base <- indec %>%
  rename(year = ano) %>%
  select(year, codigo_pais, iso3c = Alpha3code , Englishshortname, 
         export_value, descripcion_pais, iso3_oficial) %>%
  arrange(year, codigo_pais)



base_rank <- base %>% 
  group_by(year, iso3_oficial) %>% 
  mutate(value_rank = rank(-export_value, ties.method = "first"))  %>%
  mutate(
    #### ID rank TOP
    codigo_pais = if_else(value_rank > 20 | iso3_oficial == FALSE, "", codigo_pais),
    descripcion_pais = if_else(value_rank > 20 | iso3_oficial == FALSE, "Resto", descripcion_pais),
    iso3c = if_else(value_rank > 20 | iso3_oficial == FALSE, "", iso3c),
    Englishshortname = if_else(value_rank > 20 | iso3_oficial == FALSE, "", Englishshortname)
  ) 




# RANKING TOP 20 DESTINOS
# Calculamos rankings de países por año



output <- base %>%
  group_by(year) %>%
#  arrange(year, desc(export_value)) %>% 
  mutate(
    value_rank = if_else(iso3_oficial == 1, rank(-export_value, ties.method = "first", na.last = "keep"), NA_real_),
    #### ID rank TOP
    codigo_pais = if_else(value_rank > 20 | is.na(iso3c), "", codigo_pais),
    descripcion_pais = if_else(value_rank > 20 | is.na(iso3c), "Resto", descripcion_pais),
    iso3c = if_else(value_rank > 20 | is.na(iso3c), "", iso3c),
    Englishshortname = if_else(value_rank > 20 | is.na(iso3_oficial), "", Englishshortname)
  ) %>%
  ungroup()







df_output <-  base_rank %>%
    # Colapsamos
      group_by(year, codigo_pais, descripcion_pais, iso3c, Englishshortname) %>%
      summarise(export_value = sum(export_value, na.rm = TRUE)) %>%
    # Ordenamos
      arrange(year, descripcion_pais) %>%
    # Calculamos shares y etiquetamos
      group_by(year) %>%
      mutate(export_value_pc = export_value / sum(export_value) * 100) %>%
      ungroup() %>% 
      select(year, iso3 = iso3c, descripcionpais = descripcion_pais, export_value_pc)







#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- descargar_output(nombre = output_name, subtopico = "COMEXT", entrega_subtopico = "datasets_primera_entrega") 


comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            pk = c( "descripcionpais", "year"))


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(directorio = 'data/COMEXT/',
                           output_name = output_name,
                           subtopico = subtopico,
                           fuentes = c("R158C0"),
                           analista = analista,
                           pk = c("descripcionpais", "year"),
                           es_serie_tiempo = FALSE,
                           columna_indice_tiempo = FALSE,
                           columna_geo_referencia = "iso3",
                           nivel_agregacion = "continente (region)",
                           etiquetas_indicadores = list("export_value_pc" = "Exportaciones de servicios (% del total exportado en servicios)",
                                                        "export_value_pc" = "Exportaciones de servicios (% del total exportado en servicios)"),
                           unidades = list("export_value_pc" = "porcentaje"),
                           aclaraciones =  ''
  )


