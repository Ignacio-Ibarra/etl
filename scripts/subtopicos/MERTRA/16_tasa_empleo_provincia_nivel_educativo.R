################################################################################
##    Dataset: Tasa de empleo en población de 25 a 65 años,                   ##
##             por nivel educativo, provincia y año                           ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MERTRA"
output_name <- "tasa_empleo_provincia_nivel_educativo"
fuente1 <- "R49C16"  
fuente2 <- "R84C14"


#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
ephtu_df <- readr::read_csv(argendataR::get_temp_path(fuente1))
# ephtu_df <- ephtu_df %>% rename_with(tolower, everything()) #esta linea no haría falta que esté cuando cambiemos el input de fuente1 por la fuente clean. 

codigos <- readr::read_csv(argendataR::get_temp_path(fuente2))
codigos <- codigos %>% select(aglomerado = aglom_cod_indec, provincia = prov_cod, prov_desc)

#-- Procesamiento ----

ephtu_df <- ephtu_df %>% 
  left_join(codigos, by = join_by(aglomerado, provincia)) # Joineo así por los casos en que hay mismo aglomerado pero distinta provincia e.g. San Nicolás-Villa Constitucion

ephtu_df <- ephtu_df %>% mutate(
  # activo = case_when(
  #   estado == 1 | estado == 2 ~ 'activo',
  #   TRUE ~ 'no_activo'
  # ),
  ocupado = case_when(
    estado == 1 ~ 'ocupado',
    TRUE ~ 'no_ocupado'
  )
)

base <- ephtu_df %>% 
  select(anio = ano4, ocupado, provincia = prov_desc, edad = ch06, nivel_ed, pondera) %>% 
  dplyr::filter(edad>24 & edad<66) %>% 
  mutate(nivel_ed_fundar = case_when(
    nivel_ed == 4 ~ "Secundario completo",
    nivel_ed %in% c(5,6) ~ "Superior incompleto o completo",
    TRUE ~ "Hasta secundario incompleto"
  )) %>% 
  select(anio, provincia, ocupado, nivel_ed_fundar, pondera)

# Tasa empleo por nivel_ed_fundar por provincia
empleo_prov_ed <- base %>% 
  group_by(anio, provincia, nivel_ed_fundar, ocupado) %>% 
  summarize(pondera = sum(pondera)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ocupado, values_from = pondera, values_fill = 0) %>% 
  mutate(tasa_empleo = ocupado / (no_ocupado + ocupado)) %>% 
  select(anio, provincia, nivel_ed_fundar, ocupado = tasa_empleo) #en el output original viene así. 

empleo_prov_total <- base %>%
  mutate(nivel_ed_fundar = "Total") %>%
  group_by(anio, provincia, nivel_ed_fundar, ocupado) %>% 
  summarize(pondera = sum(pondera)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ocupado, values_from = pondera, values_fill = 0) %>% 
  mutate(tasa_empleo = ocupado / (no_ocupado + ocupado)) %>% 
  select(anio, provincia, nivel_ed_fundar, ocupado = tasa_empleo)

df_output <- bind_rows(empleo_prov_ed, empleo_prov_total)



#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

# Las diferencias con el output anterior radican en que CABA era antes Ciudad de Buenos Aires, debido a cambios en el nomenclador usado. 
comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("anio", "provincia", "nivel_ed_fundar"),
  drop_output_drive = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1, fuente2),
    analista = "",
    pk = c("anio", "provincia", "nivel_ed_fundar"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    etiquetas_indicadores = list("ocupado" = "Ratio entre la cantidad de personas ocupadas y la cantidad de personas pertenecientes a la población económicamente activa"),
    unidades = list("ocupado" = "unidades")
  )