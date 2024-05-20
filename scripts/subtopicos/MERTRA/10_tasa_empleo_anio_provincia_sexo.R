################################################################################
##    Dataset: Tasa de empleo por género y provincia (franja etaria 18-65 años) ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MERTRA"
output_name <- "tasa_empleo_anio_provincia_sexo"
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

e_franja_sexo_prov <- ephtu_df %>% 
  filter(ch06>=18 & ch06<=65) %>%
  select(anio = ano4, ocupado, prov_desc, sexo = ch04, pondera)%>% 
  mutate(sexo = case_when(
    sexo == 1 ~ "Varon",
    sexo == 2 ~ "Mujer"
  )) 

e_franja_total_sexo <- e_franja_sexo_prov %>% 
  mutate(sexo = "Total") 

e_franja_total_prov <- e_franja_sexo_prov %>% 
  mutate(prov_desc = "Todas") 

e_franja_total_total <- e_franja_sexo_prov %>% 
  mutate(sexo = "Total",
         prov_desc = "Todas") 

df_output <- bind_rows(e_franja_sexo_prov, e_franja_total_sexo, e_franja_total_prov, e_franja_total_total) %>% 
  group_by(anio, prov_desc, sexo, ocupado) %>% 
  summarize(pondera = sum(pondera)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ocupado, values_from = pondera, values_fill = 0) %>% 
  mutate(tasa_empleo = ocupado / (no_ocupado + ocupado)) %>% 
  select(anio, provincia = prov_desc, sexo, tasa_empleo)
  
  


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("anio", "provincia","sexo"),
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
    pk = c("anio", "provincia", "sexo"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    etiquetas_indicadores = list("tasa_empleo" = "Ratio entre la cantidad de personas ocupadas y la cantidad de personas pertenecientes a la población económicamente activa"),
    unidades = list("tasa_empleo" = "unidades")
  )