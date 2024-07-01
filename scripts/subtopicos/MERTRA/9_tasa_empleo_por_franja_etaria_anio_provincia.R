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
output_name <- "tasa_empleo_por_franja_etaria_anio_provincia"
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

d_franjas_edad_prov <- ephtu_df %>% 
  select(anio = ano4, ocupado, prov_desc, edad = ch06, pondera)%>% 
  mutate(apertura_edad = case_when(
    edad < 18 ~ "Edad, menor de 18",
    edad >= 18 & edad <=65 ~ "Edad, entre 18 y 65",
    TRUE ~ "Edad, mayor a 65"
  )) %>%
  select(anio, prov_desc, apertura_edad, ocupado, pondera)

d_franja_total_edades <- d_franjas_edad_prov %>% 
  mutate(apertura_edad = "Edad, total") 

d_franja_total_prov <- d_franjas_edad_prov %>% 
  mutate(prov_desc = "Total") 

d_franja_total_total <- d_franjas_edad_prov %>% 
  mutate(apertura_edad = "Edad, total",
         prov_desc = "Total") 

df_output <- bind_rows(d_franjas_edad_prov, d_franja_total_edades, d_franja_total_prov, d_franja_total_total) %>% 
  group_by(anio, prov_desc, apertura_edad, ocupado) %>% 
  summarize(pondera = sum(pondera)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ocupado, values_from = pondera, values_fill = 0) %>% 
  mutate(tasa_empleo = ocupado / (no_ocupado + ocupado)) %>% 
  select(anio, prov_desc, apertura_edad, tasa_empleo)




#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "datasets_primera_entrega")

comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            nombre = output_name,   pk = c("anio", "prov_desc","apertura_edad"))


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    control = comparacion,
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1, fuente2),
    analista = "",
    pk = c("anio", "prov_desc", "apertura_edad"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    etiquetas_indicadores = list("tasa_empleo" = "Ratio entre la cantidad de personas ocupadas y la cantidad de personas pertenecientes a la población económicamente activa"),
    unidades = list("tasa_empleo" = "unidades")
  )
