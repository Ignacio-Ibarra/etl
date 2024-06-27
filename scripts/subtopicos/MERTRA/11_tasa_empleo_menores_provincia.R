################################################################################
##    Dataset: Comparativa entre tasa de empleo y participación de menores    ##
##    de 18 años en la población, por año y provincia                         ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MERTRA"
output_name <- "tasa_empleo_menores_provincia"
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
  select(anio = ano4, ocupado, prov_desc, edad = ch06, pondera) 


X <- base %>% dplyr::filter(edad == -1)

empleo_prov <- base %>% 
  select(!edad)
  
empleo_total_prov <- empleo_prov %>% 
  mutate(prov_desc = "Total") 

A <- bind_rows(empleo_prov, empleo_total_prov) %>% 
  group_by(anio, prov_desc, ocupado) %>% 
  summarize(pondera = sum(pondera)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ocupado, values_from = pondera, values_fill = 0) %>% 
  mutate(tasa_empleo = ocupado / (no_ocupado + ocupado)) %>% 
  select(anio, prov_desc, tasa_empleo)
  
pob_menor_prov <- base %>% 
  dplyr::filter(edad < 18) %>% 
  select(-edad, -ocupado) %>% 
  group_by(anio, prov_desc) %>% 
  summarize(pob_menor = sum(pondera))

pob_menor_total <- base %>% 
  dplyr::filter(edad < 18) %>% 
  mutate(prov_desc = "Total") %>% 
  group_by(anio, prov_desc) %>% 
  summarize(pob_menor = sum(pondera))

pob_tot_prov <- base %>% 
  group_by(anio, prov_desc) %>% 
  summarize(pob_tot = sum(pondera)) %>% 
  ungroup()

pob_tot_tot <- base %>% 
  mutate(prov_desc = "Total") %>% 
  group_by(anio, prov_desc) %>% 
  summarize(pob_tot = sum(pondera)) %>% 
  ungroup()

pob.menor <- bind_rows(pob_menor_prov, pob_menor_total)
pob.tot <- bind_rows(pob_tot_prov, pob_tot_tot)

B <- left_join(pob.menor, pob.tot, by=join_by(anio, prov_desc)) %>% 
  mutate(tasa_menor_18 = pob_menor/pob_tot) %>% 
  select(anio, prov_desc, tasa_menor_18)



df_output <- inner_join(A, B, by=join_by(anio, prov_desc)) %>% 
  rename(provincia = prov_desc) %>% 
  mutate(provincia = ifelse(provincia == "CABA", "Ciudad de Buenos Aires",provincia))


df_anterior <- argendataR::descargar_output(nombre = output_name, 
                                            subtopico = subtopico,
                                            entrega_subtopico = "datasets_primera_entrega")

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

# Las diferencias con el output anterior radican en que CABA era antes Ciudad de Buenos Aires, debido a cambios en el nomenclador usado. 
comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior = df_anterior,
  pk = c("anio", "provincia"),
  drop_joined_df =  F
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
    pk = c("anio", "provincia"),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    etiquetas_indicadores = list("tasa_empleo" = "Ratio entre la cantidad de personas ocupadas y la cantidad de personas pertenecientes a la población económicamente activa",
                                 "tasa_menor_18" = "Porcentaje de la población que es menor a 18 años") ,
    unidades = list("tasa_empleo" = "unidades",
                    "tasa_menor_18" = "unidades")
  )
