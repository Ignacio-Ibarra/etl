################################################################################
##    Dataset: Comparativa entre la cantidad de establecimientos de trabajo   ##
##    cada 1000 habitantes y la tasa de empleo en población entre 18 y 65     ##
##    años, por provincia, para el año 2022.                                  ##  
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MERTRA"
output_name <- "establecimientos_tasa_empleo_provincia_2021_22"
fuente1 <- "./data/_FUENTES/raw/distribucion_establecimientos_productivos_sexo.csv" # corregir esto cuando se pueda cargar la data raw en fuentes_raw 
fuente2 <- "R99C25"
fuente3 <- "R49C16" 
fuente4 <- "R84C14"


#-- Lectura de Datos ----

# fuente CEP
establ_df <- readr::read_csv(fuente1) # corregir esto. 

# Fuente Censo
censo_df <- readr::read_csv(argendataR::get_temp_path(fuente2)) %>% select(jurisdiccion, total_de_poblacion) #esta linea no haría falta que esté cuando cambiemos el input de fuente1 por la fuente clean. 
censo_df$jurisdiccion[25]<- "Tierra del Fuego, Antártida e Islas del Atlántico Sur"

# Diccionario Provincias CEP
deptos <- readr::read_csv("https://cdn.produccion.gob.ar/cdn-cep/establecimientos-productivos/codigo_departamento_provincia.csv")
prov_dicc <- deptos %>% select(provincia_id, provincia) %>% distinct(.)

# Datos EPH Total Urbano 
ephtu_df <- readr::read_csv(argendataR::get_temp_path(fuente3))

# codigos de aglomerados INDEC
codigos <- readr::read_csv(argendataR::get_temp_path(fuente4))
codigos <- codigos %>% select(aglomerado = aglom_cod_indec, provincia = prov_cod, prov_desc)

#-- Procesamiento ----

normalizo_prov <- function(nombre){
  
  nombre_nuevo_df <- geoAr::get_provincias(nombre = nombre)
  nombre_nuevo <- nombre
  if (nrow(nombre_nuevo_df)>0){
    nombre_nuevo <- nombre_nuevo_df$nombre[1]
  }
  cat("Nombre a revisar: ", nombre, "Nombre nuevo: ", nombre_nuevo, "\n")
  return(nombre_nuevo)
}

# Normalizo nombres de provincias
censo_df <- censo_df %>% 
  mutate(provincia_geoar = purrr::map_chr(jurisdiccion, normalizo_prov)) %>% 
  select(provincia_geoar, pob = total_de_poblacion)


# Cuento la cantidad de establecimientos por provincias para anio == 2022
establ_df_gr <- establ_df %>% 
  dplyr::filter(anio == 2022) %>% 
  group_by(provincia_id) %>% 
  summarise(cantidad_establecimientos = n()) %>%
  left_join(., prov_dicc, by=join_by(provincia_id)) %>% 
  mutate(provincia_geoar = purrr::map_chr(provincia, normalizo_prov))

# Me traigo la población de cada provincia y hago el cociente. 
establ_df_gr <- establ_df_gr %>%
  left_join(., censo_df, by=join_by(provincia_geoar)) %>% 
  mutate(establecimientos_cada_1000_hab_2021_22 = 1000 * cantidad_establecimientos / pob)


# Calculo la tasa de empleo  
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

data_prov <- ephtu_df %>%
  dplyr::filter(ano4 == 2022) %>% 
  dplyr::filter(ch06>=18 & ch06<=65) %>% 
  select(ocupado, prov_cod = provincia, prov_desc, pondera) %>% 
  group_by(prov_cod, prov_desc, ocupado) %>% 
  summarize(pondera = sum(pondera)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = ocupado, values_from = pondera, values_fill = 0) %>%
  mutate(tasa_empleo_18_65_2022 = ocupado / (no_ocupado + ocupado)) %>% 
  select(prov_cod, prov_desc, tasa_empleo_18_65_2022)


df_output <- left_join(establ_df_gr, data_prov, by=join_by(provincia_id == prov_cod)) %>% 
  select(provincia_id, provincia_desc = prov_desc, establecimientos_cada_1000_hab_2021_22, tasa_empleo_18_65_2022)


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("provincia_id"),
  drop_output_drive = F
)

#-- Exportar Output ---- (OJO MODIFICO NOMBRES DE OUTPUTSSS)

# cambio nombre archivo
output_name_nuevo <- "establecimientos_tasa_empleo_provincia"
output_final <- df_output %>% rename(c("tasa_empleo_18_65" = "tasa_empleo_18_65_2022", "establecimientos_cada_1000_hab"="establecimientos_cada_1000_hab_2021_22"))

path <- glue::glue("{tempdir()}/{output_name_nuevo}.csv")

output_final %>% write_csv_fundar(.,path)
# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name_nuevo,
    subtopico = subtopico,
    fuentes = c(fuente1, fuente2, fuente3, fuente4),
    analista = "",
    pk = c("provincia_id"),
    es_serie_tiempo = F,
    etiquetas_indicadores = list("tasa_empleo_18_65" = "Ratio entre la cantidad de personas ocupadas y la cantidad de personas pertenecientes a la población económicamente activa, en la franja de los 18 a los 65 años",
                                 "establecimientos_cada_1000_hab" = "Cantidad de establecimientos productivos cada 1000 habitantes"),
    unidades = list("tasa_empleo_18_65" = "unidades",
                    "establecimientos_cada_1000_hab" = "unidades")
  )