# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tasa_global_fecundidad_provincias.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R437C282' # DEIS Nacidos Vivos (2005-2023)
fuente2 <- 'R439C284' # INDEC Población por sexo y grupos quinquenales de edad para el total del país y provincias. Años 2010-2040

df_deis <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_indec <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_nac_edades <- df_deis %>% 
  dplyr::filter(!(provres %in% c(98,99)), 
                !grepl("Sin especificar", imedad)) %>% 
  mutate(rango_etario_id = as.integer(str_extract(imedad, "(.*)\\.(.*)", group=1))) %>% 
  group_by(provres, prov_desc, anio, rango_etario = imedad, rango_etario_id) %>% 
  summarise(nacimientos = sum(cuenta, na.rm=T)) %>% 
  ungroup() %>% 
  select(provincia_id = provres, anio, rango_etario_id, nacimientos)
  

df_pob_fem_edades <- df_indec %>% 
  dplyr::filter(sexo == "Mujeres") %>%
  mutate(
    edad_ultima = ifelse(edad == "100 y más", 100, as.numeric(str_extract(edad, "(?<=-)[0-9]+")))) %>% 
  mutate(
    rango_etario_id = case_when(
      edad_ultima < 15 ~ 1,
      edad_ultima == 19 ~ 2,
      edad_ultima == 24 ~ 3,
      edad_ultima == 29 ~ 4,
      edad_ultima == 34 ~ 5,
      edad_ultima == 39 ~ 6,
      edad_ultima == 44 ~ 7,
      edad_ultima>45 ~ 8
    )
  ) %>% 
  group_by(anio, provincia_id, provincia, rango_etario_id) %>% 
  summarise(pob_fem = sum(poblacion_proyectada)) %>% 
  ungroup()


df_output <- df_pob_fem_edades %>% 
  inner_join(df_nac_edades, join_by(provincia_id, anio, rango_etario_id)) %>% 
  group_by(anio, provincia_id, provincia) %>% 
  summarise(
    tgf = 5*sum(nacimientos/pob_fem)
  ) %>% 
  ungroup()
  

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico)

pks <- c('anio', 'provincia_id')

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = pks
)


armador_descripcion <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){
  # metadatos: data.frame sus columnas son variable_nombre y descripcion y 
  # proviene de la info declarada por el analista 
  # etiquetas_nuevas: data.frame, tiene que ser una dataframe con la columna 
  # variable_nombre y la descripcion
  # output_cols: vector, tiene las columnas del dataset que se quiere escribir
  
  etiquetas <- metadatos %>% 
    dplyr::filter(variable_nombre %in% output_cols) 
  
  
  etiquetas <- etiquetas %>% 
    bind_rows(etiquetas_nuevas)
  
  
  diff <- setdiff(output_cols, etiquetas$variable_nombre)
  
  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)
  
  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva. 
  
  etiquetas <- etiquetas %>% 
    group_by(variable_nombre) %>% 
    filter(if(n() == 1) row_number() == 1 else row_number() == n()) %>%
    ungroup()
  
  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)
  
  return(etiquetas)
  
}

# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name), nombre_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output



descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = pks,
    descripcion_columnas = descripcion, 
    unidad = list("tgf" = "unidades por mil"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")

