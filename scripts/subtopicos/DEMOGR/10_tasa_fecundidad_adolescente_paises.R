# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tasa_fecundidad_adolescente_paises.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R438C283' # World Population Proscpects - Fertility. 1950-2100, 5-year age groups. Age-specific Fertility Rate (ASFR). Percent Age-specific Fertility Rate (PASFR). Births (thousands). CSV format
fuente2 <- 'R435C280' # World Population Proscpects - Population
fuente3 <- 'R437C282' # DEIS Nacidos Vivos (2005-2023)

df_wpp_fertility <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_wpp_pob <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_deis <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.)

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_wpp_nacimientos_adolescente <-df_wpp_fertility %>% 
  dplyr::filter(!is.na(iso3_code),
                trimws(iso3_code) != '',
                variant == "Medium",
                age_grp %in% c("10-14","15-19")) %>% 
  group_by(anio = time, geocodigoFundar = iso3_code) %>% 
  summarise(nacimientos_adolescentes = sum(births, na.rm =T)*1000) %>% 
  left_join(geo_front, join_by(geocodigoFundar))%>% 
  dplyr::filter(anio <= (year(Sys.Date())))

rm(df_wpp_fertility)


df_pop_fem_adolescente <- df_wpp_pob %>% 
  dplyr::filter(!is.na(iso3_code),
                trimws(iso3_code) != '',
                variant == "Medium",
                age_grp %in% c("10-14","15-19")) %>% 
  group_by(anio = time, geocodigoFundar = iso3_code) %>% 
  summarise(pob_fem_adolescente = sum(pop_female, na.rm =T)*1000) 

rm(df_wpp_pob)



df_deis_nacimientos_edades_adolescentes_arg <- df_deis %>% 
  dplyr::filter(!grepl("Sin especificar", imedad)) %>% 
  mutate(rango_etario_id = as.integer(str_extract(imedad, "(.*)\\.(.*)", group=1))) %>% 
  dplyr::filter(rango_etario_id %in% 1:2) %>% 
  group_by(anio) %>% 
  summarise(nacimientos = sum(cuenta, na.rm=T)) %>% 
  ungroup() 

df_deis_tgf_adolescente <- df_deis_nacimientos_edades_adolescentes_arg %>% 
  left_join(df_pop_fem_adolescente %>% 
              dplyr::filter(geocodigoFundar == "ARG"),
            join_by(anio)) %>% 
  mutate(tgf = 1000 * nacimientos / pob_fem_adolescente, 
         fuente = "Dirección de Estadísticas e Información en Salud (DEIS)") %>% 
  select(anio, geocodigoFundar, tgf, fuente)


df_wpp_tgf_adolescente <- df_wpp_nacimientos_adolescente %>% 
  left_join(df_pop_fem_adolescente, join_by(anio, geocodigoFundar)) %>% 
  mutate(
    tgf = 1000* nacimientos_adolescentes / pob_fem_adolescente,
    fuente =  "World Population Prospects (UN)"
  ) %>% 
  select(anio, geocodigoFundar, tgf, fuente)


df_output <- df_deis_tgf_adolescente %>% 
  bind_rows(df_wpp_tgf_adolescente) %>% 
  distinct(anio, geocodigoFundar, .keep_all = T) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  select(anio, geocodigoFundar, geonombreFundar, tgf_adolescente = tgf, fuente) %>% 
  arrange(anio, geocodigoFundar) %>% 
  dplyr::filter(anio <= max(df_deis_tgf_adolescente$anio))


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico)

pks <- c('anio', 'geocodigoFundar')

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
    unidad = list("tgf_adolescente" = "unidades por mil"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")
