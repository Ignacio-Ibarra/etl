# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 317
fuente_raw <- sprintf("R%sC0",id_fuente)


# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()



df_raw <- argendataR::get_raw_path(fuente_raw) %>% 
  readr::read_csv(.) 


source("scripts/utils/wto_timeseries_api.R")

paises_wto <- wto_timeseries_api.get_reporting_economies() %>% 
  dplyr::filter(!is.na(iso3A)) %>% 
  distinct(ccode = code, iso3 = iso3A)
  

geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  dplyr::filter(nivel_agregacion == 'pais') %>% 
  select(iso3 = codigo_fundar, country_name_es = desc_fundar)


diccionario_paises <- paises_wto %>% 
  inner_join(geonomenclador, join_by(iso3)) # filtro codigos que no son de países. 


df_clean <- df_raw %>% 
  right_join(., diccionario_paises, join_by( ReportingEconomyCode == ccode)) %>% # filtro codigos que no son de países.
  select(year = Year,
         m49_code = ReportingEconomyCode, 
         iso3,
         country_name_es,
         product_code = ProductOrSectorCode, 
         product_desc = ProductOrSector, 
         unit = Unit,
         value_flag = ValueFlag,
         value = Value) 
  

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("{titulo.raw} - Dataset limpio")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      nombre = clean_title,
#                      path_clean = clean_filename,
#                      script = code_name,
#                      descripcion = "Se sacan columnas demás, se cambian nombres de columnas, se filtra codigos de paises que no eran países c('EEC','CEM')"
#                      )

id_fuente_clean <- 186
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) 


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('year','iso3','product_code', 'unit')
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
