# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 245
fuente_raw <- sprintf("R%sC0",id_fuente)

library(rsdmx)
df <- rsdmx::readSDMX(argendataR::get_raw_path(fuente_raw), isURL = F)
df_clean <- as.data.frame(df) %>% 
  mutate(OBS_VALUE = as.numeric(OBS_VALUE))

# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


clean_title <- glue::glue("{titulo.raw} - Dataset limpio")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      nombre = clean_title,
#                      path_clean = clean_filename,
#                      script = code_name,
#                      )

id_fuente_clean <- 133
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) %>% 
  mutate(OBS_VALUE = as.numeric(OBS_VALUE))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('COUNTERPART_AREA',
                                            'REF_SECTOR',
                                            'COUNTERPART_SECTOR',
                                            'ACCOUNTING_ENTRY',
                                            'INT_ACC_ITEM',
                                            'FUNCTIONAL_CAT',
                                            'INSTR_ASSET',
                                            'MATURITY',
                                            'CURRENCY_DENOM',
                                            'VALUATION',
                                            'COMP_METHOD',
                                            'TIME_PERIOD')
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)



