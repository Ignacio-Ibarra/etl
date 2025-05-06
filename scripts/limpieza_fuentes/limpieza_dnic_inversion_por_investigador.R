#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 402
fuente_raw <- sprintf("R%sC0",id_fuente)

pattern <- fuentes_raw() %>% 
  pull(path_raw) %>% 
  tools::file_ext(.) %>%
  unique() %>% 
  keep(., ~all(.x != '')) %>% 
  paste0(., collapse = "|") %>% 
  paste0("(.*)\\.(",.,")$")


nombre_archivo_raw <- str_extract(fuentes_raw() %>% 
                                    dplyr::filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), 
                                  pattern = pattern, 
                                  group = 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


df_clean <- argendataR::get_raw_path(fuente_raw) %>% 
  read.csv(., sep = ";", fileEncoding = "iso-8859-1") %>% 
  rename(anio = `Año`) %>% 
  drop_na(anio) %>% 
  pivot_longer(-anio,
               values_to = "inversion_i_d",
               values_transform = as.numeric) %>% 
  mutate(
    tipo_equivalencia = ifelse(grepl("EJC", name), "EJC", "Persona física"),
    unidad_medida = case_when(
      grepl("pesos.corrientes", name) ~ "Pesos corrientes",
      grepl("pesos.constantes", name) ~ "Pesos constantes",
      grepl("dólares.corrientes", name) ~ "Dólares corrientes",
      grepl("Paridad.de.Poder.de.Compra", name) ~ "Dólares (PPP)"
    )
  ) %>% 
  select(anio, tipo_equivalencia, unidad_medida, inversion_i_d)



clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean) 

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 253
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('anio', 'tipo_equivalencia','unidad_medida')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)