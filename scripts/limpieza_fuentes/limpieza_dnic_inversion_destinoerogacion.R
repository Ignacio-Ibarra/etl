#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 407
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


df_clean <- argendataR::get_raw_path(fuente_raw) %>% 
  read.csv(., sep = ";", fileEncoding = "iso-8859-1", 
           na.strings = "S/D") %>% 
  janitor::clean_names() %>% 
  rename(anio = ano) %>% 
  select(!matches("x.*")) %>% 
  mutate(across(
    .cols = -c(anio, tipo),
    .fns = ~ .x %>%
      str_replace_all("\\.", "") %>%
      str_replace(",", ".") %>%
      as.numeric()
  )) %>% 
  drop_na(anio) %>% 
  pivot_longer(!all_of(c("anio", "tipo")), 
               names_to = "destino", 
               values_to = "inversion_i_d") %>% 
  mutate(
    destino = case_when(
      destino == "total_erogaciones_corrientes_i_d" ~ "Total de erogaciones corrientes",
      destino == "erogaciones_personal_de_i_d" ~ "Erogaciones en personal",
      destino == "otras_erogaciones_corrientes" ~ "Otras erogaciones corrientes",
      destino == "total_erogaciones_de_capital_i_d" ~ "Total de erogaciones de capital",
      destino == "erogaciones_inmuebles_y_construciones_i_d" ~ "Erogaciones en inmuebles y construcciones",
      destino == "erogaciones_equipamiento_y_rodados_i_d" ~ "Erogaciones en equipamiento y rodados",
      destino == "otras_erogaciones_de_capital_i_d" ~ "Otras erogaciones de capital",
      TRUE ~ NA_character_
    ),
    tipo = case_when(
      tipo == "Empresa" ~ "Empresas",
      tipo == "ESFL" ~ "Entidades sin fines de lucro",
      tipo == "Pais" ~ "Nacional (total)",
      tipo == "OCT" ~ "Organismos públicos",
      tipo == "UPRI" ~ "Universidades privadas",
      tipo == "UPUB" ~ "Universidades públicas"
    ),
    unidad_medida = "millones de pesos corrientes"
  ) 
  


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean) 

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 258
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('tipo','anio','destino')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)