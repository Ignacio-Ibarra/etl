#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 420
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

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais_nombre = name_long)

cleaning_func <- function(sheet_name, filas_columnas, skip){
  
  str_title <- argendataR::get_raw_path(fuente_raw) %>% 
    readxl::read_excel(., sheet = sheet_name, col_names = F) %>% 
    slice(1) %>% 
    select(2) %>% 
    pull() 
  
   df_raw <- argendataR::get_raw_path(fuente_raw) %>% 
     readxl::read_excel(.,
                               skip = 2,
                               col_names = F,
                               sheet = sheet_name) 
  
   
   cols <- argendataR::get_raw_path(fuente_raw) %>% 
     readxl::read_excel(.,
                        col_names = F,
                        sheet = sheet_name) %>% 
     slice(filas_columnas) %>% t() %>% 
     purrr::keep(., ~all(!is.na(.x))) %>% 
     c("iso3","pais_nombre","nivel_gobierno",.)
   
   
  names(df_raw) <- cols
  
  clean_df <- df_raw %>% 
    drop_na(iso3) %>% 
    pivot_longer(!all_of(c("iso3","pais_nombre","nivel_gobierno")), 
                 names_to = 'anio', 
                 values_to = "valor",
                 names_transform = as.integer) %>% 
    select(-pais_nombre) %>% 
    left_join(geo_front, join_by(iso3))
  
  return(list(title = str_title, data = clean_df))
  
}



sheet_name <- "exp"
filas_columnas <- 2
skip <- 2


result <- cleaning_func(sheet_name = sheet_name, 
                        filas_columnas = filas_columnas,
                        skip = skip)


df_clean <- result$data 

title <- result$title

clean_filename <- glue::glue("{nombre_archivo_raw}_{sheet_name}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("{titulo.raw} - {title}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

id_fuente_clean <- 270
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))  

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("iso3", "nivel_gobierno", "anio")
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)