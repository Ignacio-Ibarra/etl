#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 347
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

# Función para extraer datos por país y variable
extraer_dataframe <- function(country, variable_name=NULL) {
  country_name <- country$name_es
  
  # Usamos map_df para recorrer las filas
  map_df(country$rows, function(row) {
    variable_name_value <- row$name_es
    
    # Verificar si row$values no es NULL
    if (!is.null(row$values)) {
      # Crear un tibble básico con los valores siempre presentes
      df <- tibble(
        pais = country_name,
        anio = as.integer(names(row$values)),
        valor = as.numeric(unlist(row$values))
      )
      
      # Si variable_name no es NULL, agregar la columna correspondiente
      if (!is.null(variable_name)) {
        df <- df %>%
          mutate(!!sym(variable_name) := variable_name_value)
      }
      
      return(df)
    }
  })
}


col_cross <- "disciplina"

df_clean <- argendataR::get_raw_path(fuente_raw) %>% 
  jsonlite::read_json(.) %>%
  pluck("indicators", 1, "countries") %>%
  map(~extraer_dataframe(.x, col_cross)) %>% 
  bind_rows()


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 222
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('anio','pais',col_cross)
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)