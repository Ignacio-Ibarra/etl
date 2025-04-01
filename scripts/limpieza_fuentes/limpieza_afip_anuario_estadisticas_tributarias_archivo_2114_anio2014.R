#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 258
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()



cleaning_func <- function(name_cols, sheet_name, cell_range){
  
  df_raw <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), 
                               range = cell_range,
                               col_names = F,
                               sheet = sheet_name)
  
  names(df_raw) <- name_cols
  
  df_clean <- df_raw %>% pivot_longer(!all_of("actividad_economica"), 
                                      names_to = c("destino_venta", "detalle"), 
                                      names_sep = "#", 
                                      values_to = "valor") %>% 
    mutate(
      cod_act = if_else(
        str_detect(actividad_economica, "^\\w -"),
        str_extract(actividad_economica, "^\\w"),
        str_extract(actividad_economica, "^\\d{3}")
      ),
      
      actividad_economica = if_else(str_detect(actividad_economica, "^\\w -"), 
                                    str_remove(actividad_economica, "^\\w - "), 
                                    str_remove(actividad_economica, "^\\d{3}\\.") %>% trimws(., which="both")),
      
      nivel_agregacion = case_when(
        
        cod_act %in% LETTERS[1:19] ~ "letra",
        is.na(cod_act) ~ NA_character_,
        TRUE ~ "3 dígitos"
      ),
      valor = as.numeric(str_replace_all(valor, "\\.",""))
    )
  
  return(df_clean)
  
}


name_cols <- c("actividad_economica", 
               "Total#Presentaciones", 
               "Total#Ventas totales", 
               "Mercado interno#Ventas totales",
               "Mercado interno#Ventas gravadas",
               "Mercado interno#No gravadas y exentas",
               "Exportaciones#Ventas totales")


sheet_name <- "Sheet 1"
cell_range <- "C12:I254"

df_clean <- cleaning_func(name_cols = name_cols, sheet_name = sheet_name, cell_range = cell_range)

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("{titulo.raw}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name,
#                      descripcion = "El dataset contiene por letra y a tres dígitos de desagregacion datos sobre las presentaciones, ventas y exportaciones por actividad económica")

id_fuente_clean <- 127
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) %>% 
  mutate(valor = as.numeric(str_replace_all(valor, "\\.","")))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("cod_act", "destino_venta", "detalle")
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
