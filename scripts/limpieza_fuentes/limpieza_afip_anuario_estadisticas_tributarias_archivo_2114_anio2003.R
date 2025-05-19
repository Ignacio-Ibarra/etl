#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 413
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

source("scripts/utils/afip_anuario_estadistico_scraper.R")


zip_path <- argendataR::get_raw_path(fuente_raw)

coincidencia_cabinet<- unzip(zip_path, list = TRUE) %>% 
  dplyr::filter(grepl("\\.cab$", tolower(Name))) %>% 
  pull(Name)


ruta_cabinet <- unzip(zip_path, files = coincidencia_cabinet, exdir = tempdir(), junkpaths = TRUE)


search_file = "2.1.1.4_2.xls"

coincidencia_archivo <- cabextract_wrapper(ruta_cabinet, list_files = T) %>% 
  dplyr::filter(name == search_file) %>% 
  pull(name)


ruta_xls <- cabextract_wrapper(ruta_cabinet, directory = tempdir(), filter = coincidencia_archivo)


cleaning_func <- function(ruta, name_cols, sheet_name, cell_range){
  
  str_title <- readxl::read_excel(ruta, sheet = sheet_name, col_names = F) %>% 
    slice(1:3) %>% 
    select(1) %>% 
    pull() %>%
    stats::na.omit() %>%
    paste0(., collapse = ". ")
  
  unidad_medida_str <- readxl::read_excel(ruta, sheet = sheet_name, col_names = F) %>% 
    slice(5) %>% 
    select(1) %>% 
    pull() %>% 
    str_remove_all(., "\\(|\\)|\\\r|\\\n") %>% 
    str_replace_all(., "  ", " ")
  
  df_raw <- readxl::read_excel(ruta, 
                               range = cell_range,
                               col_names = F,
                               sheet = sheet_name) 
  
  names(df_raw) <- name_cols
  
  clean_df <- df_raw %>% pivot_longer(!all_of("actividad_economica"), names_to = c("destino_venta", "detalle"), names_sep = "#", values_to = "valor") %>% 
    mutate(
      cod_act = if_else(str_detect(actividad_economica, "^\\w -"), 
                        str_extract(actividad_economica, "^\\w"), 
                        str_extract(actividad_economica, "^\\d{3}")),
      
      actividad_economica = if_else(str_detect(actividad_economica, "^\\w -"), 
                                    str_remove(actividad_economica, "^\\w - "), 
                                    str_remove(actividad_economica, "^\\d{3}\\. ")),
      
      nivel_agregacion = case_when(
        
        cod_act %in% LETTERS[1:19] ~ "letra",
        is.na(cod_act) ~ NA_character_,
        TRUE ~ "3 dígitos"
      ),
      unidad_medida = unidad_medida_str
    )
  
  return(list(title =str_title, data = clean_df))
  
}



name_cols <- c("actividad_economica", 
               "Total#Presentaciones", 
               "Total#Ventas totales", 
               "Mercado interno#Ventas totales",
               "Mercado interno#Ventas gravadas",
               "Mercado interno#No gravadas y exentas",
               "Exportaciones#Ventas totales")

sheet_name <- readxl::excel_sheets(ruta_xls)
cell_range <- "C14:I191"

result <- cleaning_func(ruta = ruta_xls, 
                        name_cols = name_cols, 
                        sheet_name = sheet_name, 
                        cell_range = cell_range)


df_clean <- result$data 

title <- result$title

clean_filename <- glue::glue("{nombre_archivo_raw}_{tools::file_path_sans_ext(search_file) %>% janitor::make_clean_names()}_hoja{sheet_name %>% janitor::make_clean_names()}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("{titulo.raw} - {title}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name,
#                      descripcion = "El dataset contiene por letra y a tres dígitos de desagregacion datos sobre las presentaciones, ventas y exportaciones por actividad económica")

id_fuente_clean <- 264
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))  

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("cod_act", "destino_venta", "detalle")
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
