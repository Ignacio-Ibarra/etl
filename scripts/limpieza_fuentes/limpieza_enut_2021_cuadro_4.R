# Codigo de limpieza de datos de Encuesta Nacional de Uso del Tiempo 2021 Cuadro 4

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()


id_fuente <- 93
fuente_raw1 <- sprintf("R%sC0",id_fuente)

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw1) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

descargar_fuente_raw(id_fuente = id_fuente, tempdir())

sheet <- "Cuadro 4"
cell_range = "A8:D31"

# Funciona cuando las columnas activas del excel no son más que de la A a la Z
get_column_names =function(cell_range_str){
  column_names <- str_split_1(cell_range, "")
  id1 <- which(LETTERS == column_names[1])
  id2 <- which(LETTERS == column_names[4])
  return(LETTERS[id1:id2])
}

norm_string <- function(string){
  string <- str_to_lower(string)
  string <- paste0(string, collapse = "_")
  return(string)
  
}


col_names <- get_column_names(cell_range_str = cell_range)


enut_df <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1), 
                              sheet = sheet, 
                              range = cell_range, 
                              col_names = col_names,
                              col_types = "text")

enut_df <- enut_df[(seq(1, nrow(enut_df), by = 2)), ]

enut_df_clean <- enut_df %>% 
  mutate(sexo = case_when( 
    A %in% c("Total", "Mujeres", "Varones") ~ A
  ),
  grupo_edad = case_when(
    A %in% c("Total", "Mujeres", "Varones") ~ "Total",
    TRUE ~ A
  )
  ) %>% 
  fill(sexo) %>% 
  select(sexo, grupo_edad, `Trabajo total` = B, `En la ocupación` = C, `No remunerado` = D) %>% 
  type_convert(. ) %>% 
  pivot_longer(., !any_of(c("sexo", "grupo_edad")), names_to = "tipo_trabajo", values_to = "proporcion_dia") %>% 
  mutate(minutos_dia = proporcion_dia *24*60)

norm_sheet <- str_to_lower(sheet) %>% str_replace(., " ", "_")

clean_filename <- glue::glue("{norm_sheet}_{nombre_archivo_raw}_CLEAN.csv")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

enut_df_clean %>% write_csv_fundar(., file = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      dir = tempdir(),
#                      nombre = sprintf("Encuesta Nacional de Uso del Tiempo 2021 (%s)",sheet),
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 18,
                        dir = tempdir())