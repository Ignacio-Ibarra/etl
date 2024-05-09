# Codigo de limpieza de datos de EPH Total Urbano

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

id_fuente <- 49
fuente1 <- sprintf("R%sC0",id_fuente)

descargar_fuente_raw(id_fuente = 49, tempdir())

ephtu_raw <- readr::read_csv(argendataR::get_temp_path(fuente1))

names(ephtu_raw) <- tolower(names(ephtu_raw))

path_clean <- "data/_FUENTES/clean/eph_total_urbano_individual_2016_2023_CLEAN.csv"

ephtu_raw %>% write_csv_fundar(., file = glue::glue(path_clean))

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

agregar_fuente_clean(id_fuente_raw = 49, 
                     path_clean = glue::glue("{getwd()}/{path_clean}"),
                     nombre = "Encuesta Permanente de Hogares Total Urbano, Individual (2016 - 2023)",
                     script = code_name)

# actualizar_fuente_clean(id_fuente_clean = )