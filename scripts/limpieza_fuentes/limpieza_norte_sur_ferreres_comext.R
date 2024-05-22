### LIMPIEZA ####


# descargar fuente raw desde drive


descargar_fuente("R43C0")


# carga

data <- readxl::read_excel(path = get_temp_path("R43C0"),
                           sheet = "Comercio Exterior", skip = 1) %>% 
  janitor::clean_names() %>% 
  dplyr::select(anio = 1, cantidades_de_exportacion) %>% 
  tidyr::drop_na() %>% 
  dplyr::mutate_all(as.numeric)


# guardar como csv
write_csv_fundar(data,
                 file = glue::glue("{tempdir()}/cantidades_exportacion_Ferreres.csv"))  

# carga en sheet fuentes clean
# agregar_fuente_clean(id_fuente_raw = 43, 
#                      path_clean = glue::glue("cantidades_exportacion_Ferreres.csv"),
#                      nombre = "Cantidades de Exportacion ARG (2004 = 100)",
#                      script = "limpieza_norte_sur_ferreres_comext.R", descripcion_clean = 'Datos competos y seleccion de variable')

actualizar_fuente_clean(id_fuente_clean = 22)


