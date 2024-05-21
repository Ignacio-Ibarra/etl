### LIMPIEZA ####


# descargar fuente raw desde drive


descargar_fuente("R44C0")


# carga

data <- readxl::read_excel(path = get_temp_path("R44C0"), skip = 3) %>% 
  janitor::clean_names() %>% 
  dplyr::select(anio = 1, cantidades_de_exportacion = 4) %>% 
  dplyr::mutate(anio = stringr::str_remove(anio, '\\*')) %>% 
  tidyr::drop_na() %>% 
  dplyr::mutate_all(as.numeric)


# guardar como csv
write_csv_fundar(data,
                 file = glue::glue("{tempdir()}/cantidades_exportacion_INDEC.csv"))  

# carga en sheet fuentes clean
# agregar_fuente_clean(id_fuente_raw = 44, 
#                      path_clean = glue::glue("cantidades_exportacion_INDEC.csv"),
#                      nombre = "Cantidades de Exportacion ARG (2004 = 100)",
#                      script = "limpieza_indec_comex.R", descripcion_clean = 'Datos competos y seleccion de variable' )
#
actualizar_fuente_clean(id_fuente_clean = 23)




  