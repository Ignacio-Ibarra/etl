# Codigo de limpieza de datos de Participacion Laboral en los Censos (1869 - 2022)

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()


id_fuente <- 108
fuente_raw1 <- sprintf("R%sC0",id_fuente)

descargar_fuente_raw(id_fuente = id_fuente, tempdir())

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    dplyr::filter(codigo == fuente_raw1) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]




empalme_censos <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1), sheet="empalme")

#-- Procesamiento ----

empalme_censos <- empalme_censos %>% 
  select(-`participacion_total_10+`) %>% 
  pivot_longer(starts_with("partici"), names_to = "sexo", values_to = "tasa_participacion") %>% 
  mutate(sexo = case_when(
    sexo == "participacion_hombres_14+" ~ "Varones",
    sexo == "participacion_mujeres_14+" ~ "Mujeres",
    TRUE ~ "Ambos"
  ))


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

clean_filename <- "censos_ocupados_CLEAN.csv"

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

empalme_censos %>% write_csv_fundar(., file = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      directorio = tempdir(),
#                      nombre = "Participación Laboral según sexo, en los Censos (1869 - 2022)",
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 27,
                        dir = tempdir())