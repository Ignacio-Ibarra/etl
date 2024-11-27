descargar_fuente("R127C0")

archivo <- "ipc_total_regiones_divisiones_indec.parquet"

ipc_indec <- readr::read_csv(get_temp_path("R127C0"))

ipc_indec <- ipc_indec %>%
  janitor::clean_names()
# 
ipc_indec <- ipc_indec %>%
  mutate(descripcion = ifelse(is.na(descripcion), codigo, descripcion))
# 
ipc_indec <- ipc_indec %>%
  mutate(descripcion = case_when(
    descripcion == "B" ~ "Bienes",
    descripcion == "S" ~ "Servicios",
    T ~ descripcion
  ))
# 
ipc_indec <- ipc_indec %>%
  mutate(descripcion = str_to_sentence(descripcion))
# 
# 
ipc_indec <- ipc_indec %>%
  mutate(anio =  as.numeric(substr(periodo, 1, 4)),
         mes = as.numeric(substr(periodo, 5, 6)))
# 
ipc_indec <- ipc_indec %>%
  select(-c(periodo))
# 
# 
ipc_indec <- ipc_indec %>%
  select(anio, mes, region, codigo, clasificador, descripcion,  everything())


arrow::write_parquet(x = ipc_indec,
                     sink = glue::glue("{tempdir()}/{archivo}"))

print(glue::glue("{tempdir()}/{archivo}"))


# agregar_fuente_clean(id_fuente_raw = 127,script = "scripts/limpieza_fuentes/limpieza_ipc_csv_indec.R", 
#                      path_clean = archivo,
#                      nombre = "Indice de precios al consumidor (IPC) por regiones según divisiones, categorías, bienes y servicios",
#                      descripcion = "Los datos de esta tabla deberian coincidir con la sheet ipc de fuente raw 117 tambien. Se consume directo del link csv por comodidad"
#                      )

df_anterior <- arrow::read_parquet(get_clean_path(codigo = "R127C54"))


comparacion <- comparar_fuente_clean(ipc_indec,
                                     df_anterior %>% 
                                       mutate(across(c(anio, mes), as.numeric)),
                                     pk = c("anio", "mes", "region", "codigo"))



actualizar_fuente_clean(id_fuente_clean = 54,
                        df = ipc_indec, comparacion = comparacion)


