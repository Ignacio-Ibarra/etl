descargar_fuente("R127C0")

archivo <- "ipc_total_regiones_divisiones_indec.csv"

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
  mutate(anio =  substr(periodo, 1, 4),
         mes = substr(periodo, 5, 6))
# 
ipc_indec <- ipc_indec %>%
  select(-c(periodo))
# 
# 
ipc_indec <- ipc_indec %>%
  select(anio, mes, region, codigo, clasificador, descripcion,  everything())


write_csv_fundar(x = ipc_indec, file = glue::glue("{tempdir()}/{archivo}"))


# agregar_fuente_clean(id_fuente_raw = 127,script = "scripts/limpieza_fuentes/limpieza_ipc_csv_indec.R", 
#                      path_clean = archivo,
#                      nombre = "Indice de precios al consumidor (IPC) por regiones según divisiones, categorías, bienes y servicios",
#                      descripcion = "Los datos de esta tabla deberian coincidir con la sheet ipc de fuente raw 117 tambien. Se consume directo del link csv por comodidad"
#                      )

actualizar_fuente_clean(id_fuente_clean = 54)
