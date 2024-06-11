descargar_fuente("R124C0")

archivo <- "ipc_san_luis.csv"

ipc_san_luis <- readxl::read_excel(get_temp_path("R124C0"), skip = 2)

ipc_san_luis <- ipc_san_luis %>% 
  janitor::clean_names()

ipc_san_luis <- ipc_san_luis %>% 
  rename(anio =ano, var_mes_mes_porcentual = variacion_porcentual_respecto_del_mes_anterior)

ipc_san_luis <- ipc_san_luis %>% 
  fill(anio, .direction = "down")


ipc_san_luis <- ipc_san_luis %>% 
  mutate(mes = month(readr::parse_date(glue::glue("2024-{mes}-01"),
                                 format = "%Y-%B-%d", locale = locale("es")))
         )

ipc_san_luis %>% 
  write_csv_fundar(file = glue::glue("{tempdir()}/{archivo}"))

# agregar_fuente_clean(
#   id_fuente_raw = 124,
#   path_clean = archivo,
#   nombre = "Indice de precios al consumidor (IPC) - San Luis",
#   script = "limpieza_ipc_san_luis.R"
# )

actualizar_fuente_clean(id_fuente_clean = 50)
