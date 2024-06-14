descargar_fuente("R128C0")

archivo <- "ipc_gba_1948_2008.csv"

ipc_gba_1948_2008 <- readxl::read_excel(get_temp_path("R128C0"), sheet = 2, skip = 3)


ipc_gba_1948_2008 <- ipc_gba_1948_2008 %>% 
  janitor::clean_names()

ipc_gba_1948_2008 <-  ipc_gba_1948_2008 %>% 
  rename(anio = ano)

ipc_gba_1948_2008 %>% 
  write_csv_fundar(glue::glue("{tempdir()}/{archivo}"))


agregar_fuente_clean(id_fuente_raw = 128, path_clean = archivo,
                     nombre = "Índice de Precios al Consumidor (IPC) GBA desde 1943 a 2013",
                     script = "scripts/limpieza_fuentes/limpieza_ipc_gba_1943_2013_indec.R", 
                     descripcion = "Serie historica -  (empalme de las series base 1943, 1960, 1974, 1988 y 1999 con la serie base abril 2008=100), nivel general y capítulos de la canasta")


actualizar_fuente_clean(id_fuente_clean = 53)
