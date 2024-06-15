descargar_fuente("R156C0")

archivo <- "ipc_rubros_san_luis.csv"

readxl::excel_sheets(get_temp_path("R156C0"))

df <- readxl::read_xlsx(get_temp_path("R156C0"), sheet = "Serie", skip = 3)

df <- df %>% 
  rename(fecha = Periodo) %>% 
  mutate(fecha = as.Date(as.numeric(fecha), origin = "1899-12-30")) %>% 
  filter(!if_all(everything(),is.na)) %>% 
  pivot_longer(-fecha, names_to = "rubro", values_to = "indice")


df %>% 
  write_csv_fundar(glue::glue("{tempdir()}/{archivo}"))


agregar_fuente_clean(id_fuente_raw = 156,
                     path_clean = archivo,script = "scripts/limpieza_fuentes/limpieza_ipc_rubros_sanluis.R", 
                     nombre = "Índice de Precios al Consumidor (IPC) San Luis. Nivel General y Capítulos de la Canasta. Desde Octubre de 2005 en adelante")
