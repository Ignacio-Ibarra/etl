descargar_fuente("R117C0")


# ipc gba abril 2016 a mayo 2017 base abril 2016
# Nivel general y capítulos

df <- readxl::read_xls(get_temp_path("R117C0"),
                 sheet = "IPC GBA Abril 2016-Mayo 2017", trim_ws = F, range = "A5:O17")


fechas <- colnames(df)[-1] %>% 
  as.numeric() %>% 
  as.Date(., origin = "1899-12-30")

columnas <- c("capitulo", as.character(fechas))

colnames(df) <- columnas

df <- df %>% 
  pivot_longer(-capitulo, names_to = "fecha", values_to = "indice") %>% 
  filter(!is.na(capitulo))

df <- df %>% 
  mutate(fecha = lubridate::as_date(fecha))

df %>% 
  write_csv_fundar(glue::glue("{tempdir()}/ipc_capitulos_gba_abril_2016_mayo_2017_indec.csv"))

agregar_fuente_clean(
  id_fuente_raw = 117,
  descripcion = "Solo incluye indice nivel general y capitulos principales",
  script = "scripts/limpieza_fuentes/limpieza_ipc_capitulos_gba_abril_2016_mayo_2017_indec.R",
  nombre = "Índices correspondientes a los meses de abril de 2016 a mayo de 2017 según capítulos de la canasta",
  path_clean = glue::glue("ipc_capitulos_gba_abril_2016_mayo_2017_indec.csv")
)
  

actualizar_fuente_clean(id_fuente_clean = 65)