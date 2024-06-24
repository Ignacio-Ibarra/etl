descargar_fuente(codigo = "R120C0")

archivo <- "ipc_rubros_1947_2018_fyns.csv"

ipc_rubros_fyns <- readxl::read_excel(get_temp_path("R120C0"), sheet = "IPC rubros", skip = 1)


ipc_rubros_fyns <- ipc_rubros_fyns[8:226,]

ipc_rubros_fyns <- ipc_rubros_fyns %>% 
  select(where(function(x) any(!is.na(x))))

ipc_rubros_fyns <- ipc_rubros_fyns %>% 
  janitor::clean_names()

ipc_rubros_fyns <- ipc_rubros_fyns %>% 
  filter(!if_all(-ano, is.na))

ipc_rubros_fyns <- ipc_rubros_fyns %>% 
  rename(anio = ano
  )


ipc_rubros_fyns <- ipc_rubros_fyns %>% 
  mutate(across(everything(), as.numeric))

write_csv_fundar(ipc_rubros_fyns,
                 file = glue::glue("{tempdir()}/{archivo}"))

agregar_fuente_clean(
  id_fuente_raw = 120,
  path_clean = archivo,
  nombre = "Indice de precios al consumidor por rubro 1947 a 2018. Promedio anual 2015=100",
  script = "limpieza_ipc_rubros_fnys.R",
  descripcion = "El IPC se presenta calculado como promedio anual del nivel de precios. La base = 100 es el año 2015."
)

actualizar_fuente_clean(id_fuente_clean = 49)